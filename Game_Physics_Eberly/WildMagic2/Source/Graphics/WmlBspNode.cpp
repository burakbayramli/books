// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBspNode.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(BspNode,Node);
WmlImplementStream(BspNode);

//----------------------------------------------------------------------------
BspNode::BspNode ()
    :
    Node(3,0)
{
    m_oCallback = NULL;
    m_pvData = NULL;
}
//----------------------------------------------------------------------------
SpatialPtr BspNode::AttachLeftChild (Spatial* pkChild)
{
    return SetChild(0,pkChild);
}
//----------------------------------------------------------------------------
SpatialPtr BspNode::AttachRightChild (Spatial* pkChild)
{
    return SetChild(2,pkChild);
}
//----------------------------------------------------------------------------
SpatialPtr BspNode::DetachLeftChild ()
{
    return DetachChildAt(0);
}
//----------------------------------------------------------------------------
SpatialPtr BspNode::DetachRightChild ()
{
    return DetachChildAt(2);
}
//----------------------------------------------------------------------------
SpatialPtr BspNode::GetLeftChild ()
{
    return GetChild(0);
}
//----------------------------------------------------------------------------
SpatialPtr BspNode::GetRightChild ()
{
    return GetChild(2);
}
//----------------------------------------------------------------------------
void BspNode::UpdateWorldData (float fAppTime)
{
    Node::UpdateWorldData(fAppTime);

    // Let X represent points in model space and Y = s*R*X+T represent
    // points in world space where s is the world scale, R is the world
    // rotation, and T is the world translation.  The inverse transform is
    // X = (1/s)*R^t*(Y-T).  The model plane is Dot(N0,X) = C0.  Replacing
    // the formula for X in it and applying some algebra leads to the world
    // plane Dot(N1,Y) = C1 where N1 = R*N0 and C1 = s*C0+Dot(N1,T).

    Vector3f kNormal = m_kWorldRotate*m_kModelPlane.GetNormal();
    float fConstant = m_fWorldScale*m_kModelPlane.GetConstant() +
        kNormal.Dot(m_kWorldTranslate);

    m_kWorldPlane.Set(kNormal,fConstant);
}
//----------------------------------------------------------------------------
void BspNode::Draw (Renderer& rkRenderer)
{
    // draw children in back-to-front order
    SpatialPtr spkLeft = GetLeftChild();
    SpatialPtr spkRight = GetRightChild();

    CameraPtr spkCamera = rkRenderer.GetCamera();
    float fSgnDist = m_kWorldPlane.DistanceTo(spkCamera->GetLocation());
    float fNdD = m_kWorldPlane.GetNormal().Dot(spkCamera->GetDirection());
    float fCosSqr = spkCamera->GetMaxCosSqrFrustumAngle();

    if ( fSgnDist > 0.0f )
    {
        if ( -fNdD >= fCosSqr )
        {
            if ( spkRight )
                spkRight->Draw(rkRenderer);

            if ( m_oCallback )
                m_oCallback();
        }

        if ( spkLeft )
            spkLeft->Draw(rkRenderer);
    }
    else if ( fSgnDist < 0.0f )
    {
        if ( fNdD >= fCosSqr )
        {
            if ( spkLeft )
                spkLeft->Draw(rkRenderer);

            if ( m_oCallback )
                m_oCallback();
        }

        if ( spkRight )
            spkRight->Draw(rkRenderer);
    }
    else if ( fNdD >= 0.0f )
    {
        if ( -fNdD >= fCosSqr )
        {
            if ( spkRight )
                spkRight->Draw(rkRenderer);

            if ( m_oCallback )
                m_oCallback();
        }

        if ( spkLeft )
            spkLeft->Draw(rkRenderer);
    }
    else
    {
        if ( fNdD >= fCosSqr )
        {
            if ( spkLeft )
                spkLeft->Draw(rkRenderer);

            if ( m_oCallback )
                m_oCallback();
        }

        if ( spkRight )
            spkRight->Draw(rkRenderer);
    }
}
//----------------------------------------------------------------------------
BspNode* BspNode::GetContainingNode (const Vector3f& rkPoint)
{
    SpatialPtr spkLeft = GetLeftChild();
    SpatialPtr spkRight = GetRightChild();
    BspNode* pkBspChild;

    if ( spkLeft || spkRight )
    {
        if ( m_kWorldPlane.WhichSide(rkPoint) == Plane3f::NEGATIVE_SIDE )
        {
            pkBspChild = WmlDynamicCast(BspNode,spkRight);
            if ( pkBspChild )
                return pkBspChild->GetContainingNode(rkPoint);
            else
                return NULL;
        }
        else
        {
            pkBspChild = WmlDynamicCast(BspNode,spkLeft);
            if ( pkBspChild )
                return pkBspChild->GetContainingNode(rkPoint);
            else
                return NULL;
        }
    }
    else
    {
        return this;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BspNode::Factory (Stream& rkStream)
{
    BspNode* pkObject = new BspNode;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BspNode::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kModelPlane);
}
//----------------------------------------------------------------------------
void BspNode::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BspNode::Register (Stream& rkStream)
{
    return Node::Register(rkStream);
}
//----------------------------------------------------------------------------
void BspNode::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kModelPlane);

    // world plane is computed from model plane in update, no need to save
}
//----------------------------------------------------------------------------
StringTree* BspNode::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Node::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int BspNode::GetMemoryUsed () const
{
    // Does not include memory used by extra data attached to node.
    int iBaseSize = sizeof(BspNode) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BspNode::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_kModelPlane);
}
//----------------------------------------------------------------------------
