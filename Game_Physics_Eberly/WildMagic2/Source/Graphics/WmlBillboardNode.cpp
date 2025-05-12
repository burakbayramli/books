// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBillboardNode.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(BillboardNode,Node);
WmlImplementStream(BillboardNode);

//----------------------------------------------------------------------------
BillboardNode::BillboardNode (int iQuantity, int iGrowBy)
    :
    Node(iQuantity,iGrowBy)
{
    m_fLastUpdateTime = -Mathf::MAX_REAL;
}
//----------------------------------------------------------------------------
void BillboardNode::RotateBillboard (const Camera* pkCamera)
{
    // This routine is a deferred call of UpdateGS.  The billboard orientation
    // is computed just before drawing, so it is only at this point that the
    // local transforms are computed and that the world transforms can be
    // computed from them.  The first part of this code is similar to
    // Spatial::UpdateWorldData in that the transforms are computed.  The
    // twist is that after the world transforms are known, the billboard must
    // be rotated in world space to face the camera.  The last part of the
    // code is the recursive update on the children.

    // Compute billboard's world transforms based on its parent's world
    // transform and its local transforms.
    Node* pkParent = GetParent();
    if ( pkParent )
    {
        m_kWorldRotate = pkParent->WorldRotate()*m_kRotate;
        m_kWorldTranslate = pkParent->WorldTranslate() +
            pkParent->WorldScale()*(pkParent->WorldRotate()*m_kTranslate);
        m_fWorldScale = pkParent->WorldScale()*m_fScale;
    }
    else
    {
        m_kWorldRotate = m_kRotate;
        m_kWorldTranslate = m_kTranslate;
        m_fWorldScale = m_fScale;
    }

    // Compute the additional rotation required for the billboard to face
    // the camera.  To do this, the camera must be inverse-transformed into
    // the model space of the billboard.
    Vector3f kDiff = pkCamera->GetLocation() - m_kWorldTranslate;
    float fInvWorldScale = 1.0f/m_fWorldScale;
    Vector3f kCLoc = fInvWorldScale*(kDiff*m_kWorldRotate);

    // squared length of the camera projection in the xz-plane
    float fSqrLength = kCLoc.X()*kCLoc.X() + kCLoc.Z()*kCLoc.Z();
    if ( fSqrLength < Mathf::EPSILON )
    {
        // camera on the billboard axis, rotation not defined
        return;
    }

    // unitize the projection
    float fInvLength = Mathf::InvSqrt(fSqrLength);
    kCLoc.X() *= fInvLength;
    kCLoc.Y() = 0.0f;
    kCLoc.Z() *= fInvLength;
    
    // compute the local orientation matrix for the billboard
    Matrix3f kOrient(kCLoc.Z(),0.0f,kCLoc.Z(),0.0f,1.0f,0.0f,-kCLoc.Z(),0.0f,
        kCLoc.Z());

    // The billboard must be oriented to face the camera before it is
    // transformed into the world.
    m_kWorldRotate = m_kWorldRotate*kOrient;

    // compute the update now that the billboard orientation is known
    for (int i = 0; i < (int)m_kChild.size(); i++)
    {
        Spatial* pkChild = m_kChild[i];
        if ( pkChild )
            pkChild->UpdateGS(m_fLastUpdateTime,false);
    }
}
//----------------------------------------------------------------------------
void BillboardNode::UpdateWorldData (float fAppTime)
{
    // Save the update time.  The update is deferred until Draw so that the
    // billboard is rotated only when it is visible.  At this time the
    // update time is needed to pass to the UpdateGS call.
    m_fLastUpdateTime = fAppTime;

    // The deferred update means that none of the children world data is
    // computed by a recursive traversal.  Just compute the world bound.
    UpdateWorldBound();
}
//----------------------------------------------------------------------------
void BillboardNode::Draw (Renderer& rkRenderer)
{
    RotateBillboard(rkRenderer.GetCamera());
    Node::Draw(rkRenderer);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* BillboardNode::Factory (Stream& rkStream)
{
    BillboardNode* pkObject = new BillboardNode;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void BillboardNode::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void BillboardNode::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool BillboardNode::Register (Stream& rkStream)
{
    return Node::Register(rkStream);
}
//----------------------------------------------------------------------------
void BillboardNode::Save (Stream& rkStream)
{
    Node::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* BillboardNode::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Node::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int BillboardNode::GetMemoryUsed () const
{
    int iBaseSize = sizeof(BillboardNode) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int BillboardNode::GetDiskUsed () const
{
    return Node::GetDiskUsed();
}
//----------------------------------------------------------------------------
