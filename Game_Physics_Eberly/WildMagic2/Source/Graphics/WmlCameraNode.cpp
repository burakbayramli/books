// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCameraNode.h"
using namespace Wml;

WmlImplementRTTI(CameraNode,Node);
WmlImplementStream(CameraNode);

//----------------------------------------------------------------------------
CameraNode::CameraNode (Camera* pkCamera, int iQuantity, int iGrowBy)
    :
    Node(iQuantity,iGrowBy),
    m_spkCamera(pkCamera)
{
}
//----------------------------------------------------------------------------
void CameraNode::UpdateWorldData (float fAppTime)
{
    Node::UpdateWorldData(fAppTime);

    if ( m_spkCamera && m_spkCamera->GetActive() )
        m_spkCamera->SetFrame(m_kWorldTranslate,m_kWorldRotate);
}
//----------------------------------------------------------------------------
Object* CameraNode::GetObjectByName (const char* acName)
{
    Object* pkFound = Node::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    if ( m_spkCamera )
    {
        pkFound = m_spkCamera->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void CameraNode::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Node::GetAllObjectsByName(acName,rkObjects);

    if ( m_spkCamera )
        m_spkCamera->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* CameraNode::Factory (Stream& rkStream)
{
    CameraNode* pkObject = new CameraNode;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void CameraNode::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // link data
    Camera* pkCamera;
    StreamRead(rkStream,pkCamera);
    pkLink->Add(pkCamera);
}
//----------------------------------------------------------------------------
void CameraNode::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkCamera = (Camera*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool CameraNode::Register (Stream& rkStream)
{
    if ( !Node::Register(rkStream) )
        return false;

    if ( m_spkCamera )
        m_spkCamera->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void CameraNode::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // link data
    StreamWrite(rkStream,m_spkCamera);
}
//----------------------------------------------------------------------------
StringTree* CameraNode::SaveStrings ()
{
    int iCQuantity = ( m_spkCamera ? 2 : 1 );
    StringTree* pkTree = new StringTree(1,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,Spatial::SaveStrings());

    if ( m_spkCamera )
        pkTree->SetChild(1,m_spkCamera->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int CameraNode::GetMemoryUsed () const
{
    int iBaseSize = sizeof(CameraNode) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int CameraNode::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_spkCamera);
}
//----------------------------------------------------------------------------
