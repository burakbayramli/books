// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlConvexRegion.h"
#include "WmlPortal.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(ConvexRegion,BspNode);
WmlImplementStream(ConvexRegion);

//----------------------------------------------------------------------------
ConvexRegion::ConvexRegion (int iPortalQuantity, Portal** apkPortal,
    Spatial* pkRepresentation)
{
    m_iPortalQuantity = iPortalQuantity;
    m_apkPortal = apkPortal;
    m_bVisited = false;
    AttachRepresentation(pkRepresentation);
}
//----------------------------------------------------------------------------
ConvexRegion::ConvexRegion ()
{
    m_iPortalQuantity = 0;
    m_apkPortal = NULL;
    m_bVisited = false;
}
//----------------------------------------------------------------------------
ConvexRegion::~ConvexRegion ()
{
    for (int i = 0; i < m_iPortalQuantity; i++)
        delete m_apkPortal[i];

    delete[] m_apkPortal;
}
//----------------------------------------------------------------------------
SpatialPtr ConvexRegion::AttachRepresentation (Spatial* pkScene)
{
    return SetChild(1,pkScene);
}
//----------------------------------------------------------------------------
SpatialPtr ConvexRegion::DetachRepresentation ()
{
    return DetachChildAt(1);
}
//----------------------------------------------------------------------------
SpatialPtr ConvexRegion::GetRepresentation ()
{
    return GetChild(1);
}
//----------------------------------------------------------------------------
void ConvexRegion::UpdateWorldData (float fAppTime)
{
    // update the region walls and contained objects
    Node::UpdateWorldData(fAppTime);

    // update the portal geometry
    for (int i = 0; i < m_iPortalQuantity; i++)
    {
        m_apkPortal[i]->UpdateWorldData(m_kWorldRotate,m_kWorldTranslate,
            m_fWorldScale);
    }
}
//----------------------------------------------------------------------------
void ConvexRegion::Draw (Renderer& rkRenderer)
{
    if ( !m_bVisited )
    {
        m_bVisited = true;

        // draw anything visible through open portals
        for (int i = 0; i < m_iPortalQuantity; i++)
            m_apkPortal[i]->Draw(rkRenderer);

        // draw the region walls and contained objects
        Node::Draw(rkRenderer);

        m_bVisited = false;
    }
}
//----------------------------------------------------------------------------
Object* ConvexRegion::GetObjectByName (const char* acName)
{
    Object* pkFound = BspNode::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    for (int i = 0; i < m_iPortalQuantity; i++)
    {
        pkFound = m_apkPortal[i]->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void ConvexRegion::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    BspNode::GetAllObjectsByName(acName,rkObjects);

    for (int i = 0; i < m_iPortalQuantity; i++)
        m_apkPortal[i]->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ConvexRegion::Factory (Stream& rkStream)
{
    ConvexRegion* pkObject = new ConvexRegion;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ConvexRegion::Load (Stream& rkStream, Stream::Link* pkLink)
{
    BspNode::Load(rkStream,pkLink);

    // link data
    StreamRead(rkStream,m_iPortalQuantity);
    if ( m_iPortalQuantity > 0 )
    {
        m_apkPortal = new Portal*[m_iPortalQuantity];
        for (int i = 0; i < m_iPortalQuantity; i++)
        {
            Portal* pkPortal;
            StreamRead(rkStream,pkPortal);
            pkLink->Add(pkPortal);
        }
    }
}
//----------------------------------------------------------------------------
void ConvexRegion::Link (Stream& rkStream, Stream::Link* pkLink)
{
    BspNode::Link(rkStream,pkLink);

    for (int i = 0; i < m_iPortalQuantity; i++)
    {
        Object* pkLinkID = pkLink->GetLinkID();
        m_apkPortal[i] = (Portal*)rkStream.GetFromMap(pkLinkID);
    }
}
//----------------------------------------------------------------------------
bool ConvexRegion::Register (Stream& rkStream)
{
    if ( !BspNode::Register(rkStream) )
        return false;

    for (int i = 0; i < m_iPortalQuantity; i++)
        m_apkPortal[i]->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void ConvexRegion::Save (Stream& rkStream)
{
    BspNode::Save(rkStream);

    // link data
    StreamWrite(rkStream,m_iPortalQuantity);
    for (int i = 0; i < m_iPortalQuantity; i++)
        StreamWrite(rkStream,m_apkPortal[i]);
}
//----------------------------------------------------------------------------
StringTree* ConvexRegion::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,BspNode::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int ConvexRegion::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ConvexRegion) - sizeof(BspNode);
    int iDynaSize = m_iPortalQuantity*sizeof(m_apkPortal[0]);
    int iTotalSize = iBaseSize + iDynaSize + BspNode::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ConvexRegion::GetDiskUsed () const
{
    return BspNode::GetDiskUsed() +
        sizeof(m_iPortalQuantity) +
        m_iPortalQuantity*sizeof(m_apkPortal[0]);
}
//----------------------------------------------------------------------------
