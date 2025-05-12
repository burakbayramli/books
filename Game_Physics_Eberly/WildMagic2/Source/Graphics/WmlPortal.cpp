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

WmlImplementRTTI(Portal,Object);
WmlImplementStream(Portal);

//----------------------------------------------------------------------------
Portal::Portal (int iVertexQuantity, Vector3f* akModelVertex,
    ConvexRegion* pkAdjacentRegion, bool bOpen)
{
    assert( iVertexQuantity > 0 );

    m_iVertexQuantity = iVertexQuantity;
    m_akModelVertex = akModelVertex;
    m_akWorldVertex = new Vector3f[m_iVertexQuantity];
    m_pkAdjacentRegion = pkAdjacentRegion;
    m_bOpen = bOpen;
}
//----------------------------------------------------------------------------
Portal::Portal ()
{
    m_iVertexQuantity = 0;
    m_akModelVertex = NULL;
    m_akWorldVertex = NULL;
    m_pkAdjacentRegion = NULL;
    m_bOpen = false;
}
//----------------------------------------------------------------------------
Portal::~Portal ()
{
    delete[] m_akModelVertex;
    delete[] m_akWorldVertex;
}
//----------------------------------------------------------------------------
void Portal::UpdateWorldData (const Matrix3f& rkRot,
    const Vector3f& rkTrn, float fScale)
{
    for (int i = 0; i < m_iVertexQuantity; i++)
        m_akWorldVertex[i] = fScale*(rkRot*m_akModelVertex[i]) + rkTrn;
}
//----------------------------------------------------------------------------
void Portal::Draw (Renderer& rkRenderer)
{
    // only draw adjacent regions if portal is open
    if ( !m_bOpen )
        return;

    // only draw visible portals
    Camera* pkCamera = rkRenderer.GetCamera();
    if ( pkCamera->Culled(m_iVertexQuantity,m_akWorldVertex,true) )
        return;

    // push world planes formed by camera eye point and portal edges
    int i0 = 0, i1 = m_iVertexQuantity - 1;
    for (/**/; i0 < m_iVertexQuantity; i1 = i0++)
    {
        Plane3f rkPlane(pkCamera->GetLocation(),m_akWorldVertex[i0],
            m_akWorldVertex[i1]);

        pkCamera->PushPlane(rkPlane);
    }

    // draw the adjacent region and any non-culled objects in it
    m_pkAdjacentRegion->Draw(rkRenderer);

    // pop world planes
    for (i0 = 0; i0 < m_iVertexQuantity; i0++)
        pkCamera->PopPlane();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Portal::Factory (Stream& rkStream)
{
    Portal* pkObject = new Portal;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Portal::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iVertexQuantity);
    if ( m_iVertexQuantity )
    {
        m_akModelVertex = new Vector3f[m_iVertexQuantity];
        m_akWorldVertex = new Vector3f[m_iVertexQuantity];
    }

    for (int i = 0; i < m_iVertexQuantity; i++)
        StreamRead(rkStream,m_akModelVertex[i]);

    StreamReadBool(rkStream,m_bOpen);

    // link data
    ConvexRegion* pkAdjacentRegion;
    StreamRead(rkStream,pkAdjacentRegion);
    pkLink->Add(pkAdjacentRegion);
}
//----------------------------------------------------------------------------
void Portal::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_pkAdjacentRegion = (ConvexRegion*)rkStream.GetFromMap(pkLinkID);
}
//----------------------------------------------------------------------------
bool Portal::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) )
        return false;

    if ( m_pkAdjacentRegion )
        m_pkAdjacentRegion->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void Portal::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // Native data (world vertices are computed form model vertices in the
    // update call, no need to save)
    StreamWrite(rkStream,m_iVertexQuantity);
    for (int i = 0; i < m_iVertexQuantity; i++)
        StreamWrite(rkStream,m_akModelVertex[i]);

    StreamWriteBool(rkStream,m_bOpen);

    // link data
    StreamWrite(rkStream,m_pkAdjacentRegion);
}
//----------------------------------------------------------------------------
StringTree* Portal::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Object::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int Portal::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Portal) - sizeof(Object);
    int iDynaSize = m_iVertexQuantity*(2*sizeof(m_akModelVertex[0]));
    int iTotalSize = iBaseSize + iDynaSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Portal::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        sizeof(m_iVertexQuantity) +
        m_iVertexQuantity*sizeof(m_akModelVertex[0]) +
        StreamBytesBool(m_bOpen) +
        sizeof(m_pkAdjacentRegion);
}
//----------------------------------------------------------------------------
