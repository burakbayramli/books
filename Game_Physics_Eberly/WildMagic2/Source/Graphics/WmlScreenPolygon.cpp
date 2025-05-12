// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRenderer.h"
#include "WmlScreenPolygon.h"
using namespace Wml;

WmlImplementRTTI(ScreenPolygon,Object);
WmlImplementStream(ScreenPolygon);

//----------------------------------------------------------------------------
ScreenPolygon::ScreenPolygon (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture0,
    Vector2f* akTexture1, Vector2f* akTexture2, Vector2f* akTexture3,
    Vector2f* akTextureBump)
{
    assert( iVertexQuantity >= 3 && akVertex );

    // represent the screen polygon as a triangle fan
    int iTriangleQuantity = iVertexQuantity - 2;
    int* aiConnect = new int[3*iTriangleQuantity];
    for (int i0 = 1, i1 = 2, j = 0; i0 <= iTriangleQuantity; i0++, i1++)
    {
        aiConnect[j++] = 0;
        aiConnect[j++] = i0;
        aiConnect[j++] = i1;
    }

    m_spkMesh = new TriMesh(iVertexQuantity,akVertex,akNormal,akColor,
        akTexture0,iTriangleQuantity,aiConnect,akTexture1,akTexture2,
        akTexture3,akTextureBump);
}
//----------------------------------------------------------------------------
ScreenPolygon::ScreenPolygon ()
{
    m_spkMesh = NULL;
}
//----------------------------------------------------------------------------
ScreenPolygon::~ScreenPolygon ()
{
    m_spkMesh = NULL;
}
//----------------------------------------------------------------------------
void ScreenPolygon::OnDraw (Renderer& rkRenderer)
{
    m_spkMesh->Geometry::Draw(rkRenderer);
    rkRenderer.Draw(*this);
}
//----------------------------------------------------------------------------
Object* ScreenPolygon::GetObjectByName (const char* acName)
{
    Object* pkFound = Object::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    pkFound = m_spkMesh->GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    return NULL;
}
//----------------------------------------------------------------------------
void ScreenPolygon::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Object::GetAllObjectsByName(acName,rkObjects);
    m_spkMesh->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ScreenPolygon::Factory (Stream& rkStream)
{
    ScreenPolygon* pkObject = new ScreenPolygon;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ScreenPolygon::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    if ( rkStream.GetVersion() >= Version(1,4) )
    {
        // link data
        TriMesh* pkMesh;
        StreamRead(rkStream,pkMesh);
        pkLink->Add(pkMesh);
        return;
    }

    // load the old data and create the triangle mesh now
    Vector3f* akVertex;
    Vector3f* akNormal = NULL;
    ColorRGB* akColor = NULL;
    Vector2f* akTexture = NULL;
    int iVQuantity;

    if ( rkStream.GetVersion() >= Version(1,3) )
    {
        // native data
        StreamRead(rkStream,iVQuantity);
        akVertex = new Vector3f[iVQuantity];
        StreamRead(rkStream,akVertex,iVQuantity);

        StreamRead(rkStream,akNormal);
        if ( akNormal )
        {
            akNormal = new Vector3f[iVQuantity];
            StreamRead(rkStream,akNormal,iVQuantity);
        }

        StreamRead(rkStream,akColor);
        if ( akColor )
        {
            akColor = new ColorRGB[iVQuantity];
            StreamRead(rkStream,akColor,iVQuantity);
        }

        StreamRead(rkStream,akTexture);
        if ( akTexture )
        {
            akTexture = new Vector2f[iVQuantity];
            StreamRead(rkStream,akTexture,iVQuantity);
        }

        // link data (alpha, light, material, texture, wireframe, z-buffer,
        // in that order)
        RenderState* pkState;
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
    }
    else
    {
        // native data
        StreamRead(rkStream,iVQuantity);
        akVertex = new Vector3f[iVQuantity];
        StreamRead(rkStream,akVertex,iVQuantity);

        StreamRead(rkStream,akTexture);
        if ( akTexture )
        {
            akTexture = new Vector2f[iVQuantity];
            StreamRead(rkStream,akTexture,iVQuantity);
        }

        // link data (texture, alpha, wireframe, z-buffer, in that order)
        RenderState* pkState;
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
        StreamRead(rkStream,pkState);
        pkLink->Add(pkState);
    }

    // represent the screen polygon as a triangle fan
    int iTQuantity = iVQuantity - 2;
    int* aiConnect = new int[3*iTQuantity];
    for (int i0 = 1, i1 = 2, j = 0; i0 <= iTQuantity; i0++, i1++)
    {
        aiConnect[j++] = 0;
        aiConnect[j++] = i0;
        aiConnect[j++] = i1;
    }

    m_spkMesh = new TriMesh(iVQuantity,akVertex,akNormal,akColor,akTexture,
        iTQuantity,aiConnect);
}
//----------------------------------------------------------------------------
void ScreenPolygon::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);

    Object* pkLinkID;
    RenderState* pkState;

    if ( rkStream.GetVersion() >= Version(1,4) )
    {
        pkLinkID = pkLink->GetLinkID();
        m_spkMesh = (TriMesh*)rkStream.GetFromMap(pkLinkID);
        return;
    }

    if ( rkStream.GetVersion() >= Version(1,3) )
    {
        // alpha
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // light
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // material
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // texture
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // wireframe
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // z-buffer
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);
    }
    else
    {
        // texture
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // alpha
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // wireframe
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);

        // z-buffer
        pkLinkID = pkLink->GetLinkID();
        pkState = (RenderState*)rkStream.GetFromMap(pkLinkID);
        if ( pkState )
            m_spkMesh->SetRenderState(pkState);
    }
}
//----------------------------------------------------------------------------
bool ScreenPolygon::Register (Stream& rkStream)
{
    if ( !Object::Register(rkStream) )
        return false;

    m_spkMesh->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void ScreenPolygon::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // link data
    StreamWrite(rkStream,m_spkMesh);
}
//----------------------------------------------------------------------------
StringTree* ScreenPolygon::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,Object::SaveStrings());
    pkTree->SetChild(1,m_spkMesh->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int ScreenPolygon::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ScreenPolygon) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ScreenPolygon::GetDiskUsed () const
{
    int iSize = Object::GetDiskUsed() + sizeof(m_spkMesh);
    return iSize;
}
//----------------------------------------------------------------------------
