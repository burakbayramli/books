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
#include "WmlPolypoint.h"
using namespace Wml;

WmlImplementRTTI(Polypoint,Geometry);
WmlImplementStream(Polypoint);

//----------------------------------------------------------------------------
Polypoint::Polypoint (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture)
    :
    Geometry(iVertexQuantity,akVertex,akNormal,akColor,akTexture)
{
    m_iActiveQuantity = iVertexQuantity;

    // support for drawing vertex arrays
    m_aiIndex = new int[m_iVertexQuantity];
    for (int i = 0; i < m_iVertexQuantity; i++)
        m_aiIndex[i] = i;
}
//----------------------------------------------------------------------------
Polypoint::Polypoint ()
{
    m_iActiveQuantity = 0;
    m_aiIndex = NULL;
}
//----------------------------------------------------------------------------
Polypoint::~Polypoint ()
{
    delete[] m_aiIndex;
}
//----------------------------------------------------------------------------
void Polypoint::UpdateModelNormals ()
{
    // Points have no geometric neighbor information that would allow
    // automatic construction of normals.
}
//----------------------------------------------------------------------------
void Polypoint::Draw (Renderer& rkRenderer)
{
    Geometry::Draw(rkRenderer);
    rkRenderer.Draw(*this);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Polypoint::Factory (Stream& rkStream)
{
    Polypoint* pkObject = new Polypoint;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Polypoint::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iActiveQuantity);

    // support for drawing vertex arrays
    m_aiIndex = new int[m_iVertexQuantity];
    for (int i = 0; i < m_iVertexQuantity; i++)
        m_aiIndex[i] = i;
}
//----------------------------------------------------------------------------
void Polypoint::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Polypoint::Register (Stream& rkStream)
{
    return Geometry::Register(rkStream);
}
//----------------------------------------------------------------------------
void Polypoint::Save (Stream& rkStream)
{
    Geometry::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iActiveQuantity);
}
//----------------------------------------------------------------------------
StringTree* Polypoint::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("active = ",m_iActiveQuantity));

    // children
    pkTree->SetChild(0,Geometry::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Polypoint::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Polypoint) - sizeof(Geometry);
    int iDynaSize = m_iVertexQuantity*sizeof(m_aiIndex[0]);
    int iTotalSize = iBaseSize + iDynaSize + Geometry::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Polypoint::GetDiskUsed () const
{
    return Geometry::GetDiskUsed() +
        sizeof(m_iActiveQuantity);
}
//----------------------------------------------------------------------------
