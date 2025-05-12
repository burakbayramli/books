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
#include "WmlPolyline.h"
using namespace Wml;

WmlImplementRTTI(Polyline,Geometry);
WmlImplementStream(Polyline);

//----------------------------------------------------------------------------
Polyline::Polyline (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture, bool bClosed)
    :
    Geometry(iVertexQuantity,akVertex,akNormal,akColor,akTexture)
{
    assert( iVertexQuantity >= 2 );

    m_bClosed = bClosed;
    m_bContiguous = true;

    // support for drawing vertex arrays
    m_aiIndex = new int[iVertexQuantity];
    for (int i = 0; i < iVertexQuantity; i++)
        m_aiIndex[i] = i;

    m_iActiveQuantity = iVertexQuantity;
}
//----------------------------------------------------------------------------
Polyline::Polyline ()
{
    m_bClosed = false;
    m_bContiguous = true;
    m_aiIndex = NULL;
    m_iActiveQuantity = 0;
}
//----------------------------------------------------------------------------
Polyline::~Polyline ()
{
    delete[] m_aiIndex;
}
//----------------------------------------------------------------------------
void Polyline::Reconstruct (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture, bool bClosed)
{
    Geometry::Reconstruct(iVertexQuantity,akVertex,akNormal,akColor,
        akTexture);

    m_bClosed = bClosed;

    // support for drawing vertex arrays
    delete[] m_aiIndex;
    m_aiIndex = new int[iVertexQuantity];
    for (int i = 0; i < iVertexQuantity; i++)
        m_aiIndex[i] = i;
}
//----------------------------------------------------------------------------
void Polyline::GetSegment (int i, Vector3f& rkV0, Vector3f& rkV1) const
{
    if ( m_bClosed )
    {
        assert( i < m_iVertexQuantity );
        rkV0 = m_akVertex[i];
        if ( i != m_iVertexQuantity - 1 )
            rkV1 = m_akVertex[i+1];
        else
            rkV1 = m_akVertex[0];
    }
    else
    {
        assert( i < m_iVertexQuantity - 1 );
        rkV0 = m_akVertex[i];
        rkV1 = m_akVertex[i+1];
    }
}
//----------------------------------------------------------------------------
void Polyline::UpdateModelNormals ()
{
    // Polylines do not have enough geometric information to allow for an
    // intuitive definition of "vertex normal".  If an application really
    // needs vertex normals, it should set them through the Normals()
    // function and use whatever definition it needs for such vertex normals.
    // However, just to provide *some* automatic calculation, here is our
    // algorithm.
    //
    // If the two edges meeting at a vertex are not parallel, we use the
    // negative of the unit-length bisector of the two edges that lies in the
    // plane of the edges.
    //
    // The hope is that the two edges at a vertex are not parallel, for if
    // they were, you could just collapse the two edges into a single edge.
    // Worst case is that all segments are on the same line, so any choice
    // of normals at the vertices is, in effect, arbitrary.  Because this is
    // all a hack anyway, we just choose the zero vector at vertices that
    // share two parallel edges.
    //
    // For am open polyline, the normals at the end points are just copies
    // of the normals at the immediate neighbors of the end points.

    int iVQm1 = m_iVertexQuantity-1;
    Vector3f kE0, kE1;
    for (int i = 1; i < iVQm1; i++)
    {
        kE0 = m_akVertex[i-1] - m_akVertex[i];
        kE0.Normalize();
        kE1 = m_akVertex[i+1] - m_akVertex[i];
        kE1.Normalize();
        m_akNormal[i] = -(kE0 + kE1);
        m_akNormal[i].Normalize();
    }

    if ( m_bClosed )
    {
        kE0 = m_akVertex[iVQm1] - m_akVertex[0];
        kE0.Normalize();
        kE1 = m_akVertex[1] - m_akVertex[0];
        kE1.Normalize();
        m_akNormal[0] = -(kE0 + kE1);
        m_akNormal[0].Normalize();

        kE0 = -kE0;
        kE1 = m_akVertex[iVQm1-1] - m_akVertex[iVQm1];
        kE1.Normalize();
        m_akNormal[0] = -(kE0 + kE1);
        m_akNormal[0].Normalize();
    }
    else
    {
        m_akNormal[0] = m_akNormal[1];
        m_akNormal[iVQm1] = m_akNormal[iVQm1-1];
    }
}
//----------------------------------------------------------------------------
void Polyline::Draw (Renderer& rkRenderer)
{
    Geometry::Draw(rkRenderer);
    rkRenderer.Draw(*this);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Polyline::Factory (Stream& rkStream)
{
    Polyline* pkObject = new Polyline;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Polyline::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bClosed);

    // support for drawing vertex arrays
    m_aiIndex = new int[m_iVertexQuantity];
    for (int i = 0; i < m_iVertexQuantity; i++)
        m_aiIndex[i] = i;

    if ( rkStream.GetVersion() >= Version(1,6) )
        StreamRead(rkStream,m_iActiveQuantity);
    else
        m_iActiveQuantity = m_iVertexQuantity;

    if ( rkStream.GetVersion() >= Version(1,7) )
        StreamReadBool(rkStream,m_bContiguous);
    else
        m_bContiguous = true;
}
//----------------------------------------------------------------------------
void Polyline::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Polyline::Register (Stream& rkStream)
{
    return Geometry::Register(rkStream);
}
//----------------------------------------------------------------------------
void Polyline::Save (Stream& rkStream)
{
    Geometry::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bClosed);
    StreamWrite(rkStream,m_iActiveQuantity);
    StreamWrite(rkStream,m_bContiguous);
}
//----------------------------------------------------------------------------
StringTree* Polyline::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("closed =",m_bClosed));
    pkTree->SetString(2,MakeString("active quantity =",m_iActiveQuantity));
    pkTree->SetString(3,MakeString("contiguous =",m_bContiguous));

    // children
    pkTree->SetChild(0,Geometry::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Polyline::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Polyline) - sizeof(Geometry);
    int iDynaSize = m_iVertexQuantity*sizeof(m_aiIndex[0]);
    int iTotalSize = iBaseSize + iDynaSize + Geometry::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Polyline::GetDiskUsed () const
{
    return Geometry::GetDiskUsed() +
        StreamBytesBool(m_bClosed) +
        sizeof(m_iActiveQuantity) +
        StreamBytesBool(m_bContiguous);
}
//----------------------------------------------------------------------------
