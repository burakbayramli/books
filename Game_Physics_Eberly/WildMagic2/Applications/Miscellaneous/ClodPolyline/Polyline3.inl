// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline int Polyline3::GetVertexQuantity () const
{
    return m_iVQuantity;
}
//----------------------------------------------------------------------------
inline const Vector3f* Polyline3::GetVertices () const
{
    return m_akVertex;
}
//----------------------------------------------------------------------------
inline bool Polyline3::GetClosed () const
{
    return m_bClosed;
}
//----------------------------------------------------------------------------
inline int Polyline3::GetEdgeQuantity () const
{
    return m_iEQuantity;
}
//----------------------------------------------------------------------------
inline const int* Polyline3::GetEdges () const
{
    return m_aiEdge;
}
//----------------------------------------------------------------------------
inline int Polyline3::GetMinLevelOfDetail () const
{
    return m_iVMin;
}
//----------------------------------------------------------------------------
inline int Polyline3::GetMaxLevelOfDetail () const
{
    return m_iVMax;
}
//----------------------------------------------------------------------------
inline int Polyline3::GetLevelOfDetail () const
{
    return m_iVQuantity;
}
//----------------------------------------------------------------------------
