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
inline int MTTriangle::GetLabel () const
{
    return m_iLabel;
}
//----------------------------------------------------------------------------
inline int& MTTriangle::Label ()
{
    return m_iLabel;
}
//----------------------------------------------------------------------------
inline int MTTriangle::GetVertex (int i) const
{
    return m_aiVertex[i];
}
//----------------------------------------------------------------------------
inline int& MTTriangle::Vertex (int i)
{
    return m_aiVertex[i];
}
//----------------------------------------------------------------------------
inline int MTTriangle::GetEdge (int i) const
{
    return m_aiEdge[i];
}
//----------------------------------------------------------------------------
inline int& MTTriangle::Edge (int i)
{
    return m_aiEdge[i];
}
//----------------------------------------------------------------------------
inline int MTTriangle::GetAdjacent (int i) const
{
    return m_aiAdjacent[i];
}
//----------------------------------------------------------------------------
inline int& MTTriangle::Adjacent (int i)
{
    return m_aiAdjacent[i];
}
//----------------------------------------------------------------------------
