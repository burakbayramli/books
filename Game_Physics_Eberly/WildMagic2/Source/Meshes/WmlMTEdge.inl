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
inline int MTEdge::GetLabel () const
{
    return m_iLabel;
}
//----------------------------------------------------------------------------
inline int& MTEdge::Label ()
{
    return m_iLabel;
}
//----------------------------------------------------------------------------
inline int MTEdge::GetVertex (int i) const
{
    return m_aiVertex[i];
}
//----------------------------------------------------------------------------
inline int& MTEdge::Vertex (int i)
{
    return m_aiVertex[i];
}
//----------------------------------------------------------------------------
inline int MTEdge::GetTriangle (int i) const
{
    return m_aiTriangle[i];
}
//----------------------------------------------------------------------------
inline int& MTEdge::Triangle (int i)
{
    return m_aiTriangle[i];
}
//----------------------------------------------------------------------------
