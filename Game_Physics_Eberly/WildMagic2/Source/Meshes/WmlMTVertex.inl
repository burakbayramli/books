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
inline int MTVertex::GetLabel () const
{
    return m_iLabel;
}
//----------------------------------------------------------------------------
inline int MTVertex::GetEdgeQuantity () const
{
    return m_kESet.GetQuantity();
}
//----------------------------------------------------------------------------
inline int MTVertex::GetEdge (int i) const
{
    return m_kESet.Get(i);
}
//----------------------------------------------------------------------------
inline int MTVertex::GetTriangleQuantity () const
{
    return m_kTSet.GetQuantity();
}
//----------------------------------------------------------------------------
inline int MTVertex::GetTriangle (int i) const
{
    return m_kTSet.Get(i);
}
//----------------------------------------------------------------------------
