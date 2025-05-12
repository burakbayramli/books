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
inline void Polyline::SetActiveQuantity (int iActiveQuantity)
{
    if ( 0 <= iActiveQuantity && iActiveQuantity <= m_iVertexQuantity )
        m_iActiveQuantity = iActiveQuantity;
    else
        m_iActiveQuantity = m_iVertexQuantity;
}
//----------------------------------------------------------------------------
inline int Polyline::GetActiveQuantity () const
{
    return m_iActiveQuantity;
}
//----------------------------------------------------------------------------
inline bool& Polyline::Closed () const
{
    return (bool&) m_bClosed;
}
//----------------------------------------------------------------------------
inline int Polyline::GetSegmentQuantity () const
{
    return m_bClosed ? m_iActiveQuantity : m_iActiveQuantity - 1;
}
//----------------------------------------------------------------------------
inline const int* Polyline::Indices () const
{
    return m_aiIndex;
}
//----------------------------------------------------------------------------
inline const bool Polyline::Contiguous () const
{
    return m_bContiguous;
}
//----------------------------------------------------------------------------
inline bool& Polyline::Contiguous ()
{
    return m_bContiguous;
}
//----------------------------------------------------------------------------
