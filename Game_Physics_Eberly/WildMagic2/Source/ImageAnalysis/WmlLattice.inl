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
inline int Lattice::GetDimensions () const
{
    return m_iDimensions;
}
//----------------------------------------------------------------------------
inline const int* Lattice::GetBounds () const
{
    return m_aiBound;
}
//----------------------------------------------------------------------------
inline int Lattice::GetBound (int i) const
{
    assert( i < m_iDimensions );
    return m_aiBound[i];
}
//----------------------------------------------------------------------------
inline int Lattice::GetQuantity () const
{
    return m_iQuantity;
}
//----------------------------------------------------------------------------
inline const int* Lattice::GetOffsets () const
{
    return m_aiOffset;
}
//----------------------------------------------------------------------------
inline int Lattice::GetOffset (int i) const
{
    assert( i < m_iDimensions );
    return m_aiOffset[i];
}
//----------------------------------------------------------------------------
