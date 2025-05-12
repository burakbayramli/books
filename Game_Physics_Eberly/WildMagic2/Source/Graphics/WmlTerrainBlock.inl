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
inline unsigned char TerrainBlock::GetX () const
{
    return m_ucX;
}
//----------------------------------------------------------------------------
inline unsigned char TerrainBlock::GetY () const
{
    return m_ucY;
}
//----------------------------------------------------------------------------
inline unsigned char TerrainBlock::GetStride () const
{
    return m_ucStride;
}
//----------------------------------------------------------------------------
inline void TerrainBlock::SetEven (bool bSet)
{
    if ( bSet )
        m_ucFlags |= EVEN_PARITY;
    else
        m_ucFlags &= ~EVEN_PARITY;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::GetEven () const
{
    return (m_ucFlags & EVEN_PARITY) != 0;
}
//----------------------------------------------------------------------------
inline void TerrainBlock::SetProcessed (bool bSet)
{
    if ( bSet )
        m_ucFlags |= PROCESSED;
    else
        m_ucFlags &= ~PROCESSED;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::GetProcessed () const
{
    return (m_ucFlags & PROCESSED) != 0;
}
//----------------------------------------------------------------------------
inline void TerrainBlock::SetVisible (bool bSet)
{
    if ( bSet )
        m_ucFlags |= VISIBLE;
    else
        m_ucFlags &= ~VISIBLE;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::GetVisible () const
{
    return (m_ucFlags & VISIBLE) != 0;
}
//----------------------------------------------------------------------------
inline void TerrainBlock::SetVisibilityTested (bool bSet)
{
    if ( bSet )
        m_ucFlags |= VISIBILITY_TESTED;
    else
        m_ucFlags &= ~VISIBILITY_TESTED;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::GetVisibilityTested () const
{
    return (m_ucFlags & VISIBILITY_TESTED) != 0;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::BitsSet () const
{
    return (m_ucFlags & BITS_MASK) != 0;
}
//----------------------------------------------------------------------------
inline void TerrainBlock::ClearBits ()
{
    // retain parity bit, all others zeroed out
    m_ucFlags &= EVEN_PARITY;
}
//----------------------------------------------------------------------------
inline float TerrainBlock::GetDelta (int i) const
{
    assert( 0 <= i && i <= 4 );
    return m_fDelta[i];
}
//----------------------------------------------------------------------------
inline float TerrainBlock::GetDeltaMax () const
{
    return m_fDeltaMax;
}
//----------------------------------------------------------------------------
inline float TerrainBlock::GetDeltaL () const
{
    return m_fDeltaL;
}
//----------------------------------------------------------------------------
inline float TerrainBlock::GetDeltaH () const
{
    return m_fDeltaH;
}
//----------------------------------------------------------------------------
inline const Vector3f& TerrainBlock::GetMin () const
{
    return m_kMin;
}
//----------------------------------------------------------------------------
inline const Vector3f& TerrainBlock::GetMax () const
{
    return m_kMax;
}
//----------------------------------------------------------------------------
inline unsigned short TerrainBlock::GetParentIndex (unsigned short usChild)
{
    // p = (c-1)/4
    return (usChild - 1) >> 2;
}
//----------------------------------------------------------------------------
inline unsigned short TerrainBlock::GetChildIndex (unsigned short usParent,
    unsigned short usIndex)
{
    // c = 4*p+i
    return (usParent << 2) + usIndex;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::IsFirstChild (unsigned short usIndex)
{
    return usIndex > 0 && ((usIndex-1) % 4) == 0;
}
//----------------------------------------------------------------------------
inline bool TerrainBlock::IsSibling (unsigned short usIndex,
    unsigned short usTest)
{
    // assert:  usIndex > 0 && usTest > 0
    return (usIndex-1) >> 2 == (usTest-1) >> 2;
}
//----------------------------------------------------------------------------
