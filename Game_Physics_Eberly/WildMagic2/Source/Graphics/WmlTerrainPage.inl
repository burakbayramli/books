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
inline unsigned short TerrainPage::GetSize () const
{
    return m_usSize;
}
//----------------------------------------------------------------------------
inline const unsigned short* TerrainPage::GetHeights () const
{
    return m_ausHeight;
}
//----------------------------------------------------------------------------
inline const Vector2f& TerrainPage::GetOrigin () const
{
    return m_kOrigin;
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetMinElevation () const
{
    return m_fMinElevation;
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetMaxElevation () const
{
    return m_fMaxElevation;
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetSpacing () const
{
    return m_fSpacing;
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetPixelTolerance () const
{
    return m_fPixelTolerance;
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetX (unsigned char ucX) const
{
    return m_kOrigin.X() + m_fSpacing*float(ucX);
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetY (unsigned char ucY) const
{
    return m_kOrigin.Y() + m_fSpacing*float(ucY);
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetHeight (unsigned short usIndex) const
{
    return m_fMinElevation + m_fMultiplier*float(m_ausHeight[usIndex]);
}
//----------------------------------------------------------------------------
inline float TerrainPage::GetTexture (unsigned char ucIndex) const
{
    return m_fTextureSpacing*float(ucIndex);
}
//----------------------------------------------------------------------------
