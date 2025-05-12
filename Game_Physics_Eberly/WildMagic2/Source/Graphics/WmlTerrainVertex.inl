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
inline void TerrainVertex::SetDependent (int i, TerrainVertex* pkDependent)
{
    assert( 0 <= i && i <= 1 );
    m_akDependent[i] = pkDependent;
}
//----------------------------------------------------------------------------
inline TerrainVertex* TerrainVertex::GetDependent (int i)
{
    assert( 0 <= i && i <= 1 );
    return m_akDependent[i];
}
//----------------------------------------------------------------------------
inline bool TerrainVertex::GetEnabled () const
{
    return m_bEnabled;
}
//----------------------------------------------------------------------------
