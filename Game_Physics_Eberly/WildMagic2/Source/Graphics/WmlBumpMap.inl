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
inline NodePtr BumpMap::GetObjects () const
{
    return m_spkObjects;
}
//----------------------------------------------------------------------------
inline TexturePtr BumpMap::GetNormalMap () const
{
    return m_spkNormalMap;
}
//----------------------------------------------------------------------------
inline TextureStatePtr BumpMap::GetTextureState () const
{
    return m_bModulate ? m_spkTextureStateModulated : m_spkTextureState;
}
//----------------------------------------------------------------------------
inline AlphaStatePtr BumpMap::GetAlphaState () const
{
    return m_spkAlphaState;
}
//----------------------------------------------------------------------------
inline LightPtr BumpMap::GetLight () const
{
    return m_spkLight;
}
//----------------------------------------------------------------------------
inline bool BumpMap::GetModulate () const
{
    return m_bModulate;
}
//----------------------------------------------------------------------------
