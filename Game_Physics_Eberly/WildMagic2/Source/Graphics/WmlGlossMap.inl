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
inline NodePtr GlossMap::GetObjects () const
{
    return m_spkObjects;
}
//----------------------------------------------------------------------------
inline TextureStatePtr GlossMap::GetTextureState () const
{
    return m_spkTextureState;
}
//----------------------------------------------------------------------------
inline AlphaStatePtr GlossMap::GetAlphaState () const
{
    return m_spkAlphaState;
}
//----------------------------------------------------------------------------
inline int GlossMap::GetTextureUnit () const
{
    return m_iTexUnit;
}
//----------------------------------------------------------------------------
