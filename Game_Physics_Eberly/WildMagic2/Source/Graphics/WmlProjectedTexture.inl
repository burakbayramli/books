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
inline NodePtr ProjectedTexture::GetObjects () const
{
    return m_spkObjects;
}
//----------------------------------------------------------------------------
inline CameraPtr ProjectedTexture::GetCamera () const
{
    return m_spkCamera;
}
//----------------------------------------------------------------------------
inline TexturePtr ProjectedTexture::GetTexture () const
{
    return m_spkTexture;
}
//----------------------------------------------------------------------------
inline TextureStatePtr ProjectedTexture::GetTextureState () const
{
    return m_spkTextureState;
}
//----------------------------------------------------------------------------
