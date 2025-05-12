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
inline ColorRGB& MaterialState::Emissive ()
{
    return m_kEmissive;
}
//----------------------------------------------------------------------------
inline ColorRGB& MaterialState::Ambient ()
{
    return m_kAmbient;
}
//----------------------------------------------------------------------------
inline ColorRGB& MaterialState::Diffuse ()
{
    return m_kDiffuse;
}
//----------------------------------------------------------------------------
inline ColorRGB& MaterialState::Specular ()
{
    return m_kSpecular;
}
//----------------------------------------------------------------------------
inline float& MaterialState::Shininess ()
{
    return m_fShininess;
}
//----------------------------------------------------------------------------
inline float& MaterialState::Alpha ()
{
    return m_fAlpha;
}
//----------------------------------------------------------------------------
