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
inline Light::~Light ()
{
}
//----------------------------------------------------------------------------
inline ColorRGB& Light::Ambient ()
{
    return m_kAmbient;
}
//----------------------------------------------------------------------------
inline ColorRGB& Light::Diffuse ()
{
    return m_kDiffuse;
}
//----------------------------------------------------------------------------
inline ColorRGB& Light::Specular ()
{
    return m_kSpecular;
}
//----------------------------------------------------------------------------
inline bool& Light::Attenuate ()
{
    return m_bAttenuate;
}
//----------------------------------------------------------------------------
inline float& Light::Constant ()
{
    return m_fConstant;
}
//----------------------------------------------------------------------------
inline float& Light::Linear ()
{
    return m_fLinear;
}
//----------------------------------------------------------------------------
inline float& Light::Quadratic ()
{
    return m_fQuadratic;
}
//----------------------------------------------------------------------------
inline bool& Light::On ()
{
    return m_bOn;
}
//----------------------------------------------------------------------------
inline float& Light::Intensity ()
{
    return m_fIntensity;
}
//----------------------------------------------------------------------------
inline bool& Light::SpecularAfterTexture ()
{
    return m_bSpecularAfterTexture;
}
//----------------------------------------------------------------------------
