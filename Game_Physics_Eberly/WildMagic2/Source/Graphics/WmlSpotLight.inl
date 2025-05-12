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
inline Light::Type SpotLight::GetType () const
{
    return LT_SPOT;
}
//----------------------------------------------------------------------------
inline Vector3f& SpotLight::Direction ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
inline float SpotLight::GetAngle () const
{
    return m_fAngle;
}
//----------------------------------------------------------------------------
inline float& SpotLight::Exponent ()
{
    return m_fExponent;
}
//----------------------------------------------------------------------------
