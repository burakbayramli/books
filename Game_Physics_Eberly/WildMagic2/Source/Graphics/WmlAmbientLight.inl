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
inline AmbientLight::AmbientLight ()
{
}
//----------------------------------------------------------------------------
inline Light::Type AmbientLight::GetType () const
{
    return LT_AMBIENT;
}
//----------------------------------------------------------------------------
inline void AmbientLight::ComputeDiffuse (const Matrix3f&, const Vector3f&,
    float, const Vector3f*, const Vector3f*, int, const bool*, ColorRGB*)
{
    // ambient lights have no diffuse component
}
//----------------------------------------------------------------------------
inline void AmbientLight::ComputeSpecular (const Matrix3f&, const Vector3f&,
    float, const Vector3f*, const Vector3f*, int, const bool*,
    const Vector3f&, ColorRGB*)
{
    // ambient lights have no specular component
}
//----------------------------------------------------------------------------
