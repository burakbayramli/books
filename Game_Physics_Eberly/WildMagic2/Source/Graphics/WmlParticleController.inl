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
inline float& ParticleController::SystemLinearSpeed ()
{
    return m_fSystemLinearSpeed;
}
//----------------------------------------------------------------------------
inline float& ParticleController::SystemAngularSpeed ()
{
    return m_fSystemAngularSpeed;
}
//----------------------------------------------------------------------------
inline Vector3f& ParticleController::SystemLinearAxis ()
{
    return m_kSystemLinearAxis;
}
//----------------------------------------------------------------------------
inline Vector3f& ParticleController::SystemAngularAxis ()
{
    return m_kSystemAngularAxis;
}
//----------------------------------------------------------------------------
inline float* ParticleController::PointLinearSpeed ()
{
    return m_afPointLinearSpeed;
}
//----------------------------------------------------------------------------
inline float* ParticleController::PointAngularSpeed ()
{
    return m_afPointAngularSpeed;
}
//----------------------------------------------------------------------------
inline Vector3f* ParticleController::PointLinearAxis ()
{
    return m_akPointLinearAxis;
}
//----------------------------------------------------------------------------
inline Vector3f* ParticleController::PointAngularAxis ()
{
    return m_akPointAngularAxis;
}
//----------------------------------------------------------------------------
inline float& ParticleController::SystemSizeChange ()
{
    return m_fSystemSizeChange;
}
//----------------------------------------------------------------------------
inline float* ParticleController::PointSizeChange ()
{
    return m_afPointSizeChange;
}
//----------------------------------------------------------------------------
