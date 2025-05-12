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
inline float& PointController::SystemLinearSpeed ()
{
    return m_fSystemLinearSpeed;
}
//----------------------------------------------------------------------------
inline float& PointController::SystemAngularSpeed ()
{
    return m_fSystemAngularSpeed;
}
//----------------------------------------------------------------------------
inline Vector3f& PointController::SystemLinearAxis ()
{
    return m_kSystemLinearAxis;
}
//----------------------------------------------------------------------------
inline Vector3f& PointController::SystemAngularAxis ()
{
    return m_kSystemAngularAxis;
}
//----------------------------------------------------------------------------
inline float* PointController::PointLinearSpeed ()
{
    return m_afPointLinearSpeed;
}
//----------------------------------------------------------------------------
inline float* PointController::PointAngularSpeed ()
{
    return m_afPointAngularSpeed;
}
//----------------------------------------------------------------------------
inline Vector3f* PointController::PointLinearAxis ()
{
    return m_akPointLinearAxis;
}
//----------------------------------------------------------------------------
inline Vector3f* PointController::PointAngularAxis ()
{
    return m_akPointAngularAxis;
}
//----------------------------------------------------------------------------
