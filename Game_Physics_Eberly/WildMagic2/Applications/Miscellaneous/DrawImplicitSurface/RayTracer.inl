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
inline Vector3f& RayTracer::Location ()
{
    return m_kLocation;
}
//----------------------------------------------------------------------------
inline Vector3f& RayTracer::Direction ()
{
    return m_kDirection;
}
//----------------------------------------------------------------------------
inline Vector3f& RayTracer::Up ()
{
    return m_kUp;
}
//----------------------------------------------------------------------------
inline Vector3f& RayTracer::Right ()
{
    return m_kRight;
}
//----------------------------------------------------------------------------
inline float& RayTracer::Near ()
{
    return m_fNear;
}
//----------------------------------------------------------------------------
inline float& RayTracer::Far ()
{
    return m_fFar;
}
//----------------------------------------------------------------------------
inline float& RayTracer::HalfWidth ()
{
    return m_fHalfWidth;
}
//----------------------------------------------------------------------------
inline float& RayTracer::HalfHeight ()
{
    return m_fHalfHeight;
}
//----------------------------------------------------------------------------
inline int RayTracer::GetWidth () const
{
    return m_iWidth;
}
//----------------------------------------------------------------------------
inline int RayTracer::GetHeight () const
{
    return m_iHeight;
}
//----------------------------------------------------------------------------
inline const float* RayTracer::GetImage () const
{
    return m_afImage;
}
//----------------------------------------------------------------------------
