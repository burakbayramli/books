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
inline Bound::Bound ()
    :
    m_kCenter(Vector3f::ZERO)
{
    m_fRadius = 0.0f;
}
//----------------------------------------------------------------------------
inline Bound& Bound::operator= (const Bound& rkBound)
{
    m_kCenter = rkBound.m_kCenter;
    m_fRadius = rkBound.m_fRadius;
    return *this;
}
//----------------------------------------------------------------------------
inline Vector3f& Bound::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
inline float& Bound::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
inline const Vector3f& Bound::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
inline const float& Bound::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
inline float& Bound::operator[] (int i) const
{
    return ((float*)this)[i];
}
//----------------------------------------------------------------------------
inline Bound::operator float* ()
{
    return (float*)this;
}
//----------------------------------------------------------------------------
