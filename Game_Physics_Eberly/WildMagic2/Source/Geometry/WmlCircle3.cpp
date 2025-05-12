// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCircle3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Circle3<Real>::Circle3 ()
    :
    m_kU(Vector3<Real>::ZERO),
    m_kV(Vector3<Real>::ZERO),
    m_kN(Vector3<Real>::ZERO),
    m_kCenter(Vector3<Real>::ZERO)
{
    m_fRadius = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Circle3<Real>::U ()
{
    return m_kU;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Circle3<Real>::U () const
{
    return m_kU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Circle3<Real>::V ()
{
    return m_kV;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Circle3<Real>::V () const
{
    return m_kV;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Circle3<Real>::N ()
{
    return m_kN;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Circle3<Real>::N () const
{
    return m_kN;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Circle3<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Circle3<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Circle3<Real>::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Circle3<Real>::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Circle3<float>;
template class WML_ITEM Circle3<double>;
}
//----------------------------------------------------------------------------
