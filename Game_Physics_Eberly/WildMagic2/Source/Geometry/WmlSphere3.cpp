// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlSphere3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Sphere3<Real>::Sphere3 ()
    :
    m_kCenter(Vector3<Real>::ZERO)
{
    m_fRadius = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& Sphere3<Real>::Center ()
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& Sphere3<Real>::Center () const
{
    return m_kCenter;
}
//----------------------------------------------------------------------------
template <class Real>
Real& Sphere3<Real>::Radius ()
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Sphere3<Real>::Radius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Sphere3<float>;
template class WML_ITEM Sphere3<double>;
}
//----------------------------------------------------------------------------
