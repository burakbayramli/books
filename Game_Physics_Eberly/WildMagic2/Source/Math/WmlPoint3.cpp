// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPoint3.h"
namespace Wml
{
#include "WmlPoint.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Point3<Real>::Point3 ()
{
    // the point is uninitialized
}
//----------------------------------------------------------------------------
template <class Real>
Point3<Real>::Point3 (Real fX, Real fY, Real fZ)
{
    m_afTuple[0] = fX;
    m_afTuple[1] = fY;
    m_afTuple[2] = fZ;
}
//----------------------------------------------------------------------------
template <class Real>
Point3<Real>::Point3 (const Point3& rkP)
{
    memcpy(m_afTuple,rkP.m_afTuple,3*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Point3<Real>::Point3 (const Point<3,Real>& rkP)
{
    memcpy(m_afTuple,(const Real*)rkP,3*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Real Point3<Real>::X () const
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point3<Real>::X ()
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real Point3<Real>::Y () const
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point3<Real>::Y ()
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real Point3<Real>::Z () const
{
    return m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point3<Real>::Z ()
{
    return m_afTuple[2];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Point<3,float>;
template class WML_ITEM Point3<float>;
const Point3f Point3f::ZERO(0.0f,0.0f,0.0f);

template class WML_ITEM Point<3,double>;
template class WML_ITEM Point3<double>;
const Point3d Point3d::ZERO(0.0,0.0,0.0);
}
//----------------------------------------------------------------------------
