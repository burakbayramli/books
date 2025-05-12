// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPoint2.h"
namespace Wml
{
#include "WmlPoint.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Point2<Real>::Point2 ()
{
    // the point is uninitialized
}
//----------------------------------------------------------------------------
template <class Real>
Point2<Real>::Point2 (Real fX, Real fY)
{
    m_afTuple[0] = fX;
    m_afTuple[1] = fY;
}
//----------------------------------------------------------------------------
template <class Real>
Point2<Real>::Point2 (const Point2& rkP)
{
    memcpy(m_afTuple,rkP.m_afTuple,2*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Point2<Real>::Point2 (const Point<2,Real>& rkP)
{
    memcpy(m_afTuple,(const Real*)rkP,2*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Real Point2<Real>::X () const
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point2<Real>::X ()
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real Point2<Real>::Y () const
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point2<Real>::Y ()
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Point<2,float>;
template class WML_ITEM Point2<float>;
const Point2f Point2f::ZERO(0.0f,0.0f);

template class WML_ITEM Point<2,double>;
template class WML_ITEM Point2<double>;
const Point2d Point2d::ZERO(0.0,0.0);
}
//----------------------------------------------------------------------------
