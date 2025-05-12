// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPoint4.h"
namespace Wml
{
#include "WmlPoint.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Point4<Real>::Point4 ()
{
    // the point is uninitialized
}
//----------------------------------------------------------------------------
template <class Real>
Point4<Real>::Point4 (Real fX, Real fY, Real fZ, Real fW)
{
    m_afTuple[0] = fX;
    m_afTuple[1] = fY;
    m_afTuple[2] = fZ;
    m_afTuple[3] = fW;
}
//----------------------------------------------------------------------------
template <class Real>
Point4<Real>::Point4 (const Point4& rkP)
{
    memcpy(m_afTuple,rkP.m_afTuple,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Point4<Real>::Point4 (const Point<4,Real>& rkP)
{
    memcpy(m_afTuple,(const Real*)rkP,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Real Point4<Real>::X () const
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point4<Real>::X ()
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real Point4<Real>::Y () const
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point4<Real>::Y ()
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real Point4<Real>::Z () const
{
    return m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point4<Real>::Z ()
{
    return m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real Point4<Real>::W () const
{
    return m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Point4<Real>::W ()
{
    return m_afTuple[3];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Point<4,float>;
template class WML_ITEM Point4<float>;
const Point4f Point4f::ZERO(0.0f,0.0f,0.0f,0.0f);

template class WML_ITEM Point<4,double>;
template class WML_ITEM Point4<double>;
const Point4d Point4d::ZERO(0.0,0.0,0.0,0.0);
}
//----------------------------------------------------------------------------
