// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlVector4.h"
namespace Wml
{
#include "WmlVector.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 ()
{
    // the vector is uninitialized
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 (Real fX, Real fY, Real fZ, Real fW)
{
    m_afTuple[0] = fX;
    m_afTuple[1] = fY;
    m_afTuple[2] = fZ;
    m_afTuple[3] = fW;
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 (const Vector4& rkV)
{
    memcpy(m_afTuple,rkV.m_afTuple,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>::Vector4 (const Vector<4,Real>& rkV)
{
    memcpy(m_afTuple,(const Real*)rkV,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>& Vector4<Real>::operator= (const Vector4& rkV)
{
    memcpy(m_afTuple,rkV.m_afTuple,4*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Vector4<Real>& Vector4<Real>::operator= (const Vector<4,Real>& rkV)
{
    memcpy(m_afTuple,(const Real*)rkV,4*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::X () const
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::X ()
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::Y () const
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::Y ()
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::Z () const
{
    return m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::Z ()
{
    return m_afTuple[2];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector4<Real>::W () const
{
    return m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector4<Real>::W ()
{
    return m_afTuple[3];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Vector<4,float>;

#ifdef WML_USING_VC6
template WML_ITEM Vector<4,float> operator* (float,
    const Vector<4,float>&);
#else
template WML_ITEM Vector<4,float> operator*<4,float> (float,
    const Vector<4,float>&);
#endif

template class WML_ITEM Vector4<float>;
const Vector4f Vector4f::ZERO(0.0f,0.0f,0.0f,0.0f);
const Vector4f Vector4f::UNIT_X(1.0f,0.0f,0.0f,0.0f);
const Vector4f Vector4f::UNIT_Y(0.0f,1.0f,0.0f,0.0f);
const Vector4f Vector4f::UNIT_Z(0.0f,0.0f,1.0f,0.0f);
const Vector4f Vector4f::UNIT_W(0.0f,0.0f,0.0f,1.0f);

template class WML_ITEM Vector<4,double>;

#ifdef WML_USING_VC6
template WML_ITEM Vector<4,double> operator* (double,
    const Vector<4,double>&);
#else
template WML_ITEM Vector<4,double> operator*<4,double> (double,
    const Vector<4,double>&);
#endif

template class WML_ITEM Vector4<double>;
const Vector4d Vector4d::ZERO(0.0,0.0,0.0,0.0);
const Vector4d Vector4d::UNIT_X(1.0,0.0,0.0,0.0);
const Vector4d Vector4d::UNIT_Y(0.0,1.0,0.0,0.0);
const Vector4d Vector4d::UNIT_Z(0.0,0.0,1.0,0.0);
const Vector4d Vector4d::UNIT_W(0.0,0.0,0.0,1.0);
}
//----------------------------------------------------------------------------
