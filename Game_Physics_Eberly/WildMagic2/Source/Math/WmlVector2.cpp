// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlVector2.h"
namespace Wml
{
#include "WmlVector.inl"
}
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>::Vector2 ()
{
    // the vector is uninitialized
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>::Vector2 (Real fX, Real fY)
{
    m_afTuple[0] = fX;
    m_afTuple[1] = fY;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>::Vector2 (const Vector2& rkV)
{
    memcpy(m_afTuple,rkV.m_afTuple,2*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>::Vector2 (const Vector<2,Real>& rkV)
{
    memcpy(m_afTuple,(const Real*)rkV,2*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Vector2<Real>::operator= (const Vector2& rkV)
{
    memcpy(m_afTuple,rkV.m_afTuple,2*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>& Vector2<Real>::operator= (const Vector<2,Real>& rkV)
{
    memcpy(m_afTuple,(const Real*)rkV,2*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector2<Real>::X () const
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector2<Real>::X ()
{
    return m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector2<Real>::Y () const
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Vector2<Real>::Y ()
{
    return m_afTuple[1];
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Vector2<Real>::Perp () const
{
    return Vector2(m_afTuple[1],-m_afTuple[0]);
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Vector2<Real>::UnitPerp () const
{
    Vector2 kPerp(m_afTuple[1],-m_afTuple[0]);
    kPerp.Normalize();
    return kPerp;
}
//----------------------------------------------------------------------------
template <class Real>
Real Vector2<Real>::Kross (const Vector2& rkV) const
{
    return m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0];
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Vector2<Real>::Cross (const Vector2&) const
{
    return Vector2(m_afTuple[1],-m_afTuple[0]);
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Vector2<Real>::UnitCross (const Vector2&) const
{
    Vector2 kPerp(m_afTuple[1],-m_afTuple[0]);
    kPerp.Normalize();
    return kPerp;
}
//----------------------------------------------------------------------------
template <class Real>
void Vector2<Real>::Orthonormalize (Vector2& rkU, Vector2& rkV)
{
    // If the input vectors are v0 and v1, then the Gram-Schmidt
    // orthonormalization produces vectors u0 and u1 as follows,
    //
    //   u0 = v0/|v0|
    //   u1 = (v1-(u0*v1)u0)/|v1-(u0*v1)u0|
    //
    // where |A| indicates length of vector A and A*B indicates dot
    // product of vectors A and B.

    // compute u0
    rkU.Normalize();

    // compute u1
    Real fDot0 = rkU.Dot(rkV); 
    rkV -= fDot0*rkU;
    rkV.Normalize();
}
//----------------------------------------------------------------------------
template <class Real>
void Vector2<Real>::GenerateOrthonormalBasis (Vector2& rkU, Vector2& rkV,
    bool bUnitLengthV)
{
    if ( !bUnitLengthV )
        rkV.Normalize();

    rkU = rkV.Perp();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Vector<2,float>;

#ifdef WML_USING_VC6
template WML_ITEM Vector<2,float> operator* (float,
    const Vector<2,float>&);
#else
template WML_ITEM Vector<2,float> operator*<2,float> (float,
    const Vector<2,float>&);
#endif

template class WML_ITEM Vector2<float>;
const Vector2f Vector2f::ZERO(0.0f,0.0f);
const Vector2f Vector2f::UNIT_X(1.0f,0.0f);
const Vector2f Vector2f::UNIT_Y(0.0f,1.0f);

template class WML_ITEM Vector<2,double>;

#ifdef WML_USING_VC6
template WML_ITEM Vector<2,double> operator* (double,
    const Vector<2,double>&);
#else
template WML_ITEM Vector<2,double> operator*<2,double> (double,
    const Vector<2,double>&);
#endif

template class WML_ITEM Vector2<double>;
const Vector2d Vector2d::ZERO(0.0,0.0);
const Vector2d Vector2d::UNIT_X(1.0,0.0);
const Vector2d Vector2d::UNIT_Y(0.0,1.0);
}
//----------------------------------------------------------------------------
