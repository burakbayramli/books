// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPlane3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::Plane3 ()
{
    for (int i = 0; i < 4; i++)
        m_afTuple[i] = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::Plane3 (const Vector3<Real>& rkNormal, Real fConstant)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
    m_afTuple[3] = fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::Plane3 (const Vector3<Real>& rkNormal,
    const Vector3<Real>& rkPoint)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
    m_afTuple[3] = rkNormal.Dot(rkPoint);
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::Plane3 (const Vector3<Real>& rkPoint0,
    const Vector3<Real>& rkPoint1, const Vector3<Real>& rkPoint2)
{
    Vector3<Real> kEdge1 = rkPoint1 - rkPoint0;
    Vector3<Real> kEdge2 = rkPoint2 - rkPoint0;
    Vector3<Real> kNormal = kEdge1.UnitCross(kEdge2);
    m_afTuple[0] = kNormal.X();
    m_afTuple[1] = kNormal.Y();
    m_afTuple[2] = kNormal.Z();
    m_afTuple[3] = kNormal.Dot(rkPoint0);
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::Plane3 (const Plane3& rkPlane)
{
    memcpy(m_afTuple,rkPlane.m_afTuple,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
void Plane3<Real>::SetNormal (const Vector3<Real>& rkNormal)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Plane3<Real>::GetNormal () const
{
    return Vector3<Real>(m_afTuple[0],m_afTuple[1],m_afTuple[2]);
}
//----------------------------------------------------------------------------
template <class Real>
void Plane3<Real>::SetConstant (Real fConstant)
{
    m_afTuple[3] = fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
Real Plane3<Real>::GetConstant () const
{
    return m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
void Plane3<Real>::Set (const Vector3<Real>& rkNormal, Real fConstant)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
    m_afTuple[3] = fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
void Plane3<Real>::Get (Vector3<Real>& rkNormal, Real& rfConstant) const
{
    rkNormal.X() = m_afTuple[0];
    rkNormal.Y() = m_afTuple[1];
    rkNormal.Z() = m_afTuple[2];
    rfConstant = m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::operator const Real* () const
{
    return m_afTuple;
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::operator Real* ()
{
    return m_afTuple;
}
//----------------------------------------------------------------------------
template <class Real>
Real Plane3<Real>::operator[] (int i) const
{
    assert( 0 <= i && i < 4 );
    return m_afTuple[i];
}
//----------------------------------------------------------------------------
template <class Real>
Real& Plane3<Real>::operator[] (int i)
{
    assert( 0 <= i && i < 4 );
    return m_afTuple[i];
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>::operator Point4<Real> ()
{
    return Point4<Real>(m_afTuple[0],m_afTuple[1],m_afTuple[2],-m_afTuple[3]);
}
//----------------------------------------------------------------------------
template <class Real>
Plane3<Real>& Plane3<Real>::operator= (const Plane3<Real>& rkPlane)
{
    memcpy(m_afTuple,rkPlane.m_afTuple,4*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
Real Plane3<Real>::DistanceTo (const Vector3<Real>& rkPoint) const
{
    return m_afTuple[0]*rkPoint.X() + m_afTuple[1]*rkPoint.Y() +
        m_afTuple[2]*rkPoint.Z() - m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
typename Plane3<Real>::Side Plane3<Real>::WhichSide
    (const Vector3<Real>& rkPoint) const
{
    Real fDistance = DistanceTo(rkPoint);

    if ( fDistance < (Real)0.0 )
        return Plane3<Real>::NEGATIVE_SIDE;

    if ( fDistance > (Real)0.0 )
        return Plane3<Real>::POSITIVE_SIDE;

    return Plane3<Real>::NO_SIDE;
}
//----------------------------------------------------------------------------
template <class Real>
void Plane3<Real>::Normalize ()
{
    // assert:  |N| > 0
    Real fSqrLength = m_afTuple[0]*m_afTuple[0] + m_afTuple[1]*m_afTuple[1] +
        m_afTuple[2]*m_afTuple[2];
    Real fInvLength = Math<Real>::InvSqrt(fSqrLength);
    for (int i = 0; i < 4; i++)
        m_afTuple[i] *= fInvLength;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Plane3<float>;
template class WML_ITEM Plane3<double>;
}
//----------------------------------------------------------------------------
