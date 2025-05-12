// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlHalfSpace3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
HalfSpace3<Real>::HalfSpace3 ()
{
    for (int i = 0; i < 4; i++)
        m_afTuple[i] = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
HalfSpace3<Real>::HalfSpace3 (const Vector3<Real>& rkNormal, Real fConstant)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
    m_afTuple[3] = fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
HalfSpace3<Real>::HalfSpace3 (const Vector3<Real>& rkNormal,
    const Vector3<Real>& rkPoint)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
    m_afTuple[3] = rkNormal.Dot(rkPoint);
}
//----------------------------------------------------------------------------
template <class Real>
HalfSpace3<Real>::HalfSpace3 (const Vector3<Real>& rkPoint0,
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
HalfSpace3<Real>::HalfSpace3 (const HalfSpace3& rkHalfSpace)
{
    memcpy(m_afTuple,rkHalfSpace.m_afTuple,4*sizeof(Real));
}
//----------------------------------------------------------------------------
template <class Real>
void HalfSpace3<Real>::SetNormal (const Vector3<Real>& rkNormal)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> HalfSpace3<Real>::GetNormal () const
{
    return Vector3<Real>(m_afTuple[0],m_afTuple[1],m_afTuple[2]);
}
//----------------------------------------------------------------------------
template <class Real>
void HalfSpace3<Real>::SetConstant (Real fConstant)
{
    m_afTuple[3] = fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
Real HalfSpace3<Real>::GetConstant () const
{
    return m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
void HalfSpace3<Real>::Set (const Vector3<Real>& rkNormal, Real fConstant)
{
    m_afTuple[0] = rkNormal.X();
    m_afTuple[1] = rkNormal.Y();
    m_afTuple[2] = rkNormal.Z();
    m_afTuple[3] = fConstant;
}
//----------------------------------------------------------------------------
template <class Real>
void HalfSpace3<Real>::Get (Vector3<Real>& rkNormal, Real& rfConstant) const
{
    rkNormal.X() = m_afTuple[0];
    rkNormal.Y() = m_afTuple[1];
    rkNormal.Z() = m_afTuple[2];
    rfConstant = m_afTuple[3];
}
//----------------------------------------------------------------------------
template <class Real>
HalfSpace3<Real>& HalfSpace3<Real>::operator=
    (const HalfSpace3<Real>& rkHalfSpace)
{
    memcpy(m_afTuple,rkHalfSpace.m_afTuple,4*sizeof(Real));
    return *this;
}
//----------------------------------------------------------------------------
template <class Real>
void HalfSpace3<Real>::Normalize ()
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
template class WML_ITEM HalfSpace3<float>;
template class WML_ITEM HalfSpace3<double>;
}
//----------------------------------------------------------------------------
