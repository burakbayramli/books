// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTorus3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Torus3<Real>::Torus3 (Real fRo, Real fRi)
{
    assert( fRo > (Real)0.0 && fRi > (Real)0.0 );

    m_fRo = fRo;
    m_fRi = fRi;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Torus3<Real>::Ro () const
{
    return m_fRo;
}
//----------------------------------------------------------------------------
template <class Real>
const Real& Torus3<Real>::Ri () const
{
    return m_fRi;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Torus3<Real>::Position (Real fS, Real fT)
{
    Real fTwoPiS = Math<Real>::TWO_PI*fS;
    Real fTwoPiT = Math<Real>::TWO_PI*fT;
    Real fCosTwoPiS = Math<Real>::Cos(fTwoPiS);
    Real fSinTwoPiS = Math<Real>::Sin(fTwoPiS);
    Real fCosTwoPiT = Math<Real>::Cos(fTwoPiT);
    Real fSinTwoPiT = Math<Real>::Sin(fTwoPiT);
    Real fRc = m_fRo + m_fRi*fCosTwoPiT;
    Vector3<Real> kPos(fRc*fCosTwoPiS,fRc*fSinTwoPiS,m_fRi*fSinTwoPiT);
    return kPos;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Torus3<Real>::Normal (Real fS, Real fT)
{
    Real fTwoPiS = Math<Real>::TWO_PI*fS;
    Real fCosTwoPiS = Math<Real>::Cos(fTwoPiS);
    Real fSinTwoPiS = Math<Real>::Sin(fTwoPiS);
    Vector3<Real> kPos = Position(fS,fT);
    Vector3<Real> kNor(kPos.X()-m_fRo*fCosTwoPiS,kPos.Y()-m_fRo*fSinTwoPiS,
        kPos.Z());
    kNor.Normalize();
    return kNor;
}
//----------------------------------------------------------------------------
template <class Real>
void Torus3<Real>::GetParameters (const Vector3<Real>& rkPos, Real& rfS,
    Real& rfT) const
{
    Real fRc = Math<Real>::Sqrt(rkPos.X()*rkPos.X() + rkPos.Y()*rkPos.Y());
    Real fAngle;

    if ( fRc < Math<Real>::EPSILON )
    {
        rfS = (Real)0.0;
    }
    else
    {
        fAngle = Math<Real>::ATan2(rkPos.Y(),rkPos.X());
        if ( fAngle >= (Real)0.0 )
            rfS = fAngle*Math<Real>::INV_TWO_PI;
        else
            rfS = (Real)1.0 + fAngle*Math<Real>::INV_TWO_PI;
    }

    Real fDiff = fRc - m_fRo;
    if ( Math<Real>::FAbs(fDiff) < Math<Real>::EPSILON
    &&   Math<Real>::FAbs(rkPos.Z()) < Math<Real>::EPSILON )
    {
        rfT = (Real)0.0;
    }
    else
    {
        fAngle = Math<Real>::ATan2(rkPos.Z(),fDiff);
        if ( fAngle >= (Real)0.0 )
            rfT = fAngle*Math<Real>::INV_TWO_PI;
        else
            rfT = (Real)1.0 + fAngle*Math<Real>::INV_TWO_PI;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Torus3<float>;
template class WML_ITEM Torus3<double>;
}
//----------------------------------------------------------------------------
