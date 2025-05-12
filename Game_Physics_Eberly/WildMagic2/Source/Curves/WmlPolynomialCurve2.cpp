// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPolynomialCurve2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
PolynomialCurve2<Real>::PolynomialCurve2 (Polynomial1<Real>* pkXPoly,
    Polynomial1<Real>* pkYPoly)
    :
    SingleCurve2<Real>(0.0,1.0)
{
    assert( pkXPoly && pkYPoly );
    assert( pkXPoly->GetDegree() == pkYPoly->GetDegree() );

    m_pkXPoly = pkXPoly;
    m_pkYPoly = pkYPoly;
    m_kXDer1 = m_pkXPoly->GetDerivative();
    m_kYDer1 = m_pkYPoly->GetDerivative();
    m_kXDer2 = m_kXDer1.GetDerivative();
    m_kYDer2 = m_kYDer1.GetDerivative();
    m_kXDer3 = m_kXDer2.GetDerivative();
    m_kYDer3 = m_kYDer2.GetDerivative();
}
//----------------------------------------------------------------------------
template <class Real>
PolynomialCurve2<Real>::~PolynomialCurve2 ()
{
    delete m_pkXPoly;
    delete m_pkYPoly;
}
//----------------------------------------------------------------------------
template <class Real>
int PolynomialCurve2<Real>::GetDegree () const
{
    return m_pkXPoly->GetDegree();
}
//----------------------------------------------------------------------------
template <class Real>
const Polynomial1<Real>* PolynomialCurve2<Real>::GetXPolynomial () const
{
    return m_pkXPoly;
}
//----------------------------------------------------------------------------
template <class Real>
const Polynomial1<Real>* PolynomialCurve2<Real>::GetYPolynomial () const
{
    return m_pkYPoly;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> PolynomialCurve2<Real>::GetPosition (Real fTime) const
{
    Vector2<Real> kResult((*m_pkXPoly)(fTime),(*m_pkYPoly)(fTime));
    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> PolynomialCurve2<Real>::GetFirstDerivative (Real fTime) const
{
    Vector2<Real> kResult(m_kXDer1(fTime),m_kYDer1(fTime));
    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> PolynomialCurve2<Real>::GetSecondDerivative (Real fTime) const
{
    Vector2<Real> kResult(m_kXDer2(fTime),m_kYDer2(fTime));
    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> PolynomialCurve2<Real>::GetThirdDerivative (Real fTime) const
{
    Vector2<Real> kResult(m_kXDer3(fTime),m_kYDer3(fTime));
    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialCurve2<Real>::GetVariation (Real fT0, Real fT1,
    const Vector2<Real>* pkP0, const Vector2<Real>* pkP1) const
{
    Vector2<Real> kP0, kP1;
    if ( !pkP0 )
    {
        kP0 = GetPosition(fT0);
        pkP0 = &kP0;
    }
    if ( !pkP1 )
    {
        kP1 = GetPosition(fT1);
        pkP1 = &kP1;
    }
    
    // construct line segment A + t*B
    Real fInvDT = ((Real)1.0)/(fT1 - fT0);
    Vector2<Real> kB = fInvDT*(*pkP1 - *pkP0);
    Vector2<Real> kA = *pkP0 - fT0*kB;
    Polynomial1<Real> kLx(1), kLy(1);
    kLx[0] = kA.X();
    kLx[1] = kB.X();
    kLy[0] = kA.Y();
    kLy[1] = kB.Y();

    // compute |X(t) - L(t)|^2
    Polynomial1<Real> kDx = *m_pkXPoly - kLx;
    Polynomial1<Real> kDy = *m_pkYPoly - kLy;
    Polynomial1<Real> kNormSqr = kDx*kDx + kDy*kDy;

    // compute indefinite integral of |X(t)-L(t)|^2
    Polynomial1<Real> kIntegral(kNormSqr.GetDegree()+1);
    kIntegral[0] = (Real)0.0;
    for (int i = 1; i <= kIntegral.GetDegree(); i++)
        kIntegral[i] = kNormSqr[i-1]/i;

    // compute definite Integral(t0,t1,|X(t)-L(t)|^2)
    Real fResult = kIntegral(fT1) - kIntegral(fT0);
    return fResult;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM PolynomialCurve2<float>;
template class WML_ITEM PolynomialCurve2<double>;
}
//----------------------------------------------------------------------------
