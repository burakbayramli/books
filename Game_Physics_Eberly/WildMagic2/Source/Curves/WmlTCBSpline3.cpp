// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTCBSpline3.h"
#include "WmlIntegrate1.h"
#include "WmlPolynomial1.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
TCBSpline3<Real>::TCBSpline3 (int iSegments, Real* afTime,
    Vector3<Real>* akPoint, Real* afTension, Real* afContinuity, Real* afBias)
    :
    MultipleCurve3<Real>(iSegments,afTime)
{
    // TO DO.  Add 'boundary type' just as in natural splines.
    assert( m_iSegments >= 3 );

    // all four of these arrays have m_iSegments+1 elements
    m_akPoint = akPoint;
    m_afTension = afTension;
    m_afContinuity = afContinuity;
    m_afBias = afBias;

    m_akA = new Vector3<Real>[m_iSegments];
    m_akB = new Vector3<Real>[m_iSegments];
    m_akC = new Vector3<Real>[m_iSegments];
    m_akD = new Vector3<Real>[m_iSegments];

    // For now, treat the first point as if it occurred twice.
    ComputePoly(0,0,1,2);

    for (int i = 1; i < m_iSegments-1; i++)
        ComputePoly(i-1,i,i+1,i+2);

    // For now, treat the last point as if it occurred twice.
    ComputePoly(m_iSegments-2,m_iSegments-1,m_iSegments,m_iSegments);

}
//----------------------------------------------------------------------------
template <class Real>
TCBSpline3<Real>::~TCBSpline3 ()
{
    delete[] m_akPoint;
    delete[] m_afTension;
    delete[] m_afContinuity;
    delete[] m_afBias;
    delete[] m_akA;
    delete[] m_akB;
    delete[] m_akC;
    delete[] m_akD;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* TCBSpline3<Real>::GetPoints () const
{
    return m_akPoint;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* TCBSpline3<Real>::GetTensions () const
{
    return m_afTension;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* TCBSpline3<Real>::GetContinuities () const
{
    return m_afContinuity;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* TCBSpline3<Real>::GetBiases () const
{
    return m_afBias;
}
//----------------------------------------------------------------------------
template <class Real>
void TCBSpline3<Real>::ComputePoly (int i0, int i1, int i2, int i3)
{
    Vector3<Real> kDiff = m_akPoint[i2] - m_akPoint[i1];
    Real fDt = m_afTime[i2] - m_afTime[i1];

    // build multipliers at P1
    Real fOmt0 = (Real)1.0 - m_afTension[i1];
    Real fOmc0 = (Real)1.0 - m_afContinuity[i1];
    Real fOpc0 = (Real)1.0 + m_afContinuity[i1];
    Real fOmb0 = (Real)1.0 - m_afBias[i1];
    Real fOpb0 = (Real)1.0 + m_afBias[i1];
    Real fAdj0 = ((Real)2.0)*fDt/(m_afTime[i2]-m_afTime[i0]);
    Real fOut0 = ((Real)0.5)*fAdj0*fOmt0*fOpc0*fOpb0;
    Real fOut1 = ((Real)0.5)*fAdj0*fOmt0*fOmc0*fOmb0;

    // build outgoing tangent at P1
    Vector3<Real> kTOut = fOut1*kDiff + fOut0*(m_akPoint[i1] - m_akPoint[i0]);

    // build multipliers at point P2
    Real fOmt1 = (Real)1.0 - m_afTension[i2];
    Real fOmc1 = (Real)1.0 - m_afContinuity[i2];
    Real fOpc1 = (Real)1.0 + m_afContinuity[i2];
    Real fOmb1 = (Real)1.0 - m_afBias[i2];
    Real fOpb1 = (Real)1.0 + m_afBias[i2];
    Real fAdj1 = ((Real)2.0)*fDt/(m_afTime[i3] - m_afTime[i1]);
    Real fIn0 = ((Real)0.5)*fAdj1*fOmt1*fOmc1*fOpb1;
    Real fIn1 = ((Real)0.5)*fAdj1*fOmt1*fOpc1*fOmb1;

    // build incoming tangent at P2
    Vector3<Real> kTIn = fIn1*(m_akPoint[i3] - m_akPoint[i2]) + fIn0*kDiff;

    m_akA[i1] = m_akPoint[i1];
    m_akB[i1] = kTOut;
    m_akC[i1] = ((Real)3.0)*kDiff - ((Real)2.0)*kTOut - kTIn;
    m_akD[i1] = ((Real)-2.0)*kDiff + kTOut + kTIn;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> TCBSpline3<Real>::GetPosition (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    fDt /= (m_afTime[iKey+1] - m_afTime[iKey]);

    Vector3<Real> kResult = m_akA[iKey] + fDt*(m_akB[iKey] + fDt*(m_akC[iKey]
        + fDt*m_akD[iKey]));

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> TCBSpline3<Real>::GetFirstDerivative (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    fDt /= (m_afTime[iKey+1] - m_afTime[iKey]);

    Vector3<Real> kResult = m_akB[iKey] + fDt*(((Real)2.0)*m_akC[iKey] +
        ((Real)3.0)*fDt*m_akD[iKey]);

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> TCBSpline3<Real>::GetSecondDerivative (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    fDt /= (m_afTime[iKey+1] - m_afTime[iKey]);

    Vector3<Real> kResult = ((Real)2.0)*m_akC[iKey] +
        ((Real)6.0)*fDt*m_akD[iKey];

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> TCBSpline3<Real>::GetThirdDerivative (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    fDt /= (m_afTime[iKey+1] - m_afTime[iKey]);

    Vector3<Real> kResult = ((Real)6.0)*m_akD[iKey];

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real TCBSpline3<Real>::GetSpeedKey (int iKey, Real fTime) const
{
    Vector3<Real> kVelocity = m_akB[iKey] + fTime*(((Real)2.0)*m_akC[iKey] +
        ((Real)3.0)*fTime*m_akD[iKey]);
    return kVelocity.Length();
}
//----------------------------------------------------------------------------
template <class Real>
Real TCBSpline3<Real>::GetLengthKey (int iKey, Real fT0, Real fT1) const
{
    ThisPlusKey kData(this,iKey);
    return Integrate1<Real>::RombergIntegral(fT0,fT1,GetSpeedWithData,
        (void*)&kData);
}
//----------------------------------------------------------------------------
template <class Real>
Real TCBSpline3<Real>::GetVariationKey (int iKey, Real fT0, Real fT1,
    const Vector3<Real>& rkA, const Vector3<Real>& rkB) const
{
    Polynomial1<Real> kXPoly(3);
    kXPoly[0] = m_akA[iKey].X();
    kXPoly[1] = m_akB[iKey].X();
    kXPoly[2] = m_akC[iKey].X();
    kXPoly[3] = m_akD[iKey].X();

    Polynomial1<Real> kYPoly(3);
    kYPoly[0] = m_akA[iKey].Y();
    kYPoly[1] = m_akB[iKey].Y();
    kYPoly[2] = m_akC[iKey].Y();
    kYPoly[3] = m_akD[iKey].Y();

    Polynomial1<Real> kZPoly(3);
    kZPoly[0] = m_akA[iKey].Z();
    kZPoly[1] = m_akB[iKey].Z();
    kZPoly[2] = m_akC[iKey].Z();
    kZPoly[3] = m_akD[iKey].Z();

    // construct line segment A + t*B
    Polynomial1<Real> kLx(1), kLy(1), kLz(1);
    kLx[0] = rkA.X();
    kLx[1] = rkB.X();
    kLy[0] = rkA.Y();
    kLy[1] = rkB.Y();
    kLz[0] = rkA.Z();
    kLz[1] = rkB.Z();

    // compute |X(t) - L(t)|^2
    Polynomial1<Real> kDx = kXPoly - kLx;
    Polynomial1<Real> kDy = kYPoly - kLy;
    Polynomial1<Real> kDz = kZPoly - kLz;
    Polynomial1<Real> kNormSqr = kDx*kDx + kDy*kDy + kDz*kDz;

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
template class WML_ITEM TCBSpline3<float>;
template class WML_ITEM TCBSpline3<double>;
}
//----------------------------------------------------------------------------
