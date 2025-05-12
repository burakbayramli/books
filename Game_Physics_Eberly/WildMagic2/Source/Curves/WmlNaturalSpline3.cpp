// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlNaturalSpline3.h"
#include "WmlIntegrate1.h"
#include "WmlLinearSystem.h"
#include "WmlPolynomial1.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
NaturalSpline3<Real>::NaturalSpline3 (BoundaryType eType, int iSegments,
    Real* afTime, Vector3<Real>* akPoint)
    :
    MultipleCurve3<Real>(iSegments,afTime)
{
    m_akA = akPoint;

    switch ( eType )
    {
        case BT_FREE:
        {
            CreateFreeSpline();
            break;
        }
        case BT_CLAMPED:
        {
            CreateClampedSpline();
            break;
        }
        case BT_CLOSED:
        {
            CreateClosedSpline();
            break;
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
NaturalSpline3<Real>::~NaturalSpline3 ()
{
    delete[] m_akA;
    delete[] m_akB;
    delete[] m_akC;
    delete[] m_akD;
}
//----------------------------------------------------------------------------
template <class Real>
void NaturalSpline3<Real>::CreateFreeSpline ()
{
    Real* afDt = new Real[m_iSegments];
    int i;
    for (i = 0; i < m_iSegments; i++)
        afDt[i] = m_afTime[i+1] - m_afTime[i];

    Real* afD2t = new Real[m_iSegments];
    for (i = 1; i < m_iSegments; i++)
        afD2t[i] = m_afTime[i+1] - m_afTime[i-1];

    Vector3<Real>* akAlpha = new Vector3<Real>[m_iSegments];
    for (i = 1; i < m_iSegments; i++)
    {
        Vector3<Real> kNumer = ((Real)3.0)*(afDt[i-1]*m_akA[i+1] -
            afD2t[i]*m_akA[i] + afDt[i]*m_akA[i-1]);
        Real fInvDenom = ((Real)1.0)/(afDt[i-1]*afDt[i]);
        akAlpha[i] = fInvDenom*kNumer;
    }

    Real* afEll = new Real[m_iSegments+1];
    Real* afMu = new Real[m_iSegments];
    Vector3<Real>* akZ = new Vector3<Real>[m_iSegments+1];
    Real fInv;

    afEll[0] = (Real)1.0;
    afMu[0] = (Real)0.0;
    akZ[0] = Vector3<Real>::ZERO;
    for (i = 1; i < m_iSegments; i++)
    {
        afEll[i] = ((Real)2.0)*afD2t[i] - afDt[i-1]*afMu[i-1];
        fInv = ((Real)1.0)/afEll[i];
        afMu[i] = fInv*afDt[i];
        akZ[i] = fInv*(akAlpha[i] - afDt[i-1]*akZ[i-1]);
    }
    afEll[m_iSegments] = (Real)1.0;
    akZ[m_iSegments] = Vector3<Real>::ZERO;

    m_akB = new Vector3<Real>[m_iSegments];
    m_akC = new Vector3<Real>[m_iSegments+1];
    m_akD = new Vector3<Real>[m_iSegments];

    m_akC[m_iSegments] = Vector3<Real>::ZERO;

    const Real fOneThird = (Real)(1.0/3.0);
    for (i = m_iSegments-1; i >= 0; i--)
    {
        m_akC[i] = akZ[i] - afMu[i]*m_akC[i+1];
        fInv = ((Real)1.0)/afDt[i];
        m_akB[i] = fInv*(m_akA[i+1] - m_akA[i]) - fOneThird*afDt[i]*(
            m_akC[i+1] + ((Real)2.0)*m_akC[i]);
        m_akD[i] = fOneThird*fInv*(m_akC[i+1] - m_akC[i]);
    }

    delete[] afDt;
    delete[] afD2t;
    delete[] akAlpha;
    delete[] afEll;
    delete[] afMu;
    delete[] akZ;
}
//----------------------------------------------------------------------------
template <class Real>
void NaturalSpline3<Real>::CreateClampedSpline ()
{
    Real* afDt = new Real[m_iSegments];
    int i;
    for (i = 0; i < m_iSegments; i++)
        afDt[i] = m_afTime[i+1] - m_afTime[i];

    Real* afD2t = new Real[m_iSegments];
    for (i = 1; i < m_iSegments; i++)
        afD2t[i] = m_afTime[i+1] - m_afTime[i-1];

    Vector3<Real>* akAlpha = new Vector3<Real>[m_iSegments+1];
    Real fInv = ((Real)1.0)/afDt[0];
    akAlpha[0] = ((Real)3.0)*(fInv - (Real)1.0)*(m_akA[1] - m_akA[0]);
    fInv = ((Real)1.0)/afDt[m_iSegments-1];
    akAlpha[m_iSegments] = ((Real)3.0)*((Real)1.0 - fInv)*(m_akA[m_iSegments]
        - m_akA[m_iSegments-1]);
    for (i = 1; i < m_iSegments; i++)
    {
        Vector3<Real> kNumer = ((Real)3.0)*(afDt[i-1]*m_akA[i+1] -
            afD2t[i]*m_akA[i] + afDt[i]*m_akA[i-1]);
        Real fInvDenom = ((Real)1.0)/(afDt[i-1]*afDt[i]);
        akAlpha[i] = fInvDenom*kNumer;
    }

    Real* afEll = new Real[m_iSegments+1];
    Real* afMu = new Real[m_iSegments];
    Vector3<Real>* akZ = new Vector3<Real>[m_iSegments+1];

    afEll[0] = ((Real)2.0)*afDt[0];
    afMu[0] = (Real)0.5;
    fInv = ((Real)1.0)/afEll[0];
    akZ[0] = fInv*akAlpha[0];

    for (i = 1; i < m_iSegments; i++)
    {
        afEll[i] = ((Real)2.0)*afD2t[i] - afDt[i-1]*afMu[i-1];
        fInv = ((Real)1.0)/afEll[i];
        afMu[i] = fInv*afDt[i];
        akZ[i] = fInv*(akAlpha[i] - afDt[i-1]*akZ[i-1]);
    }
    afEll[m_iSegments] = afDt[m_iSegments-1]*(((Real)2.0) -
        afMu[m_iSegments-1]);
    fInv = ((Real)1.0)/afEll[m_iSegments];
    akZ[m_iSegments] = fInv*(akAlpha[m_iSegments] - afDt[m_iSegments-1]*
        akZ[m_iSegments-1]);

    m_akB = new Vector3<Real>[m_iSegments];
    m_akC = new Vector3<Real>[m_iSegments+1];
    m_akD = new Vector3<Real>[m_iSegments];

    m_akC[m_iSegments] = akZ[m_iSegments];

    const Real fOneThird = (Real)(1.0/3.0);
    for (i = m_iSegments-1; i >= 0; i--)
    {
        m_akC[i] = akZ[i] - afMu[i]*m_akC[i+1];
        fInv = ((Real)1.0)/afDt[i];
        m_akB[i] = fInv*(m_akA[i+1] - m_akA[i]) - fOneThird*afDt[i]*(
            m_akC[i+1] + ((Real)2.0)*m_akC[i]);
        m_akD[i] = fOneThird*fInv*(m_akC[i+1] - m_akC[i]);
    }

    delete[] afDt;
    delete[] afD2t;
    delete[] akAlpha;
    delete[] afEll;
    delete[] afMu;
    delete[] akZ;
}
//----------------------------------------------------------------------------
template <class Real>
void NaturalSpline3<Real>::CreateClosedSpline ()
{
    // TO DO.  A general linear system solver is used here.  The matrix
    // corresponding to this case is actually "cyclic banded", so a faster
    // linear solver can be used.  The current linear system code does not
    // have such a solver.

    Real* afDt = new Real[m_iSegments];
    int i;
    for (i = 0; i < m_iSegments; i++)
        afDt[i] = m_afTime[i+1] - m_afTime[i];

    // construct matrix of system
    GMatrix<Real> kMat(m_iSegments+1,m_iSegments+1);
    kMat[0][0] = (Real)1.0;
    kMat[0][m_iSegments] = (Real)-1.0;
    for (i = 1; i <= m_iSegments-1; i++)
    {
        kMat[i][i-1] = afDt[i-1];
        kMat[i][i  ] = ((Real)2.0)*(afDt[i-1] + afDt[i]);
        kMat[i][i+1] = afDt[i];
    }
    kMat[m_iSegments][m_iSegments-1] = afDt[m_iSegments-1];
    kMat[m_iSegments][0] = ((Real)2.0)*(afDt[m_iSegments-1] + afDt[0]);
    kMat[m_iSegments][1] = afDt[0];

    // construct right-hand side of system
    m_akC = new Vector3<Real>[m_iSegments+1];
    m_akC[0] = Vector3<Real>::ZERO;
    Real fInv0, fInv1;
    for (i = 1; i <= m_iSegments-1; i++)
    {
        fInv0 = ((Real)1.0)/afDt[i];
        fInv1 = ((Real)1.0)/afDt[i-1];
        m_akC[i] = ((Real)3.0)*(fInv0*(m_akA[i+1] - m_akA[i]) -
            fInv1*(m_akA[i] - m_akA[i-1]));
    }
    fInv0 = ((Real)1.0)/afDt[0];
    fInv1 = ((Real)1.0)/afDt[m_iSegments-1];
    m_akC[m_iSegments] = ((Real)3.0)*(fInv0*(m_akA[1] - m_akA[0]) -
        fInv1*(m_akA[0] - m_akA[m_iSegments-1]));

    // solve the linear systems
    Real* afInput = new Real[m_iSegments+1];
    Real* afOutput = new Real[m_iSegments+1];

    for (i = 0; i <= m_iSegments; i++)
        afInput[i] = m_akC[i].X();
    LinearSystem<Real>::Solve(kMat,afInput,afOutput);
    for (i = 0; i <= m_iSegments; i++)
        m_akC[i].X() = afOutput[i];

    for (i = 0; i <= m_iSegments; i++)
        afInput[i] = m_akC[i].Y();
    LinearSystem<Real>::Solve(kMat,afInput,afOutput);
    for (i = 0; i <= m_iSegments; i++)
        m_akC[i].Y() = afOutput[i];

    for (i = 0; i <= m_iSegments; i++)
        afInput[i] = m_akC[i].Z();
    LinearSystem<Real>::Solve(kMat,afInput,afOutput);
    for (i = 0; i <= m_iSegments; i++)
        m_akC[i].Z() = afOutput[i];

    delete[] afInput;
    delete[] afOutput;
    // end linear system solving

    const Real fOneThird = (Real)(1.0/3.0);
    m_akB = new Vector3<Real>[m_iSegments];
    m_akD = new Vector3<Real>[m_iSegments];
    for (i = 0; i < m_iSegments; i++)
    {
        fInv0 = ((Real)1.0)/afDt[i];
        m_akB[i] = fInv0*(m_akA[i+1] - m_akA[i]) - fOneThird*(m_akC[i+1] +
            ((Real)2.0)*m_akC[i])*afDt[i];
        m_akD[i] = fOneThird*fInv0*(m_akC[i+1] - m_akC[i]);
    }

    delete[] afDt;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* NaturalSpline3<Real>::GetPoints () const
{
    return m_akA;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NaturalSpline3<Real>::GetPosition (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    Vector3<Real> kResult = m_akA[iKey] + fDt*(m_akB[iKey] + fDt*(m_akC[iKey]
        + fDt*m_akD[iKey]));

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NaturalSpline3<Real>::GetFirstDerivative (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    Vector3<Real> kResult = m_akB[iKey] + fDt*(((Real)2.0)*m_akC[iKey] +
        ((Real)3.0)*fDt*m_akD[iKey]);

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NaturalSpline3<Real>::GetSecondDerivative (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    Vector3<Real> kResult = ((Real)2.0)*m_akC[iKey] +
        ((Real)6.0)*fDt*m_akD[iKey];

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NaturalSpline3<Real>::GetThirdDerivative (Real fTime) const
{
    int iKey;
    Real fDt;
    GetKeyInfo(fTime,iKey,fDt);

    Vector3<Real> kResult = ((Real)6.0)*m_akD[iKey];

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real NaturalSpline3<Real>::GetSpeedKey (int iKey, Real fTime) const
{
    Vector3<Real> kVelocity = m_akB[iKey] + fTime*(((Real)2.0)*m_akC[iKey] +
        ((Real)3.0)*fTime*m_akD[iKey]);
    return kVelocity.Length();
}
//----------------------------------------------------------------------------
template <class Real>
Real NaturalSpline3<Real>::GetLengthKey (int iKey, Real fT0, Real fT1) const
{
    ThisPlusKey kData(this,iKey);
    return Integrate1<Real>::RombergIntegral(fT0,fT1,GetSpeedWithData,
        (void*)&kData);
}
//----------------------------------------------------------------------------
template <class Real>
Real NaturalSpline3<Real>::GetVariationKey (int iKey, Real fT0, Real fT1,
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
template class WML_ITEM NaturalSpline3<float>;
template class WML_ITEM NaturalSpline3<double>;
}
//----------------------------------------------------------------------------
