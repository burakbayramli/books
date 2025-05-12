// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistEllipsoidGeodesic.h"
#include "WmlIntegrate1.h"
using namespace Wml;

// This is experimental code.  It appears to work so far.  Numeric
// parameters that may be varied to see how the performance varies
// are
//
//   1.  ds, the default spacing between sample points on the initial curve
//   2.  tmax, the number of iterations in the relaxation.  It should be
//       possible instead to terminate the relaxation when the change in
//       arc length is below a given threshold.
//   3.  factor, the ratio dt/ds^2.  Try to choose as large as possible
//       while retaining numerical stability.

//----------------------------------------------------------------------------
template <class Real>
EllipsoidGeodesicDistance<Real>::EllipsoidGeodesicDistance (
    const Real afAxis[3],  const Vector3<Real>& rkP, const Vector3<Real>& rkQ,
    Real& rfDistance)
{
    // Calculate products P*P, P*Q, Q*Q, and P*DQ where
    // D = diag((1/a0)^2,(1/a1)^2,(1/a2)^2).
    m_fPP = rkP.Dot(rkP);
    m_fPQ = rkP.Dot(rkQ);
    m_fQQ = rkQ.Dot(rkQ);
    m_fPDQ =
        rkP.X()*rkQ.X()/(afAxis[0]*afAxis[0]) +
        rkP.Y()*rkQ.Y()/(afAxis[1]*afAxis[1]) +
        rkP.Z()*rkQ.Z()/(afAxis[2]*afAxis[2]);

    // length of initial curve
    Real fLimit = Math<Real>::ATan(Math<Real>::Sqrt(
        ((Real)1.0-m_fPDQ)/((Real)1.0+m_fPDQ)));
    rfDistance = Integrate1<Real>::RombergIntegral(-fLimit,fLimit,Speed);

    // calculate number of samples for the default spacing
    Real fDS = (Real)0.01;
    int iQuantity = 1 + (int)Math<Real>::Ceil(rfDistance/fDS);
    Real fInvQuantityM1 = ((Real)1.0)/(Real)(iQuantity-1);
    fDS = rfDistance*fInvQuantityM1;

    // allocate N sample points for the evolution scheme
    Real** aafX = new Real*[iQuantity];
    Real** aafY = new Real*[iQuantity];
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        aafX[i] = new Real[3];
        aafY[i] = new Real[3];
    }

    // Calculate initial curve as a planar elliptic arc from P to Q.
    // The curve is
    //   C(t) = a(t) P + b(t) Q
    // where
    //   a(t) = cos(t)/sqrt(2*(1+r)) - sin(t)/sqrt(2*(1-r))
    //   b(t) = cos(t)/sqrt(2*(1+r)) + sin(t)/sqrt(2*(1-r))
    //   r = P^T D Q,  D = diag((1/a0)^2,(1/a1)^2,(1/a2)^2)
    //   -t0 <= t <= t0, t0 = Tan^{-1}(sqrt((1-r)/(1+r)))
    //
    // Sample points are chosen to be uniformly spaced along curve.

    // end points remain fixed
    int iDim;
    for (iDim = 0; iDim < 3; iDim++)
    {
        aafX[0][iDim] = rkP[iDim];
        aafY[0][iDim] = rkP[iDim];
        aafX[iQuantity-1][iDim] = rkQ[iDim];
        aafY[iQuantity-1][iDim] = rkQ[iDim];
    }

    // compute evenly spaced points
    for (i = 1; i < iQuantity-1; i++)
    {
        // find point along curve at this distance from P
        Real fPartialLength = i*fDS;

        // use Newton's method
        Real fAngle = -fLimit + ((Real)2.0)*i*fLimit*fInvQuantityM1;
        const int iMaxNewton = 32;
        int iIter;
        for (iIter = 0; iIter < iMaxNewton; iIter++)
        {
            Real fG = Integrate1<Real>::RombergIntegral(-fLimit,fAngle,Speed)
                - fPartialLength;
            if ( Math<Real>::FAbs(fG) <= ms_fEpsilon )
                break;
            Real fDG = Speed(fAngle,this);
            if ( Math<Real>::FAbs(fDG) <= ms_fEpsilon )
            {
                iIter = iMaxNewton;
                break;
            }
            fAngle -= fG/fDG;
        }
        if ( iIter == iMaxNewton )
        {
            // Newton's failed.  Deallocate the sample points and
            // return an invalid distance.
            for (i = 0; i < iQuantity; i++)
            {
                delete[] aafX[i];
                delete[] aafY[i];
            }
            delete[] aafX;
            delete[] aafY;
            rfDistance = -(Real)1.0;
            return;
        }

        Real fCos = Math<Real>::Cos(fAngle);
        Real fSin = Math<Real>::Sin(fAngle);
        Real fUVal = fCos/Math<Real>::Sqrt(((Real)2.0)*((Real)1.0+m_fPDQ));
        Real fVVal = fSin/Math<Real>::Sqrt(((Real)2.0)*((Real)1.0-m_fPDQ));
        Real fAlph = fUVal - fVVal;
        Real fBeta = fUVal + fVVal;
        for (iDim = 0; iDim < 3; iDim++)
            aafX[i][iDim] = fAlph*rkP[iDim] + fBeta*rkQ[iDim];
    }

    // The general evolution scheme for finding geodesics is
    //
    //   X_t = X_{ss} - (X_{ss}*N) N, t > 0
    //   X(s,0) = C(s)  [initial conditions]
    //   X(0,t) = P, X(L,t) = Q  [boundary conditions, L=curve length]
    //
    // N is a unit normal to the ellipsoid.  Try to use central difference
    // in s and forward difference in t.  This approach generally can be
    // numerically unstable.  I want to see if the special case of an
    // ellipsoid is stable.

    const int iTMax = 16;
    int iT;
    for (iT = 1; iT <= iTMax; iT++)
    {
        // take a time step
        for (i = 1; i < iQuantity-1; i++)
        {
            // compute X_{ss}*N
            Real afDiff[3], afNorm[3];
            Real fNorLen = (Real)0.0;
            for (iDim = 0; iDim < 3; iDim++)
            {
                afDiff[iDim] = aafX[i+1][iDim] - ((Real)2.0)*aafX[i][iDim] +
                    aafX[i-1][iDim];
                afNorm[iDim] = aafX[i][iDim]/(afAxis[iDim]*afAxis[iDim]);
                fNorLen += afNorm[iDim]*afNorm[iDim];
            }
            Real fInvNorLen = Math<Real>::InvSqrt(fNorLen);
            for (iDim = 0; iDim < 3; iDim++)
                afNorm[iDim] *= fInvNorLen;
            Real fDot = afDiff[0]*afNorm[0] + afDiff[1]*afNorm[1] +
                afDiff[2]*afNorm[2];

            // evaluate the next point in time, dt/ds^2 = 0.125
            const Real fFactor = (Real)0.125;
            for (iDim = 0; iDim < 3; iDim++)
            {
                aafY[i][iDim] = aafX[i][iDim] + fFactor*(afDiff[iDim] -
                    fDot*afNorm[iDim]);
            }
        }

        // project back onto ellipsoid
        for (i = 1; i < iQuantity-1; i++)
        {
            Real fLevel = (Real)0.0;
            for (iDim = 0; iDim < 3; iDim++)
            {
                Real fTmp = aafY[i][iDim]/afAxis[iDim];
                fLevel += fTmp*fTmp;
            }
            fLevel = Math<Real>::Sqrt(fLevel);
            for (iDim = 0; iDim < 3; iDim++)
                aafX[i][iDim] = aafY[i][iDim]/fLevel;
        }
    }

    // compute length of polyline
    rfDistance = (Real)0.0;
    for (i = 1; i < iQuantity; i++)
    {
        Real fSegLen = (Real)0.0;
        for (iDim = 0; iDim < 3; iDim++)
        {
            Real fTmp = aafX[i][iDim] - aafX[i-1][iDim];
            fSegLen += fTmp*fTmp;
        }
        rfDistance += Math<Real>::Sqrt(fSegLen);
    }

    // deallocate the sample points
    for (i = 0; i < iQuantity; i++)
    {
        delete[] aafX[i];
        delete[] aafY[i];
    }
    delete[] aafX;
    delete[] aafY;
}
//----------------------------------------------------------------------------
template <class Real>
Real EllipsoidGeodesicDistance<Real>::Speed (Real fT, void* pvData)
{
    EllipsoidGeodesicDistance<Real>& rkSelf =
        *(EllipsoidGeodesicDistance<Real>*)pvData;

    Real fUNumer = -Math<Real>::Sin(fT);
    Real fVNumer = Math<Real>::Cos(fT);
    Real fUDenom = Math<Real>::Sqrt(((Real)2.0)*((Real)1.0+rkSelf.m_fPDQ));
    Real fVDenom = Math<Real>::Sqrt(((Real)2.0)*((Real)1.0-rkSelf.m_fPDQ));
    Real fUVal = fUNumer/fUDenom;
    Real fVVal = fVNumer/fVDenom;
    Real fDAlph = fUVal - fVVal;
    Real fDBeta = fUVal + fVVal;
    Real fTmp =
        fDAlph*fDAlph*rkSelf.m_fPP + 
        ((Real)2.0)*fDAlph*fDBeta*rkSelf.m_fPQ +
        fDBeta*fDBeta*rkSelf.m_fQQ;

    return Math<Real>::Sqrt(Math<Real>::FAbs(fTmp));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM EllipsoidGeodesicDistance<float>;
const float EllipsoidGeodesicDistancef::ms_fEpsilon = 1e-04f;

template class WML_ITEM EllipsoidGeodesicDistance<double>;
const double EllipsoidGeodesicDistanced::ms_fEpsilon = 1e-04;
}
//----------------------------------------------------------------------------
