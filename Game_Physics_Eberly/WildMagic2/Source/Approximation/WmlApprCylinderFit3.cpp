// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprCylinderFit3.h"
#include "WmlApprLineFit3.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static Real UpdateInvRSqr (int iQuantity, const Vector3<Real>* akPoint,
    const Vector3<Real>& rkC, const Vector3<Real>& rkU, Real& rfInvRSqr)
{
    Real fASum = (Real)0.0, fAASum = (Real)0.0;
    for (int i = 0; i < iQuantity; i++)
    {
        Vector3<Real> kDelta = akPoint[i] - rkC;
        Vector3<Real> kDxU = kDelta.Cross(rkU);
        Real fL2 = kDxU.SquaredLength();
        fASum += fL2;
        fAASum += fL2*fL2;
    }

    rfInvRSqr = fASum/fAASum;
    Real fMin = (Real)1.0 - rfInvRSqr*fASum/(Real)iQuantity;
    return fMin;
}
//----------------------------------------------------------------------------
template <class Real>
static Real UpdateDirection (int iQuantity, const Vector3<Real>* akPoint,
    const Vector3<Real>& rkC, Vector3<Real>& rkU, Real& rfInvRSqr)
{
    Real fInvQuantity = ((Real)1.0)/(Real)iQuantity;
    int i;
    Vector3<Real> kDelta, kDxU, kDxVDir;
    Real fA, fB, fC;

    // compute direction of steepest descent
    Vector3<Real> kVDir = Vector3<Real>::ZERO;
    Real fAMean = (Real)0.0, fAAMean = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        kDelta = akPoint[i] - rkC;
        kDxU = kDelta.Cross(rkU);
        fA = rfInvRSqr*kDxU.SquaredLength() - (Real)1.0;
        fAMean += fA;
        fAAMean += fA*fA;
        kVDir.X() += fA*(rkU.X()*(kDelta.Y()*kDelta.Y() +
            kDelta.Z()*kDelta.Z()) - kDelta.X()*(rkU.Y()*kDelta.Y() +
            rkU.Z()*kDelta.Z()));
        kVDir.Y() += fA*(rkU.Y()*(kDelta.X()*kDelta.X() +
            kDelta.Z()*kDelta.Z()) - kDelta.Y()*(rkU.X()*kDelta.X() +
            rkU.Z()*kDelta.Z()));
        kVDir.Z() += fA*(rkU.Z()*(kDelta.X()*kDelta.X() + 
            kDelta.Y()*kDelta.Y()) - kDelta.Z()*(rkU.X()*kDelta.X() +
            rkU.Y()*kDelta.Y()));
    }
    fAMean *= fInvQuantity;
    fAAMean *= fInvQuantity;
    if ( kVDir.Normalize() < Math<Real>::EPSILON )
        return fAAMean;

    // compute 4th-degree polynomial for line of steepest descent
    Real fABMean = (Real)0.0, fACMean = (Real)0.0;
    Real fBBMean = (Real)0.0, fBCMean = (Real)0.0, fCCMean = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        kDelta = akPoint[i] - rkC;
        kDxU = kDelta.Cross(rkU);
        kDxVDir = kDelta.Cross(kVDir);
        fA = rfInvRSqr*kDxU.SquaredLength() - (Real)1.0;
        fB = rfInvRSqr*(kDxU.Dot(kDxVDir));
        fC = rfInvRSqr*kDxVDir.SquaredLength();
        fABMean += fA*fB;
        fACMean += fA*fC;
        fBBMean += fB*fB;
        fBCMean += fB*fC;
        fCCMean += fC*fC;
    }
    fABMean *= fInvQuantity;
    fACMean *= fInvQuantity;
    fBBMean *= fInvQuantity;
    fBCMean *= fInvQuantity;
    fCCMean *= fInvQuantity;

    Polynomial1<Real> kPoly(4);
    kPoly[0] = fAAMean;
    kPoly[1] = -((Real)4.0)*fABMean;
    kPoly[2] = ((Real)2.0)*fACMean + ((Real)4.0)*fBBMean;
    kPoly[3] = -((Real)4.0)*fBCMean;
    kPoly[4] = fCCMean;

    Polynomial1<Real> kDPoly = kPoly.GetDerivative();

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindA(kDPoly[0],kDPoly[1],kDPoly[2],kDPoly[3]);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    Real fMin = kPoly((Real)0.0);
    int iMin = -1;
    for (i = 0; i < iCount; i++)
    {
        Real fValue = kPoly(afRoot[i]);
        if ( fValue < fMin )
        {
            fMin = fValue;
            iMin = i;
        }
    }

    if ( iMin >= 0 )
    {
        rkU -= afRoot[iMin]*kVDir;
        Real fLength = rkU.Normalize();
        rfInvRSqr *= fLength*fLength;
    }

    return fMin;
}
//----------------------------------------------------------------------------
template <class Real>
static Real UpdateCenter (int iQuantity, const Vector3<Real>* akPoint,
    Vector3<Real>& rkC, const Vector3<Real>& rkU, const Real& rfInvRSqr)
{
    Real fInvQuantity = ((Real)1.0)/(Real)iQuantity;
    int i;
    Vector3<Real> kDelta, kDxU, kDxCDir;
    Real fA, fB, fC;

    // compute direction of steepest descent
    Vector3<Real> kCDir = Vector3<Real>::ZERO;
    Real fAMean = (Real)0.0, fAAMean = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        kDelta = akPoint[i] - rkC;
        kDxU = kDelta.Cross(rkU);
        fA = rfInvRSqr*kDxU.SquaredLength() - (Real)1.0;
        fAMean += fA;
        fAAMean += fA*fA;
        kCDir += fA*(kDelta-rkU.Dot(kDelta)*rkU); // |U|=1 assumed
    }
    fAMean *= fInvQuantity;
    fAAMean *= fInvQuantity;
    if ( kCDir.Normalize() < Math<Real>::EPSILON )
        return fAAMean;

    // compute 4th-degree polynomial for line of steepest descent
    kDxCDir = kCDir.Cross(rkU);
    fC = kDxCDir.SquaredLength()*fInvQuantity*rfInvRSqr;
    Real fBMean = (Real)0.0,  fABMean = (Real)0.0, fBBMean = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        kDelta = akPoint[i] - rkC;
        kDxU = kDelta.Cross(rkU);
        fA = rfInvRSqr*kDxU.SquaredLength() - (Real)1.0;
        fB = rfInvRSqr*(kDxU.Dot(kDxCDir));
        fBMean += fB;
        fABMean += fA*fB;
        fBBMean += fB*fB;
    }
    fBMean *= fInvQuantity;
    fABMean *= fInvQuantity;
    fBBMean *= fInvQuantity;

    Polynomial1<Real> kPoly(4);
    kPoly[0] = fAAMean;
    kPoly[1] = ((Real)4.0)*fABMean;
    kPoly[2] = ((Real)2.0)*fC*fAMean + ((Real)4.0)*fBBMean;
    kPoly[3] = ((Real)4.0)*fC*fBMean;
    kPoly[4] = fC*fC;

    Polynomial1<Real> kDPoly = kPoly.GetDerivative();

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindA(kDPoly[0],kDPoly[1],kDPoly[2],kDPoly[3]);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    Real fMin = kPoly((Real)0.0);
    int iMin = -1;
    for (i = 0; i < iCount; i++)
    {
        Real fValue = kPoly(afRoot[i]);
        if ( fValue < fMin )
        {
            fMin = fValue;
            iMin = i;
        }
    }

    if ( iMin >= 0 )
        rkC -= afRoot[iMin]*kCDir;

    return fMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::CylinderFit (int iQuantity, const Vector3<Real>* akPoint,
    Vector3<Real>& rkC, Vector3<Real>& rkU, Real& rfR, Real& rfH,
    bool bInputsAreInitialGuess)
{
    Real fInvRSqr;
    if ( !bInputsAreInitialGuess )
    {
        // Find the least-squares line that fits the data and use it as an
        // initial guess for the cylinder axis.
        OrthogonalLineFit(iQuantity,akPoint,rkC,rkU);
    }

    Real fError = Math<Real>::MAX_REAL;
    const int iMax = 8;
    int i;
    for (i = 0; i < iMax; i++)
    {
        fError = UpdateInvRSqr(iQuantity,akPoint,rkC,rkU,fInvRSqr);
        fError = UpdateDirection(iQuantity,akPoint,rkC,rkU,fInvRSqr);
        fError = UpdateCenter(iQuantity,akPoint,rkC,rkU,fInvRSqr);
    }

    // Compute the radius.
    rfR = Math<Real>::InvSqrt(fInvRSqr);

    // Project points onto fitted axis to determine extent of cylinder along
    // the axis.
    Real fTMin = rkU.Dot(akPoint[0]-rkC), fTMax = fTMin;
    for (i = 1; i < iQuantity; i++)
    {
        Real fT = rkU.Dot(akPoint[i]-rkC);
        if ( fT < fTMin )
            fTMin = fT;
        else if ( fT > fTMax )
            fTMax = fT;
    }

    // Compute the height.  Adjust the center to point that projects to
    // midpoint of extent.
    rfH = fTMax - fTMin;
    rkC += ((Real)0.5)*(fTMin+fTMax)*rkU;

    return fError;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float CylinderFit<float> (int,
    const Vector3<float>*, Vector3<float>&, Vector3<float>&, float&,
    float&, bool);
template WML_ITEM double CylinderFit<double> (int,
    const Vector3<double>*, Vector3<double>&, Vector3<double>&, double&,
    double&, bool);
}
//----------------------------------------------------------------------------
