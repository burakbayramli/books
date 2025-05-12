// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistLin3Cir3.h"
#include "WmlDistVec3Cir3.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Line3<Real>& rkLine,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkLineClosest,
    Vector3<Real>* pkCircleClosest)
{
    Vector3<Real> kDiff = rkLine.Origin() - rkCircle.Center();
    Real fDSqrLen = kDiff.SquaredLength();
    Real fMdM = rkLine.Direction().SquaredLength();
    Real fDdM = kDiff.Dot(rkLine.Direction());
    Real fNdM = rkCircle.N().Dot(rkLine.Direction());
    Real fDdN = kDiff.Dot(rkCircle.N());

    Real fA0 = fDdM;
    Real fA1 = fMdM;
    Real fB0 = fDdM - fNdM*fDdN;
    Real fB1 = fMdM - fNdM*fNdM;
    Real fC0 = fDSqrLen - fDdN*fDdN;
    Real fC1 = fB0;
    Real fC2 = fB1;
    Real fRSqr = rkCircle.Radius()*rkCircle.Radius();

    Real fA0Sqr = fA0*fA0;
    Real fA1Sqr = fA1*fA1;
    Real fTwoA0A1 = ((Real)2.0)*fA0*fA1;
    Real fB0Sqr = fB0*fB0;
    Real fB1Sqr = fB1*fB1;
    Real fTwoB0B1 = ((Real)2.0)*fB0*fB1;
    Real fTwoC1 = ((Real)2.0)*fC1;

    // The minimum point B+t*M occurs when t is a root of the quartic
    // equation whose coefficients are defined below.
    Polynomial1<Real> kPoly(4);
    kPoly[0] = fA0Sqr*fC0 - fB0Sqr*fRSqr;
    kPoly[1] = fTwoA0A1*fC0 + fA0Sqr*fTwoC1 - fTwoB0B1*fRSqr;
    kPoly[2] = fA1Sqr*fC0 + fTwoA0A1*fTwoC1 + fA0Sqr*fC2 - fB1Sqr*fRSqr;
    kPoly[3] = fA1Sqr*fTwoC1 + fTwoA0A1*fC2;
    kPoly[4] = fA1Sqr*fC2;

    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindB(kPoly,6);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    Real fMinSqrDist = Math<Real>::MAX_REAL;
    for (int i = 0; i < iCount; i++)
    {
        // compute distance from P(t) to circle
        Vector3<Real> kP = rkLine.Origin() + afRoot[i]*rkLine.Direction();
        Vector3<Real> kCircleClosest;
        Real fSqrDist = SqrDistance(kP,rkCircle,&kCircleClosest);
        if ( fSqrDist < fMinSqrDist )
        {
            fMinSqrDist = fSqrDist;
            if ( pkLineClosest )
                *pkLineClosest = kP;
            if ( pkCircleClosest )
                *pkCircleClosest = kCircleClosest;
        }
    }

    return fMinSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Line3<Real>& rkLine, const Circle3<Real>& rkCircle,
    Vector3<Real>* pkLineClosest, Vector3<Real>* pkCircleClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkLine,rkCircle,pkLineClosest,
        pkCircleClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Line3<float>&,
    const Circle3<float>&, Vector3<float>*, Vector3<float>*);
template WML_ITEM float Distance<float> (const Line3<float>&,
    const Circle3<float>&, Vector3<float>*, Vector3<float>*);

template WML_ITEM double SqrDistance<double> (const Line3<double>&,
    const Circle3<double>&, Vector3<double>*, Vector3<double>*);
template WML_ITEM double Distance<double> (const Line3<double>&,
    const Circle3<double>&, Vector3<double>*, Vector3<double>*);
}
//----------------------------------------------------------------------------
