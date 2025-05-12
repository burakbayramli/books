// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprLineFit3.h"
#include "WmlContCapsule3.h"
#include "WmlDistVec3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Capsule3<Real> Wml::ContCapsule (int iQuantity, const Vector3<Real>* akPoint)
{
    Capsule3<Real> kCapsule;

    Line3<Real> kLine;
    OrthogonalLineFit<Real>(iQuantity,akPoint,kLine.Origin(),
        kLine.Direction());

    Real fMaxRadiusSqr = (Real)0.0;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        Real fRadiusSqr = SqrDistance<Real>(akPoint[i],kLine);
        if ( fRadiusSqr > fMaxRadiusSqr )
            fMaxRadiusSqr = fRadiusSqr;
    }

    Vector3<Real> kU, kV, kW = kLine.Direction();
    Vector3<Real>::GenerateOrthonormalBasis(kU,kV,kW,true);

    Real fMin = Math<Real>::MAX_REAL, fMax = -fMin;
    for (i = 0; i < iQuantity; i++)
    {
        Vector3<Real> kDiff = akPoint[i] - kLine.Origin();
        Real fU = kU.Dot(kDiff);
        Real fV = kV.Dot(kDiff);
        Real fW = kW.Dot(kDiff);
        Real fDiscr = fMaxRadiusSqr - (fU*fU + fV*fV);
        Real fRadical = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));

        Real fTest = fW + fRadical;
        if ( fTest < fMin )
            fMin = fTest;

        fTest = fW - fRadical;
        if ( fTest > fMax )
            fMax = fTest;
    }

    if ( fMin < fMax )
    {
        kCapsule.Origin() = kLine.Origin() + fMin*kLine.Direction();
        kCapsule.Direction() = (fMax-fMin)*kLine.Direction();
    }
    else
    {
        // enclosing capsule is really a sphere
        kCapsule.Origin() = kLine.Origin() +
            (((Real)0.5)*(fMin+fMax))*kLine.Direction();
        kCapsule.Direction() = Vector3<Real>::ZERO;
    }

    kCapsule.Radius() = Math<Real>::Sqrt(fMaxRadiusSqr);

    return kCapsule;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::ContCapsule (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Capsule3<Real>& rkCapsule)
{
    Line3<Real> kLine;
    if ( !OrthogonalLineFit<Real>(iQuantity,akPoint,abValid,kLine.Origin(),
         kLine.Direction()) )
    {
        return false;
    }

    Real fMaxRadiusSqr = (Real)0.0;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            Real fRadiusSqr = SqrDistance<Real>(akPoint[i],kLine);
            if ( fRadiusSqr > fMaxRadiusSqr )
                fMaxRadiusSqr = fRadiusSqr;
        }
    }

    Vector3<Real> kU, kV, kW = kLine.Direction();
    Vector3<Real>::GenerateOrthonormalBasis(kU,kV,kW,true);

    Real fMin = Math<Real>::MAX_REAL, fMax = -fMin;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            Vector3<Real> kDiff = akPoint[i] - kLine.Origin();
            Real fU = kU.Dot(kDiff);
            Real fV = kV.Dot(kDiff);
            Real fW = kW.Dot(kDiff);
            Real fDiscr = fMaxRadiusSqr - (fU*fU + fV*fV);
            Real fRadical = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));

            Real fTest = fW + fRadical;
            if ( fTest < fMin )
                fMin = fTest;

            fTest = fW - fRadical;
            if ( fTest > fMax )
                fMax = fTest;
        }
    }

    if ( fMin < fMax )
    {
        rkCapsule.Origin() = kLine.Origin() + fMin*kLine.Direction();
        rkCapsule.Direction() = (fMax-fMin)*kLine.Direction();
    }
    else
    {
        // enclosing capsule is really a sphere
        rkCapsule.Origin() = kLine.Origin() +
            (((Real)0.5)*(fMin+fMax))*kLine.Direction();
        rkCapsule.Direction() = Vector3<Real>::ZERO;
    }

    rkCapsule.Radius() = Math<Real>::Sqrt(fMaxRadiusSqr);

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::InCapsule (const Vector3<Real>& rkPoint,
    const Capsule3<Real>& rkCapsule, Real fEpsilon)
{
    Real fRSqr = rkCapsule.Radius()*rkCapsule.Radius();
    Real fSqrDist = SqrDistance<Real>(rkPoint,rkCapsule.Segment());
    return fSqrDist <= fRSqr + fEpsilon;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::InCapsule (const Sphere3<Real>& rkSphere,
    const Capsule3<Real>& rkCapsule)
{
    Real fRDiff = rkCapsule.Radius() - rkSphere.Radius();
    if ( fRDiff >= (Real)0.0 )
    {
        return SqrDistance<Real>(rkSphere.Center(),rkCapsule.Segment()) <=
            fRDiff*fRDiff;
    }
    else
    {
        return false;
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::InCapsule (const Capsule3<Real>& rkTestCapsule,
    const Capsule3<Real>& rkCapsule)
{
    Sphere3<Real> kSphere0, kSphere1;
    kSphere0.Center() = rkTestCapsule.Origin();
    kSphere0.Radius() = rkTestCapsule.Radius();
    kSphere1.Center() = rkTestCapsule.Origin() + rkTestCapsule.Direction();
    kSphere1.Radius() = rkTestCapsule.Radius();

    return InCapsule<Real>(kSphere0,rkCapsule)
        && InCapsule<Real>(kSphere1,rkCapsule);
}
//----------------------------------------------------------------------------
template <class Real>
Capsule3<Real> Wml::MergeCapsules (const Capsule3<Real>& rkCapsule0,
    const Capsule3<Real>& rkCapsule1)
{
    if ( InCapsule<Real>(rkCapsule0,rkCapsule1) )
        return rkCapsule1;

    if ( InCapsule<Real>(rkCapsule1,rkCapsule0) )
        return rkCapsule0;

    const Vector3<Real>& rkP0 = rkCapsule0.Origin();
    const Vector3<Real>& rkP1 = rkCapsule1.Origin();
    const Vector3<Real>& rkD0 = rkCapsule0.Direction();
    const Vector3<Real>& rkD1 = rkCapsule1.Direction();

    // axis of final capsule
    Line3<Real> kLine;

    // axis center is average of input axis centers
    kLine.Origin() = ((Real)0.5)*(rkP0 + rkP1) + ((Real)0.25)*(rkD0 + rkD1);

    // axis unit direction is average of input axis unit directions
    Vector3<Real> kDirection0 = rkD0;
    Vector3<Real> kDirection1 = rkD1;
    kDirection0.Normalize();
    kDirection1.Normalize();
    Vector3<Real>& rkLineDir = kLine.Direction();
    if ( kDirection0.Dot(kDirection1) >= (Real)0.0 )
        rkLineDir = kDirection0 + kDirection1;
    else
        rkLineDir = kDirection0 - kDirection1;
    rkLineDir.Normalize();

    // Cylinder with axis 'kLine' must contain the spheres centered at the
    // end points of the input capsules.
    Real fRadius = Distance<Real>(rkP0,kLine) + rkCapsule0.Radius();

    Real fDist = Distance<Real>(rkP1,kLine) + rkCapsule1.Radius();
    if ( fDist > fRadius )
        fRadius = fDist;

    Vector3<Real> kP0D0 = rkP0 + rkD0;
    fDist = Distance<Real>(kP0D0,kLine) + rkCapsule0.Radius();
    if ( fDist > fRadius )
        fRadius = fDist;

    Vector3<Real> kP1D1 = rkP1 + rkD1;
    fDist = Distance<Real>(kP1D1,kLine) + rkCapsule1.Radius();
    if ( fDist > fRadius )
        fRadius = fDist;

    // process sphere <P0,r0>
    Real fRDiff = fRadius - rkCapsule0.Radius();
    Real fRDiffSqr = fRDiff*fRDiff;
    Vector3<Real> kDiff = kLine.Origin() - rkP0;
    Real fK0 = kDiff.SquaredLength() - fRDiffSqr;
    Real fK1 = kDiff.Dot(kLine.Direction());
    Real fDiscr = fK1*fK1 - fK0;  // assert:  K1*K1-K0 >= 0
    Real fRoot = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
    Real fTPos = fK1 - fRoot;
    Real fTNeg = fK1 + fRoot;
    Real fTmp;

    // process sphere <P0+D0,r0>
    kDiff = kLine.Origin() - kP0D0;
    fK0 = kDiff.SquaredLength() - fRDiffSqr;
    fK1 = kDiff.Dot(kLine.Direction());
    fDiscr = fK1*fK1 - fK0;  // assert:  K1*K1-K0 >= 0
    fRoot = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
    fTmp = fK1 - fRoot;
    if ( fTmp > fTPos )
        fTPos = fTmp;
    fTmp = fK1 + fRoot;
    if ( fTmp < fTNeg )
        fTNeg = fTmp;

    // process sphere <P1,r1>
    fRDiff = fRadius - rkCapsule1.Radius();
    fRDiffSqr = fRDiff*fRDiff;
    kDiff = kLine.Origin() - rkP1;
    fK0 = kDiff.SquaredLength() - fRDiffSqr;
    fK1 = kDiff.Dot(kLine.Direction());
    fDiscr = fK1*fK1 - fK0;  // assert:  K1*K1-K0 >= 0
    fRoot = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
    fTmp = fK1 - fRoot;
    if ( fTmp > fTPos )
        fTPos = fTmp;
    fTmp = fK1 + fRoot;
    if ( fTmp < fTNeg )
        fTNeg = fTmp;

    // process sphere <P1+D1,r1>
    kDiff = kLine.Origin() - kP1D1;
    fK0 = kDiff.SquaredLength() - fRDiffSqr;
    fK1 = kDiff.Dot(kLine.Direction());
    fDiscr = fK1*fK1 - fK0;  // assert:  K1*K1-K0 >= 0
    fRoot = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
    fTmp = fK1 - fRoot;
    if ( fTmp > fTPos )
        fTPos = fTmp;
    fTmp = fK1 + fRoot;
    if ( fTmp < fTNeg )
        fTNeg = fTmp;

    if ( fTPos < fTNeg )
    {
        Real fAverage = ((Real)0.5)*(fTPos + fTNeg);
        fTPos = fAverage;
        fTNeg = fAverage;
    }

    Capsule3<Real> kCapsule;
    kCapsule.Radius() = fRadius;
    kCapsule.Origin() = kLine.Origin() + fTNeg*kLine.Direction();
    kCapsule.Direction() = (fTPos - fTNeg)*kLine.Direction();
    return kCapsule;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Capsule3<float> ContCapsule<float> (int,
    const Vector3<float>*);
template WML_ITEM bool ContCapsule<float> (int, const Vector3<float>*,
    const bool*, Capsule3<float>&);
template WML_ITEM bool InCapsule<float> (const Vector3<float>&,
    const Capsule3<float>&, float);
template WML_ITEM bool InCapsule<float> (const Sphere3<float>&,
    const Capsule3<float>&);
template WML_ITEM bool InCapsule<float> (const Capsule3<float>&,
    const Capsule3<float>&);
template WML_ITEM Capsule3<float> MergeCapsules<float>
    (const Capsule3<float>&, const Capsule3<float>&);

template WML_ITEM Capsule3<double> ContCapsule<double> (int,
    const Vector3<double>*);
template WML_ITEM bool ContCapsule<double> (int, const Vector3<double>*,
    const bool*, Capsule3<double>&);
template WML_ITEM bool InCapsule<double> (const Vector3<double>&,
    const Capsule3<double>&, double);
template WML_ITEM bool InCapsule<double> (const Sphere3<double>&,
    const Capsule3<double>&);
template WML_ITEM bool InCapsule<double> (const Capsule3<double>&,
    const Capsule3<double>&);
template WML_ITEM Capsule3<double> MergeCapsules<double>
    (const Capsule3<double>&, const Capsule3<double>&);
}
//----------------------------------------------------------------------------
