// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Cap3.h"
#include "WmlDistLin3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Capsule3<Real>& rkCapsule)
{
    Real fSqrDist = SqrDistance(rkSegment,rkCapsule.Segment());
    return fSqrDist <= rkCapsule.Radius()*rkCapsule.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray3<Real>& rkRay,
    const Capsule3<Real>& rkCapsule)
{
    Real fSqrDist = SqrDistance(rkRay,rkCapsule.Segment());
    return fSqrDist <= rkCapsule.Radius()*rkCapsule.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line3<Real>& rkLine,
    const Capsule3<Real>& rkCapsule)
{
    Real fSqrDist = SqrDistance(rkLine,rkCapsule.Segment());
    return fSqrDist <= rkCapsule.Radius()*rkCapsule.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
static int Find (const Vector3<Real>& rkOrigin,
    const Vector3<Real>& rkDirection, const Capsule3<Real>& rkCapsule,
    Real afT[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kU, kV, kW = rkCapsule.Direction();
    Real fWLength = kW.Normalize();
    Vector3<Real>::GenerateOrthonormalBasis(kU,kV,kW,true);
    Vector3<Real> kD(kU.Dot(rkDirection),kV.Dot(rkDirection),
        kW.Dot(rkDirection));
    Real fDLength = kD.Normalize();

    Real fInvDLength = ((Real)1.0)/fDLength;
    Vector3<Real> kDiff = rkOrigin - rkCapsule.Origin();
    Vector3<Real> kP(kU.Dot(kDiff),kV.Dot(kDiff),kW.Dot(kDiff));
    Real fRadiusSqr = rkCapsule.Radius()*rkCapsule.Radius();

    Real fInv, fA, fB, fC, fDiscr, fRoot, fT, fTmp;

    // Is the velocity parallel to the capsule direction? (or zero)
    if ( Math<Real>::FAbs(kD.Z()) >= (Real)1.0 - Math<Real>::EPSILON
    ||   fDLength < Math<Real>::EPSILON )
    {

        Real fAxisDir = rkDirection.Dot(rkCapsule.Direction());

        fDiscr = fRadiusSqr - kP.X()*kP.X() - kP.Y()*kP.Y();
        if ( fAxisDir < 0 && fDiscr >= (Real)0.0 )
        {
            // Velocity anti-parallel to the capsule direction
            fRoot = Math<Real>::Sqrt(fDiscr);
            afT[0] = (kP.Z() + fRoot)*fInvDLength;
            afT[1] = -(fWLength - kP.Z() + fRoot)*fInvDLength;
            return 2;
        }
        else if ( fAxisDir > 0  && fDiscr >= (Real)0.0 )
        {
            // Velocity parallel to the capsule direction
            fRoot = Math<Real>::Sqrt(fDiscr);
            afT[0] = -(kP.Z() + fRoot)*fInvDLength;
            afT[1] = (fWLength - kP.Z() + fRoot)*fInvDLength;
            return 2;
        }
        else
        {
            // sphere heading wrong direction, or no velocity at all
            return 0;
        }   
    }

    // test intersection with infinite cylinder
    fA = kD.X()*kD.X() + kD.Y()*kD.Y();
    fB = kP.X()*kD.X() + kP.Y()*kD.Y();
    fC = kP.X()*kP.X() + kP.Y()*kP.Y() - fRadiusSqr;
    fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
    {
        // line does not intersect infinite cylinder
        return 0;
    }

    int iQuantity = 0;

    if ( fDiscr > (Real)0.0 )
    {
        // line intersects infinite cylinder in two places
        fRoot = Math<Real>::Sqrt(fDiscr);
        fInv = ((Real)1.0)/fA;
        fT = (-fB - fRoot)*fInv;
        fTmp = kP.Z() + fT*kD.Z();
        if ( (Real)0.0 <= fTmp && fTmp <= fWLength )
            afT[iQuantity++] = fT*fInvDLength;

        fT = (-fB + fRoot)*fInv;
        fTmp = kP.Z() + fT*kD.Z();
        if ( (Real)0.0 <= fTmp && fTmp <= fWLength )
            afT[iQuantity++] = fT*fInvDLength;

        if ( iQuantity == 2 )
        {
            // line intersects capsule wall in two places
            return 2;
        }
    }
    else
    {
        // line is tangent to infinite cylinder
        fT = -fB/fA;
        fTmp = kP.Z() + fT*kD.Z();
        if ( (Real)0.0 <= fTmp && fTmp <= fWLength )
        {
            afT[0] = fT*fInvDLength;
            return 1;
        }
    }

    // test intersection with bottom hemisphere
    // fA = 1
    fB += kP.Z()*kD.Z();
    fC += kP.Z()*kP.Z();
    fDiscr = fB*fB - fC;
    if ( fDiscr > (Real)0.0 )
    {
        fRoot = Math<Real>::Sqrt(fDiscr);
        fT = -fB - fRoot;
        fTmp = kP.Z() + fT*kD.Z();
        if ( fTmp <= (Real)0.0 )
        {
            afT[iQuantity++] = fT*fInvDLength;
            if ( iQuantity == 2 )
                return 2;
        }

        fT = -fB + fRoot;
        fTmp = kP.Z() + fT*kD.Z();
        if ( fTmp <= (Real)0.0 )
        {
            afT[iQuantity++] = fT*fInvDLength;
            if ( iQuantity == 2 )
                return 2;
        }
    }
    else if ( fDiscr == (Real)0.0 )
    {
        fT = -fB;
        fTmp = kP.Z() + fT*kD.Z();
        if ( fTmp <= (Real)0.0 )
        {
            afT[iQuantity++] = fT*fInvDLength;
            if ( iQuantity == 2 )
                return 2;
        }
    }

    // test intersection with top hemisphere
    // fA = 1
    fB -= kD.Z()*fWLength;
    fC += fWLength*(fWLength - ((Real)2.0)*kP.Z());

    fDiscr = fB*fB - fC;
    if ( fDiscr > (Real)0.0 )
    {
        fRoot = Math<Real>::Sqrt(fDiscr);
        fT = -fB - fRoot;
        fTmp = kP.Z() + fT*kD.Z();
        if ( fTmp >= fWLength )
        {
            afT[iQuantity++] = fT*fInvDLength;
            if ( iQuantity == 2 )
                return 2;
        }

        fT = -fB + fRoot;
        fTmp = kP.Z() + fT*kD.Z();
        if ( fTmp >= fWLength )
        {
            afT[iQuantity++] = fT*fInvDLength;
            if ( iQuantity == 2 )
                return 2;
        }
    }
    else if ( fDiscr == (Real)0.0 )
    {
        fT = -fB;
        fTmp = kP.Z() + fT*kD.Z();
        if ( fTmp >= fWLength )
        {
            afT[iQuantity++] = fT*fInvDLength;
            if ( iQuantity == 2 )
                return 2;
        }
    }

    return iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Capsule3<Real>& rkCapsule, int& riQuantity,
    Vector3<Real> akPoint[2], Real afT[2])
{
    riQuantity = Find(rkSegment.Origin(),rkSegment.Direction(),rkCapsule,afT);

    int iClipQuantity = 0;
    for (int i = 0; i < riQuantity; i++)
    {
        if ( (Real)0.0 <= afT[i] && afT[i] <= (Real)1.0 )
        {
            akPoint[iClipQuantity++] = rkSegment.Origin() +
                afT[i]*rkSegment.Direction();
        }
    }

    riQuantity = iClipQuantity;
    return riQuantity > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray3<Real>& rkRay,
    const Capsule3<Real>& rkCapsule, int& riQuantity,
    Vector3<Real> akPoint[2], Real afT[2])
{
    riQuantity = Find(rkRay.Origin(),rkRay.Direction(),rkCapsule,afT);

    int iClipQuantity = 0;
    for (int i = 0; i < riQuantity; i++)
    {
        if ( afT[i] >= (Real)0.0 )
        {
            akPoint[iClipQuantity++] = rkRay.Origin() +
                afT[i]*rkRay.Direction();
        }
    }

    riQuantity = iClipQuantity;
    return riQuantity > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line3<Real>& rkLine,
    const Capsule3<Real>& rkCapsule, int& riQuantity,
    Vector3<Real> akPoint[2], Real afT[2])
{
    riQuantity = Find(rkLine.Origin(),rkLine.Direction(),rkCapsule,afT);

    for (int i = 0; i < riQuantity; i++)
        akPoint[i] = rkLine.Origin() + afT[i]*rkLine.Direction();

    return riQuantity > 0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Capsule3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Ray3<float>&, const Capsule3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Line3<float>&, const Capsule3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Capsule3<float>&, int&,
    Vector3<float>[2], float[2]);
template WML_ITEM bool FindIntersection<float> (
    const Ray3<float>&, const Capsule3<float>&, int&,
    Vector3<float>[2], float[2]);
template WML_ITEM bool FindIntersection<float> (
    const Line3<float>&, const Capsule3<float>&, int&,
    Vector3<float>[2], float[2]);

template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Capsule3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Ray3<double>&, const Capsule3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Line3<double>&, const Capsule3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Capsule3<double>&, int&,
    Vector3<double>[2], double[2]);
template WML_ITEM bool FindIntersection<double> (
    const Ray3<double>&, const Capsule3<double>&, int&,
    Vector3<double>[2], double[2]);
template WML_ITEM bool FindIntersection<double> (
    const Line3<double>&, const Capsule3<double>&, int&,
    Vector3<double>[2], double[2]);
}
//----------------------------------------------------------------------------
