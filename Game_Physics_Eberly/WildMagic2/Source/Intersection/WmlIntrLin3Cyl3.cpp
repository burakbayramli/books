// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Cyl3.h"
#include "WmlDistLin3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static int Find (const Vector3<Real>& rkOrigin,
    const Vector3<Real>& rkDirection, const Cylinder3<Real>& rkCylinder,
    Real afT[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kU, kV, kW = rkCylinder.Direction();
    Vector3<Real>::GenerateOrthonormalBasis(kU,kV,kW,true);
    Vector3<Real> kD(kU.Dot(rkDirection),kV.Dot(rkDirection),
        kW.Dot(rkDirection));
    Real fDLength = kD.Normalize();
    Real fInvDLength = ((Real)1.0)/fDLength;
    Vector3<Real> kDiff = rkOrigin - rkCylinder.Center();
    Vector3<Real> kP(kU.Dot(kDiff),kV.Dot(kDiff),kW.Dot(kDiff));
    Real fHalfHeight = ((Real)0.5)*rkCylinder.Height();
    Real fRadiusSqr = rkCylinder.Radius()*rkCylinder.Radius();

    Real fInv, fA, fB, fC, fDiscr, fRoot, fT, fT0, fT1, fTmp0, fTmp1;

    if ( Math<Real>::FAbs(kD.Z()) >= (Real)1.0 - Math<Real>::EPSILON )
    {
        // line is parallel to cylinder axis
        if ( kP.X()*kP.X()+kP.Y()*kP.Y() <= fRadiusSqr )
        {
            fTmp0 = fInvDLength/kD.Z();
            afT[0] = (+fHalfHeight - kP.Z())*fTmp0;
            afT[1] = (-fHalfHeight - kP.Z())*fTmp0;
            return 2;
        }
        else
        {
            return 0;
        }
    }

    if ( Math<Real>::FAbs(kD.Z()) <= Math<Real>::EPSILON )
    {
        // line is perpendicular to axis of cylinder
        if ( Math<Real>::FAbs(kP.Z()) > fHalfHeight )
        {
            // line is outside the planar caps of cylinder
            return 0;
        }

        fA = kD.X()*kD.X() + kD.Y()*kD.Y();
        fB = kP.X()*kD.X() + kP.Y()*kD.Y();
        fC = kP.X()*kP.X() + kP.Y()*kP.Y() - fRadiusSqr;
        fDiscr = fB*fB - fA*fC;
        if ( fDiscr < (Real)0.0 )
        {
            // line does not intersect cylinder wall
            return 0;
        }
        else if ( fDiscr > (Real)0.0 )
        {
            fRoot = Math<Real>::Sqrt(fDiscr);
            fTmp0 = fInvDLength/fA;
            afT[0] = (-fB - fRoot)*fTmp0;
            afT[1] = (-fB + fRoot)*fTmp0;
            return 2;
        }
        else
        {
            afT[0] = -fB*fInvDLength/fA;
            return 1;
        }
    }

    // test plane intersections first
    int iQuantity = 0;
    fInv = ((Real)1.0)/kD.Z();
    fT0 = (+fHalfHeight - kP.Z())*fInv;
    fTmp0 = kP.X() + fT0*kD.X();
    fTmp1 = kP.Y() + fT0*kD.Y();
    if ( fTmp0*fTmp0 + fTmp1*fTmp1 <= fRadiusSqr )
        afT[iQuantity++] = fT0*fInvDLength;

    fT1 = (-fHalfHeight - kP.Z())*fInv;
    fTmp0 = kP.X() + fT1*kD.X();
    fTmp1 = kP.Y() + fT1*kD.Y();
    if ( fTmp0*fTmp0 + fTmp1*fTmp1 <= fRadiusSqr )
        afT[iQuantity++] = fT1*fInvDLength;

    if ( iQuantity == 2 )
    {
        // line intersects both top and bottom
        return 2;
    }

    // If iQuantity == 1, then line must intersect cylinder wall
    // somewhere between caps in a single point.  This case is detected
    // in the following code that tests for intersection between line and
    // cylinder wall.

    fA = kD.X()*kD.X() + kD.Y()*kD.Y();
    fB = kP.X()*kD.X() + kP.Y()*kD.Y();
    fC = kP.X()*kP.X() + kP.Y()*kP.Y() - fRadiusSqr;
    fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
    {
        // line does not intersect cylinder wall
        assert( iQuantity == 0 );
        return 0;
    }
    else if ( fDiscr > (Real)0.0 )
    {
        fRoot = Math<Real>::Sqrt(fDiscr);
        fInv = ((Real)1.0)/fA;
        fT = (-fB - fRoot)*fInv;
        if ( fT0 <= fT1 )
        {
            if ( fT0 <= fT && fT <= fT1 )
                afT[iQuantity++] = fT*fInvDLength;
        }
        else
        {
            if ( fT1 <= fT && fT <= fT0 )
                afT[iQuantity++] = fT*fInvDLength;
        }

        if ( iQuantity == 2 )
        {
            // Line intersects one of top/bottom of cylinder and once on
            // cylinder wall.
            return 2;
        }

        fT = (-fB + fRoot)*fInv;
        if ( fT0 <= fT1 )
        {
            if ( fT0 <= fT && fT <= fT1 )
                afT[iQuantity++] = fT*fInvDLength;
        }
        else
        {
            if ( fT1 <= fT && fT <= fT0 )
                afT[iQuantity++] = fT*fInvDLength;
        }
    }
    else
    {
        fT = -fB/fA;
        if ( fT0 <= fT1 )
        {
            if ( fT0 <= fT && fT <= fT1 )
                afT[iQuantity++] = fT*fInvDLength;
        }
        else
        {
            if ( fT1 <= fT && fT <= fT0 )
                afT[iQuantity++] = fT*fInvDLength;
        }
    }

    return iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
static int FindHollow (const Vector3<Real>& rkOrigin,
    const Vector3<Real>& rkDirection, const Cylinder3<Real>& rkCylinder,
    Real afT[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kU, kV, kW = rkCylinder.Direction();
    Vector3<Real>::GenerateOrthonormalBasis(kU,kV,kW,true);
    Vector3<Real> kD(kU.Dot(rkDirection),kV.Dot(rkDirection),
        kW.Dot(rkDirection));
    Real fDLength = kD.Normalize();
    Real fInvDLength = ((Real)1.0)/fDLength;
    Vector3<Real> kDiff = rkOrigin - rkCylinder.Center();
    Vector3<Real> kP(kU.Dot(kDiff),kV.Dot(kDiff),kW.Dot(kDiff));
    Real fHalfHeight = ((Real)0.5)*rkCylinder.Height();
    Real fRadiusSqr = rkCylinder.Radius()*rkCylinder.Radius();

    Real fA, fB, fC, fDiscr, fRoot, fT;

    if ( Math<Real>::FAbs(kD.Z()) >= (Real)1.0 - Math<Real>::EPSILON )
    {
        // line is parallel to cylinder axis
        if ( kP.X()*kP.X() + kP.Y()*kP.Y() != fRadiusSqr )
        {
            // line inside or outside the cylinder
            return 0;
        }
        else
        {
            // The line intersects the cylinder along a line segment on the
            // cylinder wall.  Compute the line parameters for the end points
            // of the segment.
            fT = fInvDLength/kD.Z();
            afT[0] = (+fHalfHeight - kP.Z())*fT;
            afT[1] = (-fHalfHeight - kP.Z())*fT;
            return 2;
        }
    }

    if ( Math<Real>::FAbs(kD.Z()) <= Math<Real>::EPSILON )
    {
        // line is perpendicular to axis of cylinder
        if ( Math<Real>::FAbs(kP.Z()) > fHalfHeight )
        {
            // line is outside the planar caps of cylinder
            return 0;
        }

        fA = kD.X()*kD.X() + kD.Y()*kD.Y();
        fB = kP.X()*kD.X() + kP.Y()*kD.Y();
        fC = kP.X()*kP.X() + kP.Y()*kP.Y() - fRadiusSqr;
        fDiscr = fB*fB - fA*fC;
        if ( fDiscr < (Real)0.0 )
        {
            // line does not intersect cylinder wall
            return 0;
        }
        else if ( fDiscr > (Real)0.0 )
        {
            fRoot = Math<Real>::Sqrt(fDiscr);
            fT = fInvDLength/fA;
            afT[0] = (-fB - fRoot)*fT;
            afT[1] = (-fB + fRoot)*fT;
            return 2;
        }
        else
        {
            afT[0] = -fB*fInvDLength/fA;
            return 1;
        }
    }

    // Clip line to a segment that is between the two planes of the cylinder
    // end disks.
    Real fInv = ((Real)1.0)/kD.Z();
    Real fT0 = (+fHalfHeight - kP.Z())*fInv;
    Real fT1 = (-fHalfHeight - kP.Z())*fInv;

    // Compute the intersections (if any) between the line and the infinite
    // cylinder.
    int iQuantity = 0;
    fA = kD.X()*kD.X() + kD.Y()*kD.Y();
    fB = kP.X()*kD.X() + kP.Y()*kD.Y();
    fC = kP.X()*kP.X() + kP.Y()*kP.Y() - fRadiusSqr;
    fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
    {
        // line does not intersect infinite cylinder
        return 0;
    }
    else if ( fDiscr > (Real)0.0 )
    {
        // Line intersects infinite cylinder in two points.  Only save the
        // line-parameters of intersection if those parameters are within the
        // clipped line.
        fRoot = Math<Real>::Sqrt(fDiscr);
        fInv = ((Real)1.0)/fA;

        fT = (-fB - fRoot)*fInv;
        if ( fT0 <= fT1 )
        {
            if ( fT0 <= fT && fT <= fT1 )
                afT[iQuantity++] = fT*fInvDLength;
        }
        else
        {
            if ( fT1 <= fT && fT <= fT0 )
                afT[iQuantity++] = fT*fInvDLength;
        }

        fT = (-fB + fRoot)*fInv;
        if ( fT0 <= fT1 )
        {
            if ( fT0 <= fT && fT <= fT1 )
                afT[iQuantity++] = fT*fInvDLength;
        }
        else
        {
            if ( fT1 <= fT && fT <= fT0 )
                afT[iQuantity++] = fT*fInvDLength;
        }
    }
    else
    {
        // Line intersects infinite cylinder in one point (line is tangent to
        // cylinder).  Only save the line-parameter of intersection if that
        // parameter is within the clipped line.
        fT = -fB/fA;
        if ( fT0 <= fT1 )
        {
            if ( fT0 <= fT && fT <= fT1 )
                afT[iQuantity++] = fT*fInvDLength;
        }
        else
        {
            if ( fT1 <= fT && fT <= fT0 )
                afT[iQuantity++] = fT*fInvDLength;
        }
    }

    return iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Cylinder3<Real>& rkCylinder, int& riQuantity,
    Vector3<Real> akPoint[2])
{
    Real afT[2];

    if ( rkCylinder.Capped() )
    {
        riQuantity = Find(rkSegment.Origin(),rkSegment.Direction(),
            rkCylinder,afT);
    }
    else
    {
        riQuantity = FindHollow(rkSegment.Origin(),rkSegment.Direction(),
            rkCylinder,afT);
    }

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
    const Cylinder3<Real>& rkCylinder, int& riQuantity,
    Vector3<Real> akPoint[2])
{
    Real afT[2];

    if ( rkCylinder.Capped() )
    {
        riQuantity = Find(rkRay.Origin(),rkRay.Direction(),rkCylinder,afT);
    }
    else
    {
        riQuantity = FindHollow(rkRay.Origin(),rkRay.Direction(),
            rkCylinder,afT);
    }

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
    const Cylinder3<Real>& rkCylinder, int& riQuantity,
    Vector3<Real> akPoint[2])
{
    Real afT[2];

    if ( rkCylinder.Capped() )
    {
        riQuantity = Find(rkLine.Origin(),rkLine.Direction(),rkCylinder,afT);
    }
    else
    {
        riQuantity = FindHollow(rkLine.Origin(),rkLine.Direction(),
            rkCylinder,afT);
    }

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
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Cylinder3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Ray3<float>&, const Cylinder3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Line3<float>&, const Cylinder3<float>&, int&,
    Vector3<float>[2]);

template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Cylinder3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Ray3<double>&, const Cylinder3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Line3<double>&, const Cylinder3<double>&, int&,
    Vector3<double>[2]);
}
//----------------------------------------------------------------------------
