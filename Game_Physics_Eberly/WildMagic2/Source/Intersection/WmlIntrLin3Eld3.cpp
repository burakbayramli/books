// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Eld3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Ellipsoid3<Real>& rkEllipsoid3)
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkSegment.Origin() - rkEllipsoid3.Center();
    Vector3<Real> kMatDir = rkEllipsoid3.A()*rkSegment.Direction();
    Vector3<Real> kMatDiff = rkEllipsoid3.A()*kDiff;
    Real fA = rkSegment.Direction().Dot(kMatDir);
    Real fB = rkSegment.Direction().Dot(kMatDiff);
    Real fC = kDiff.Dot(kMatDiff) - (Real)1.0;

    // no intersection if Q(t) has no real roots
    Real fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
        return false;

    // test if line origin is inside ellipsoid
    if ( fC <= (Real)0.0 )
        return true;

    // At this point fC > 0 and Q(t) has real roots.  No intersection if
    // Q'(0) >= 0.
    if ( fB >= (Real)0.0 )
        return false;

    // Need to determine if Q(t) has real roots on [0,1].  Effectively is
    // a test for sign changes of Sturm polynomials.
    Real fSum = fA + fB;
    if ( fSum >= (Real)0.0 )
        return true;

    return fSum + fB + fC <= (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray3<Real>& rkRay,
    const Ellipsoid3<Real>& rkEllipsoid3)
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkRay.Origin() - rkEllipsoid3.Center();
    Vector3<Real> kMatDir = rkEllipsoid3.A()*rkRay.Direction();
    Vector3<Real> kMatDiff = rkEllipsoid3.A()*kDiff;
    Real fA = rkRay.Direction().Dot(kMatDir);  // fA > 0 is necessary
    Real fB = rkRay.Direction().Dot(kMatDiff);
    Real fC = kDiff.Dot(kMatDiff) - (Real)1.0;

    // no intersection if Q(t) has no real roots
    Real fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
        return false;

    // test if ray origin is inside ellipsoid
    if ( fC <= (Real)0.0 )
        return true;

    // At this point, fC > 0 and Q(t) has real roots.  Intersection occurs
    // if Q'(0) < 0.
    return fB < (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line3<Real>& rkLine,
    const Ellipsoid3<Real>& rkEllipsoid3)
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkLine.Origin() - rkEllipsoid3.Center();
    Vector3<Real> kMatDir = rkEllipsoid3.A()*rkLine.Direction();
    Vector3<Real> kMatDiff = rkEllipsoid3.A()*kDiff;
    Real fA = rkLine.Direction().Dot(kMatDir);
    Real fB = rkLine.Direction().Dot(kMatDiff);
    Real fC = kDiff.Dot(kMatDiff) - (Real)1.0;

    // intersection occurs if Q(t) has real roots
    Real fDiscr = fB*fB - fA*fC;
    return fDiscr >= (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Ellipsoid3<Real>& rkEllipsoid3, int& riQuantity,
    Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkSegment.Origin() - rkEllipsoid3.Center();
    Vector3<Real> kMatDir = rkEllipsoid3.A()*rkSegment.Direction();
    Vector3<Real> kMatDiff = rkEllipsoid3.A()*kDiff;
    Real fA = rkSegment.Direction().Dot(kMatDir);
    Real fB = rkSegment.Direction().Dot(kMatDiff);
    Real fC = kDiff.Dot(kMatDiff) - (Real)1.0;

    // no intersection if Q(t) has no real roots
    Real afT[2];
    Real fDiscr = fB*fB - fA*fC;

    if ( fDiscr < (Real)0.0 )
    {
        riQuantity = 0;
        return false;
    }
    else if ( fDiscr > (Real)0.0 )
    {
        Real fRoot = Math<Real>::Sqrt(fDiscr);
        Real fInvA = ((Real)1.0)/fA;
        afT[0] = (-fB - fRoot)*fInvA;
        afT[1] = (-fB + fRoot)*fInvA;

        // assert: t0 < t1 since A > 0

        if ( afT[0] > (Real)1.0 || afT[1] < (Real)0.0 )
        {
            riQuantity = 0;
            return false;
        }
        else if ( afT[0] >= (Real)0.0 )
        {
            if ( afT[1] > (Real)1.0 )
            {
                riQuantity = 1;
                akPoint[0] = rkSegment.Origin()+afT[0]*rkSegment.Direction();
                return true;
            }
            else
            {
                riQuantity = 2;
                akPoint[0] = rkSegment.Origin()+afT[0]*rkSegment.Direction();
                akPoint[1] = rkSegment.Origin()+afT[1]*rkSegment.Direction();
                return true;
            }
        }
        else  // afT[1] >= 0
        {
            riQuantity = 1;
            akPoint[0] = rkSegment.Origin()+afT[1]*rkSegment.Direction();
            return true;
        }
    }
    else
    {
        afT[0] = -fB/fA;
        if ( (Real)0.0 <= afT[0] && afT[0] <= (Real)1.0 )
        {
            riQuantity = 1;
            akPoint[0] = rkSegment.Origin()+afT[0]*rkSegment.Direction();
            return true;
        }
        else
        {
            riQuantity = 0;
            return false;
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray3<Real>& rkRay,
    const Ellipsoid3<Real>& rkEllipsoid3, int& riQuantity,
    Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkRay.Origin() - rkEllipsoid3.Center();
    Vector3<Real> kMatDir = rkEllipsoid3.A()*rkRay.Direction();
    Vector3<Real> kMatDiff = rkEllipsoid3.A()*kDiff;
    Real fA = rkRay.Direction().Dot(kMatDir);  // fA > 0 is necessary
    Real fB = rkRay.Direction().Dot(kMatDiff);
    Real fC = kDiff.Dot(kMatDiff) - (Real)1.0;

    Real afT[2];
    Real fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
    {
        riQuantity = 0;
        return false;
    }
    else if ( fDiscr > (Real)0.0 )
    {
        Real fRoot = Math<Real>::Sqrt(fDiscr);
        Real fInvA = ((Real)1.0)/fA;
        afT[0] = (-fB - fRoot)*fInvA;
        afT[1] = (-fB + fRoot)*fInvA;

        if ( afT[0] >= (Real)0.0 )
        {
            riQuantity = 2;
            akPoint[0] = rkRay.Origin() + afT[0]*rkRay.Direction();
            akPoint[1] = rkRay.Origin() + afT[1]*rkRay.Direction();
            return true;
        }
        else if ( afT[1] >= (Real)0.0 )
        {
            riQuantity = 1;
            akPoint[0] = rkRay.Origin() + afT[1]*rkRay.Direction();
            return true;
        }
        else
        {
            riQuantity = 0;
            return false;
        }
    }
    else
    {
        afT[0] = -fB/fA;
        if ( afT[0] >= (Real)0.0 )
        {
            riQuantity = 1;
            akPoint[0] = rkRay.Origin() + afT[0]*rkRay.Direction();
            return true;
        }
        else
        {
            riQuantity = 0;
            return false;
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line3<Real>& rkLine,
    const Ellipsoid3<Real>& rkEllipsoid3, int& riQuantity,
    Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkLine.Origin() - rkEllipsoid3.Center();
    Vector3<Real> kMatDir = rkEllipsoid3.A()*rkLine.Direction();
    Vector3<Real> kMatDiff = rkEllipsoid3.A()*kDiff;
    Real fA = rkLine.Direction().Dot(kMatDir);
    Real fB = rkLine.Direction().Dot(kMatDiff);
    Real fC = kDiff.Dot(kMatDiff) - (Real)1.0;

    Real afT[2];
    Real fDiscr = fB*fB - fA*fC;
    if ( fDiscr < (Real)0.0 )
    {
        riQuantity = 0;
        return false;
    }
    else if ( fDiscr > (Real)0.0 )
    {
        Real fRoot = Math<Real>::Sqrt(fDiscr);
        Real fInvA = ((Real)1.0)/fA;
        riQuantity = 2;
        afT[0] = (-fB - fRoot)*fInvA;
        afT[1] = (-fB + fRoot)*fInvA;
        akPoint[0] = rkLine.Origin() + afT[0]*rkLine.Direction();
        akPoint[1] = rkLine.Origin() + afT[1]*rkLine.Direction();
        return true;
    }
    else
    {
        riQuantity = 1;
        afT[0] = -fB/fA;
        akPoint[0] = rkLine.Origin() + afT[0]*rkLine.Direction();
        return true;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Ellipsoid3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Ray3<float>&, const Ellipsoid3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Line3<float>&, const Ellipsoid3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Ellipsoid3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Ray3<float>&, const Ellipsoid3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Line3<float>&, const Ellipsoid3<float>&, int&,
    Vector3<float>[2]);

template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Ellipsoid3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Ray3<double>&, const Ellipsoid3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Line3<double>&, const Ellipsoid3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Ellipsoid3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Ray3<double>&, const Ellipsoid3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Line3<double>&, const Ellipsoid3<double>&, int&,
    Vector3<double>[2]);
}
//----------------------------------------------------------------------------
