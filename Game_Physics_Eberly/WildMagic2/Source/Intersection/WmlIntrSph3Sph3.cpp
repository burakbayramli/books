// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// Intersection of sphere with sphere.
//
// Two spheres are |X-C0|^2 = R0^2 and |X-C1|^2 = R1^2.  If the centers are
// identical (C1=C0), then the spheres intersect if and only if R0=R1.  If
// there is a circle of intersection, the plane of intersection must have
// normal N = C1-C0.  Let U and V be unit vectors such that Dot(N,U) = 0,
// Dot(N,V) = 0, Dot(U,V) = 0.  The circle of intersection is given by
//
//   X = C+R*(cos(A)*U+sin(A)*V)
//
// where C=C0+t*N with 0 <= t <= 1 (t is fixed) and 0 <= A < 2*pi
// (A is variable).
//
// Plug into first sphere equation to get
//
//   R0^2 = t^2*|N|^2 + R^2
//
// Plug into second sphere equation to get
//
//   R1^2 = (t-1)^2*|N|^2 + R^2
//
// Subtract these two equations and solve for t to get
//
//   t = 0.5*(1+(R0^2-R1^2)/|N|^2)
//
// It is necessary that 0 <= t <= 1 for there to be an intersection since
// the center of the circle of intersection must be on the segment connecting
// C0 and C1.
//
// Then plug this value of t into the first of the previous equations and
// solve for
//
//   R^2 = R0^2 - t^2*|N|^2
//
// As long as the right-hand side is nonnegative, the circle of intersection
// exists and the radius is R = sqrt(R0^2 - t^2*|N|^2).

#include "WmlIntrSph3Sph3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1)
{
    Vector3<Real> kDiff = rkS1.Center() - rkS0.Center();
    Real fSqrLen = kDiff.SquaredLength();
    Real fRSum = rkS0.Radius() + rkS1.Radius();
    Real fRSumSqr = fRSum*fRSum;

    return fSqrLen <= fRSumSqr;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1, Vector3<Real>& rkU, Vector3<Real>& rkV,
    Vector3<Real>& rkC, Real& rfR)
{
    // plane of intersection must have N as its normal
    Vector3<Real> kN = rkS1.Center() - rkS0.Center();
    Real fNSqrLen = kN.SquaredLength();
    Real fRSum = rkS0.Radius() + rkS1.Radius();
    if ( fNSqrLen > fRSum*fRSum )
    {
        // sphere centers are too far apart for intersection
        return false;
    }

    Real fR0Sqr = rkS0.Radius()*rkS0.Radius();
    Real fR1Sqr = rkS1.Radius()*rkS1.Radius();
    Real fInvNSqrLen = ((Real)1.0)/fNSqrLen;
    Real fT = ((Real)0.5)*((Real)1.0+(fR0Sqr-fR1Sqr)*fInvNSqrLen);
    if ( fT < (Real)0.0 || fT > (Real)1.0 )
        return false;

    Real fRSqr = fR0Sqr - fT*fT*fNSqrLen;
    if ( fRSqr < (Real)0.0 )
        return false;

    // center and radius of circle of intersection
    rkC = rkS0.Center() + fT*kN;
    rfR = Math<Real>::Sqrt(fRSqr);

    // compute U and V for plane of circle
    kN *= Math<Real>::Sqrt(fInvNSqrLen);
    Vector3<Real>::GenerateOrthonormalBasis(rkU,rkV,kN,true);

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1, Real fTime, const Vector3<Real>& rkV0,
    const Vector3<Real>& rkV1)
{
    Vector3<Real> kVDiff = rkV1 - rkV0;
    Real fA = kVDiff.SquaredLength();
    Vector3<Real> kCDiff = rkS1.Center() - rkS0.Center();
    Real fC = kCDiff.SquaredLength();
    Real fRSum = rkS0.Radius() + rkS1.Radius();
    Real fRSumSqr = fRSum*fRSum;

    if ( fA > (Real)0.0 )
    {
        Real fB = kCDiff.Dot(kVDiff);
        if ( fB <= (Real)0.0 )
        {
            if ( -fTime*fA <= fB )
                return fA*fC - fB*fB <= fA*fRSumSqr;
            else
                return fTime*(fTime*fA + ((Real)2.0)*fB) + fC <= fRSumSqr;
        }
    }

    return fC <= fRSumSqr;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1, Real fTime, const Vector3<Real>& rkV0,
    const Vector3<Real>& rkV1, Real& rfFirstTime,
    Vector3<Real>& rkFirstPoint)
{
    Vector3<Real> kVDiff = rkV1 - rkV0;
    Real fA = kVDiff.SquaredLength();
    Vector3<Real> kCDiff = rkS1.Center() - rkS0.Center();
    Real fC = kCDiff.SquaredLength();
    Real fRSum = rkS0.Radius() + rkS1.Radius();
    Real fRSumSqr = fRSum*fRSum;

    if ( fA > (Real)0.0 )
    {
        Real fB = kCDiff.Dot(kVDiff);
        if ( fB <= (Real)0.0 )
        {
            if ( -fTime*fA <= fB
            ||   fTime*(fTime*fA + ((Real)2.0)*fB) + fC <= fRSumSqr )
            {
                Real fCDiff = fC - fRSumSqr;
                Real fDiscr = fB*fB - fA*fCDiff;
                if ( fDiscr >= (Real)0.0 )
                {
                    if ( fCDiff <= (Real)0.0 )
                    {
                        // The spheres are initially intersecting.  Estimate a
                        // point of contact by using the midpoint of the line
                        // segment connecting the sphere centers.
                        rfFirstTime = (Real)0.0;
                        rkFirstPoint = ((Real)0.5)*(rkS0.Center() +
                            rkS1.Center());
                    }
                    else
                    {
                        // The first time of contact is in [0,fTime].
                        rfFirstTime = -(fB + Math<Real>::Sqrt(fDiscr))/fA;
                        if ( rfFirstTime < (Real)0.0 )
                            rfFirstTime = (Real)0.0;
                        else if ( rfFirstTime > fTime )
                            rfFirstTime = fTime;

                        Vector3<Real> kNewCDiff = kCDiff + rfFirstTime*kVDiff;

                        rkFirstPoint = rkS0.Center() + rfFirstTime*rkV0 +
                            (rkS0.Radius()/fRSum)*kNewCDiff;
                    }
                    return true;
                }
            }
            return false;
        }
    }

    if ( fC <= fRSumSqr )
    {
        // The spheres are initially intersecting.  Estimate a point of
        // contact by using the midpoint of the line segment connecting the
        // sphere centers.
        rfFirstTime = (Real)0.0;
        rkFirstPoint = ((Real)0.5)*(rkS0.Center() + rkS1.Center());
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Sphere3<float>&,
    const Sphere3<float>&);
template WML_ITEM bool TestIntersection<float> (const Sphere3<float>&,
    const Sphere3<float>&, float, const Vector3<float>&,
    const Vector3<float>&);
template WML_ITEM bool FindIntersection<float> (const Sphere3<float>&,
    const Sphere3<float>&, Vector3<float>&, Vector3<float>&,
    Vector3<float>&, float&);
template WML_ITEM bool FindIntersection<float> (const Sphere3<float>&,
    const Sphere3<float>&, float, const Vector3<float>&,
    const Vector3<float>&, float&, Vector3<float>&);

template WML_ITEM bool TestIntersection<double> (const Sphere3<double>&,
    const Sphere3<double>&);
template WML_ITEM bool TestIntersection<double> (const Sphere3<double>&,
    const Sphere3<double>&, double, const Vector3<double>&,
    const Vector3<double>&);
template WML_ITEM bool FindIntersection<double> (const Sphere3<double>&,
    const Sphere3<double>&, Vector3<double>&, Vector3<double>&,
    Vector3<double>&, double&);
template WML_ITEM bool FindIntersection<double> (const Sphere3<double>&,
    const Sphere3<double>&, double, const Vector3<double>&,
    const Vector3<double>&, double&, Vector3<double>&);
}
//----------------------------------------------------------------------------
