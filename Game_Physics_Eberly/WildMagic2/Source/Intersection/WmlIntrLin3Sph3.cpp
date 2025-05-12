// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Sph3.h"
#include "WmlIntrLin3Cap3.h"
#include "WmlDistVec3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Sphere3<Real>& rkSphere)
{
    Real fSqrDist = SqrDistance(rkSphere.Center(),rkSegment);
    return fSqrDist <= rkSphere.Radius()*rkSphere.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray3<Real>& rkRay,
    const Sphere3<Real>& rkSphere)
{
    Real fSqrDist = SqrDistance(rkSphere.Center(),rkRay);
    return fSqrDist <= rkSphere.Radius()*rkSphere.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line3<Real>& rkLine,
    const Sphere3<Real>& rkSphere)
{
    Real fSqrDist = SqrDistance(rkSphere.Center(),rkLine);
    return fSqrDist <= rkSphere.Radius()*rkSphere.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Sphere3<Real>& rkSphere, int& riQuantity, Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkSegment.Origin() - rkSphere.Center();
    Real fA = rkSegment.Direction().SquaredLength();
    Real fB = kDiff.Dot(rkSegment.Direction());
    Real fC = kDiff.SquaredLength() -
        rkSphere.Radius()*rkSphere.Radius();

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
    const Sphere3<Real>& rkSphere, int& riQuantity, Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkRay.Origin() - rkSphere.Center();
    Real fA = rkRay.Direction().SquaredLength();
    Real fB = kDiff.Dot(rkRay.Direction());
    Real fC = kDiff.SquaredLength() -
        rkSphere.Radius()*rkSphere.Radius();

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
    const Sphere3<Real>& rkSphere, int& riQuantity, Vector3<Real> akPoint[2])
{
    // set up quadratic Q(t) = a*t^2 + 2*b*t + c
    Vector3<Real> kDiff = rkLine.Origin() - rkSphere.Center();
    Real fA = rkLine.Direction().SquaredLength();
    Real fB = kDiff.Dot(rkLine.Direction());
    Real fC = kDiff.SquaredLength() -
        rkSphere.Radius()*rkSphere.Radius();

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
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity)
{
    // check if initially intersecting
    if ( TestIntersection(rkSegment,rkSphere) )
        return true;

    // Substract the segment velocity from the sphere velocity so that
    // the calculations are based in the coordinate system of the segment.
    // In this system, the line is of course stationary.  The sphere spans
    // a capsule.  The intersection test reduces to a static one between
    // a segment and a capsule.

    Capsule3<Real> kCapsule;
    kCapsule.Origin() = rkSphere.Center();
    kCapsule.Direction() = rkSphVelocity - rkSegVelocity;
    kCapsule.Radius() = rkSphere.Radius();

    return TestIntersection(rkSegment,kCapsule);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real /* fTMax */,
    int& riQuantity, Vector3<Real> akPoint[2])
{
    // TO DO.  The parameter fTMax is not being used.  The value rfTFirst
    // should be compared to it to decide if the first time of contact is
    // within the specified time interval.

    // check if initially intersecting
    if ( FindIntersection(rkSegment,rkSphere,riQuantity,akPoint) )
        return true;

    // Substract the segment velocity from the sphere velocity so that
    // the calculations are based in the coordinate system of the segment.
    // In this system, the line is of course stationary.  The sphere spans
    // a capsule, but instead we will "grow" the segment by the sphere radius
    // and shrink the sphere to its center.  The problem is now to detect
    // the first time the moving center intersects the capsule formed by
    // the line segment and sphere radius.

    Capsule3<Real> kCapsule;
    kCapsule.Origin() = rkSegment.Origin();
    kCapsule.Direction() = rkSegment.Direction();
    kCapsule.Radius() = rkSphere.Radius();

    Segment3<Real> kPath;
    kPath.Origin() = rkSphere.Center();
    kPath.Direction() = rkSphVelocity - rkSegVelocity;

    Real afT[2];
    if ( !FindIntersection(kPath,kCapsule,riQuantity,akPoint,afT) )
        return false;

    // We now know the sphere will intersect the segment.  This can happen
    // either at a segment end point or at a segment interior point.  We
    // need to determine which.

    Vector3<Real> kNewSphCenter = rkSphere.Center()+rfTFirst*rkSphVelocity;
    Vector3<Real> kNewSegOrigin = rkSegment.Origin()+rfTFirst*rkSegVelocity;

    Real fPoint0 = rkSegment.Direction().Dot(kNewSegOrigin);
    Real fPoint1 = fPoint0 + rkSegment.Direction().SquaredLength();
    Real fSphere = rkSegment.Direction().Dot(kNewSphCenter);

    if ( fSphere < fPoint0 )
    {
        // intersection at segment origin
        akPoint[0] = kNewSegOrigin;
    }
    else if ( fSphere > fPoint1 )
    {
        // intersection at segment origin+direction
        akPoint[0] = kNewSegOrigin + rkSegment.Direction();
    }
    else
    {
        // Intersection with interior point on edge.  Use the projection
        // along direction axis to find which point that is.
        akPoint[0] = kNewSegOrigin + (fSphere-fPoint0)/(fPoint1-fPoint0) *
            rkSegment.Direction();
    }

    riQuantity = 1;
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Sphere3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Ray3<float>&, const Sphere3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Line3<float>&, const Sphere3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Sphere3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Ray3<float>&, const Sphere3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Line3<float>&, const Sphere3<float>&, int&,
    Vector3<float>[2]);
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Vector3<float>&, const Sphere3<float>&,
    const Vector3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Vector3<float>&, const Sphere3<float>&,
    const Vector3<float>&, float&, float, int&, Vector3<float>[2]);

template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Sphere3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Ray3<double>&, const Sphere3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Line3<double>&, const Sphere3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Sphere3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Ray3<double>&, const Sphere3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Line3<double>&, const Sphere3<double>&, int&,
    Vector3<double>[2]);
template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Vector3<double>&, const Sphere3<double>&,
    const Vector3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Vector3<double>&, const Sphere3<double>&,
    const Vector3<double>&, double&, double, int&, Vector3<double>[2]);
}
//----------------------------------------------------------------------------
