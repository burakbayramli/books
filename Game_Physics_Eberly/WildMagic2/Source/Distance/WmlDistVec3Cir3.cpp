// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Cir3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkClosest)
{
    // signed distance from point to plane of circle
    Vector3<Real> kDiff0 = rkPoint - rkCircle.Center();
    Real fDist = kDiff0.Dot(rkCircle.N());

    // projection of P-C onto plane is Q-C = P-C - (fDist)*N
    Vector3<Real> kDiff1 = kDiff0 - fDist*rkCircle.N();
    Real fSqrLen = kDiff1.SquaredLength();
    Vector3<Real> kClosest;
    Real fSqrDist;

    if ( fSqrLen >= Math<Real>::EPSILON )
    {
        kClosest = rkCircle.Center() + (rkCircle.Radius()/
            Math<Real>::Sqrt(fSqrLen))*kDiff1;
        Vector3<Real> kDiff2 = rkPoint - kClosest;
        fSqrDist = kDiff2.SquaredLength();
    }
    else
    {
        kClosest = Vector3<Real>(Math<Real>::MAX_REAL,Math<Real>::MAX_REAL,
            Math<Real>::MAX_REAL);
        fSqrDist = rkCircle.Radius()*rkCircle.Radius() + fDist*fDist;
    }

    if ( pkClosest )
        *pkClosest = kClosest;

    return fSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Disk3<Real>& rkDisk, Vector3<Real>* pkClosest)
{
    // signed distance from point to plane of rkDisk
    Vector3<Real> kDiff0 = rkPoint - rkDisk.Center();
    Real fDist = kDiff0.Dot(rkDisk.N());

    // projection of P-C onto plane is Q-C = P-C - (fDist)*N
    Vector3<Real> kDiff1 = kDiff0 - fDist*rkDisk.N();
    Real fSqrLen = kDiff1.SquaredLength();
    Vector3<Real> kClosest;
    Real fSqrDist;

    if ( fSqrLen <= rkDisk.Radius()*rkDisk.Radius() )
    {
        // projected point Q is in disk and is closest to P
        kClosest = rkDisk.Center() + kDiff1;
        fSqrDist = fDist*fDist;
    }
    else
    {
        // projected point Q is outside disk, closest point is on circle
        kClosest = rkDisk.Center() +
            (rkDisk.Radius()/Math<Real>::Sqrt(fSqrLen))*kDiff1;
        Vector3<Real> kDiff2 = rkPoint - kClosest;
        fSqrDist = kDiff2.SquaredLength();
    }

    if ( pkClosest )
        *pkClosest = kClosest;

    return fSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkCircle,pkClosest));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint, const Disk3<Real>& rkDisk,
    Vector3<Real>* pkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkDisk,pkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Circle3<float>&, Vector3<float>*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Circle3<float>&, Vector3<float>*);
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Disk3<float>&, Vector3<float>*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Disk3<float>&, Vector3<float>*);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Circle3<double>&, Vector3<double>*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Circle3<double>&, Vector3<double>*);
template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Disk3<double>&, Vector3<double>*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Disk3<double>&, Vector3<double>*);
}
//----------------------------------------------------------------------------
