// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3SPH3_H
#define WMLINTRLIN3SPH3_H

#include "WmlSphere3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// The return value is 'true' if and only if the objects intersect.

// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Sphere3<Real>& rkSphere);

template <class Real>
WML_ITEM bool TestIntersection (const Ray3<Real>& rkRay,
    const Sphere3<Real>& rkSphere);

template <class Real>
WML_ITEM bool TestIntersection (const Line3<Real>& rkLine,
    const Sphere3<Real>& rkSphere);

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Sphere3<Real>& rkSphere, int& riQuantity, Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
    const Sphere3<Real>& rkSphere, int& riQuantity, Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Sphere3<Real>& rkSphere, int& riQuantity, Vector3<Real> akPoint[2]);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity);

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akPoint[2]);

}

#endif
