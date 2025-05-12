// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRSPH3SPH3_H
#define WMLINTRSPH3SPH3_H

#include "WmlSphere3.h"

namespace Wml
{

// Test for intersection of static spheres.
template <class Real>
WML_ITEM bool TestIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1);

// Test for intersection of moving spheres.
template <class Real>
WML_ITEM bool TestIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1, Real fTime, const Vector3<Real>& rkV0,
    const Vector3<Real>& rkV1);

// Find the intersection of static spheres.  The circle of intersection is
// X(t) = C + R*(cos(t)*U + sin(t)*V) for 0 <= t < 2*pi.
template <class Real>
WML_ITEM bool FindIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1, Vector3<Real>& rkU, Vector3<Real>& rkV,
    Vector3<Real>& rkC, Real& rfR);

// Find the first time/point of contact of moving spheres.  If the spheres
// are initially intersecting, the reported first time is 0 and a point of
// contact is *estimated* as the average of sphere centers.
template <class Real>
WML_ITEM bool FindIntersection (const Sphere3<Real>& rkS0,
    const Sphere3<Real>& rkS1, Real fTime, const Vector3<Real>& rkV0,
    const Vector3<Real>& rkV1, Real& rfFirstTime,
    Vector3<Real>& rkFirstPoint);

}

#endif
