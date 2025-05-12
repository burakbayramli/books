// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRHSP3SPH3_H
#define WMLINTRHSP3SPH3_H

#include "WmlHalfSpace3.h"
#include "WmlSphere3.h"

namespace Wml
{

// A half space is the set of points on the side of a plane to which the
// plane normal points.  The queries here are for intersection of a sphere
// and a half space.  In the FindIntersection query where the objects have
// velocity, if the sphere is already intersecting the half space, the return
// value is 'false'.  The idea is to find first time of contact, in which
// case there is a single point of intersection.

// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Sphere3<Real>& rkSphere);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real>& rkP);

}

#endif
