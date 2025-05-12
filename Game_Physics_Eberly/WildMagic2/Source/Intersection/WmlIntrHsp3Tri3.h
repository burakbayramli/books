// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRHSP3TRI3_H
#define WMLINTRHSP3TRI3_H

#include "WmlHalfSpace3.h"
#include "WmlTriangle3.h"

namespace Wml
{

// A half space is the set of points on the side of a plane to which the
// plane normal points.  In the FindIntersection query where the objects have
// velocity, if the triangle is already intersecting the half space, the
// return value is 'false'.  The idea is to find first time of contact.

// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Triangle3<Real>& rkTri);

template <class Real>
WML_ITEM bool FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Triangle3<Real>& rkTri, int& riQuantity, Vector3<Real> akP[3]);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity,  Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[3]);
}

#endif
