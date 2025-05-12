// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRHSP3LIN3_H
#define WMLINTRHSP3LIN3_H

#include "WmlHalfSpace3.h"
#include "WmlSegment3.h"

namespace Wml
{

// A half space is the set of points on the side of a plane to which the
// plane normal points.  The queries here are for intersection of a line
// segment and a half space.  In the FindIntersection query where the
// objects have velocity, if the segment is already intersecting the half
// space, the return value is 'false'.  The idea is to find first time of
// contact.

// segment and half space are stationary

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Segment3<Real>& rkSegment);

template <class Real>
WML_ITEM bool FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Segment3<Real>& rkSegment, int& riQuantity, Vector3<Real> akP[2]);


// segment and half space are moving

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[2]);

}

#endif
