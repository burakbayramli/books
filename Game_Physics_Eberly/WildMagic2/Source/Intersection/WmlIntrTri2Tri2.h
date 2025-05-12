// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRTRI2TRI2_H
#define WMLINTRTRI2TRI2_H

#include "WmlTriangle2.h"

namespace Wml
{

// The vertices are assumed to be counterclockwise ordered.  The functions
// return 'true' if and only if there is at least one point of intersection.
// Optimization of this code for your applications clearly depends on how you
// represent your triangles and what precomputed information is stored in that
// representation.

// The following two functions are for stationary triangles.

template <class Real>
WML_ITEM bool TestIntersection (const Triangle2<Real>& rkTri0,
    const Triangle2<Real>& rkTri1);

// If the return value is true, then riQuantity > 0 and is the number of
// vertices of the polygon of intersection (at most a hexagon).  The vertices
// are stored in akVertex[0] through akVertex[riQuantity-1] and are in
// counterclockwise order.

template <class Real>
WML_ITEM bool FindIntersection (const Triangle2<Real>& rkTri0,
    const Triangle2<Real>& rkTri1, int& riQuantity,
    Vector2<Real> akVertex[6]);

// The following two functions are for triangles moving with velocities W0 and
// W1 (not necessarily unit length).  The time interval over which the
// intersection query is valid is [0,TMax].  Set TMax = MAX_REAL if you just
// care if the triangles will intersect eventually.  If the function returns
// 'false', the values TFirst and TLast are not meaningful and should not be
// used.

template <class Real>
WML_ITEM bool TestIntersection (Real fTMax, const Triangle2<Real>& rkTri0,
    const Vector2<Real>& rkW0, const Triangle2<Real>& rkTri1,
    const Vector2<Real>& rkW1, Real& rfTFirst, Real& rfTLast);

template <class Real>
WML_ITEM bool FindIntersection (Real fTMax, const Triangle2<Real>& rkTri0,
    const Vector2<Real>& rkW0, const Triangle2<Real>& rkTri1,
    const Vector2<Real>& rkW1, Real& rfTFirst, Real& rfTLast,
    int& riQuantity, Vector2<Real> akVertex[6]);

}

#endif
