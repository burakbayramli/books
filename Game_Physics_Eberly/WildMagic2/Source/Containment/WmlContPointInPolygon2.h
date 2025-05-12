// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTPOINTINPOLYGON2_H
#define WMLCONTPOINTINPOLYGON2_H

// Given a polygon as an order list of vertices (x[i],y[i]) for
// 0 <= i < N and a test point (xt,yt), return 'true' if (xt,yt) is in
// the polygon and 'false' if it is not.  All queries require that the
// number of vertices satisfies N >= 3.

#include "WmlVector2.h"

namespace Wml
{

// general polygons
template <class Real>
WML_ITEM bool PointInPolygon (int iQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP);

// Algorithms for convex polygons.  The input polygons must have vertices in
// counterclockwise order.

// O(N) algorithm
template <class Real>
WML_ITEM bool PointInConvexOrderN (int iQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP);

// O(log N) algorithm, uses bisection and recursion
template <class Real>
WML_ITEM bool PointInConvexOrderLogN (int iQuantity, const Vector2<Real>* akV,
    const Vector2<Real>& rkP);

// O(log N) algorithm but hard-coded for the specified size.  The number at
// the end of the function name is the number of vertices in the convex
// polygon.
template <class Real>
WML_ITEM bool PointInConvex4 (const Vector2<Real>* akV,
     const Vector2<Real>& rkP);

}

#endif
