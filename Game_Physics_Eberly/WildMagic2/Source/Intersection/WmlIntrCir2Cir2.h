// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRCIR2CIR2_H
#define WMLINTRCIR2CIR2_H

#include "WmlArc2.h"

// The return value is 'true' if and only if the objects intersect.  The
// returned quantity is usually 0, 1, or 2 and is the number of valid points
// in akPoint[2] (the intersection points).  It is also possible that the
// two objects overlap in infinitely many points.  For the circle-circle case,
// the returned quantity is -1 and the first circle can be used as the
// intersection set.  The akPoint[] values are invalid.  For the circle-arc
// case, the returned quantity is -1 and the arc can be used as the
// intersection set.  The akPoint[] values are invalid.  For the arc-arc case,
// the returned quantity is
//    0: arcs do not overlap
//   -1: arcs just touch at a single point, akPoint[0]
//   -2: arcs overlap on an arc, <akPoint[0],akPoint[1]>
// The application must look at the sign of riQuantity when the function
// returns 'true' to trap these cases.

namespace Wml
{

template <class Real>
WML_ITEM bool FindIntersection (const Circle2<Real>& rkCircle0,
    const Circle2<Real>& rkCircle1, int& riQuantity,
    Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Circle2<Real>& rkCircle,
    const Arc2<Real>& rkArc, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Arc2<Real>& rkArc0,
    const Arc2<Real>& rkArc1, int& riQuantity, Vector2<Real> akPoint[2]);

}

#endif
