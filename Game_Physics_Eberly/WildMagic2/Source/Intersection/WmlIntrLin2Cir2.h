// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN2CIR2_H
#define WMLINTRLIN2CIR2_H

#include "WmlArc2.h"
#include "WmlLine2.h"
#include "WmlRay2.h"
#include "WmlSegment2.h"

namespace Wml
{

// The return value is 'true' if and only if the objects intersect.  The
// returned quantity is 0, 1, or 2 and is the number of valid points in
// akPoint[2] (the intersection points).

template <class Real>
WML_ITEM bool FindIntersection (const Line2<Real>& rkLine,
    const Circle2<Real>& rkCircle, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line2<Real>& rkLine,
    const Arc2<Real>& rkArc, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray2<Real>& rkRay,
    const Circle2<Real>& rkCircle, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray2<Real>& rkRay,
    const Arc2<Real>& rkArc, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Segment2<Real>& rkSegment,
    const Circle2<Real>& rkCircle, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Segment2<Real>& rkSegment,
    const Arc2<Real>& rkArc, int& riQuantity, Vector2<Real> akPoint[2]);

}

#endif
