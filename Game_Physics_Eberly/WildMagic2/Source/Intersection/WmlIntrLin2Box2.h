// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN2BOX2_H
#define WMLINTRLIN2BOX2_H

#include "WmlBox2.h"
#include "WmlLine2.h"
#include "WmlRay2.h"
#include "WmlSegment2.h"

namespace Wml
{

// return value is 'true' if and only if objects intersect

template <class Real>
WML_ITEM bool TestIntersection (const Segment2<Real>& rkSegment,
    const Box2<Real>& rkBox);

template <class Real>
WML_ITEM bool TestIntersection (const Ray2<Real>& rkRay,
    const Box2<Real>& rkBox);

template <class Real>
WML_ITEM bool TestIntersection (const Line2<Real>& rkLine,
    const Box2<Real>& rkBox);

// Clipping of a linear component 'origin'+t*'direction' against an
// axis-aligned box [-e0,e0]x[-e1,e1] where 'extent'=(e0,e1,e2).  The values
// of t0 and t1 must be set by the caller.  If the component is a segment, set
// t0 = 0 and t1 = 1.  If the component is a ray, set t0 = 0 and t1 = MAXREAL.
// If the component is a line, set t0 = -MAXREAL and t1 = MAXREAL.  The values
// are (possibly) modified by the clipper.

template <class Real>
WML_ITEM bool FindIntersection (const Vector2<Real>& rkOrigin,
    const Vector2<Real>& rkDirection, const Real afExtent[2], Real& rfT0,
    Real& rfT1);

template <class Real>
WML_ITEM bool FindIntersection (const Segment2<Real>& rkSegment,
    const Box2<Real>& rkBox, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray2<Real>& rkRay,
    const Box2<Real>& rkBox, int& riQuantity, Vector2<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line2<Real>& rkLine,
    const Box2<Real>& rkBox, int& riQuantity, Vector2<Real> akPoint[2]);

}

#endif
