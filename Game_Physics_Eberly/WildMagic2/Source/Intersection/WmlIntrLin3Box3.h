// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3BOX3_H
#define WMLINTRLIN3BOX3_H

#include "WmlBox3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// return value is 'true' if and only if objects intersect

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Box3<Real>& rkBox);

template <class Real>
WML_ITEM bool TestIntersection (const Ray3<Real>& rkRay,
    const Box3<Real>& rkBox);

template <class Real>
WML_ITEM bool TestIntersection (const Line3<Real>& rkLine,
    const Box3<Real>& rkBox);

// Clipping of a linear component 'origin'+t*'direction' against an
// axis-aligned box [-e0,e0]x[-e1,e1]x[-e2,e2] where 'extent'=(e0,e1,e2).
// The values of t0 and t1 must be set by the caller.  If the component is a
// segment, set t0 = 0 and t1 = 1.  If the component is a ray, set t0 = 0 and
// t1 = MAXREAL.  If the component is a line, set t0 = -MAXREAL and
// t1 = MAXREAL.  The values are (possibly) modified by the clipper.

template <class Real>
WML_ITEM bool FindIntersection (const Vector3<Real>& rkOrigin,
    const Vector3<Real>& rkDirection, const Real afExtent[3], Real& rfT0,
    Real& rfT1);

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akPoint[2]);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[2]);

}

#endif
