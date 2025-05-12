// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN2LIN2_H
#define WMLINTRLIN2LIN2_H

#include "WmlLine2.h"
#include "WmlRay2.h"
#include "WmlSegment2.h"

namespace Wml
{

// The return value is 'true' if and only if the objects intersect.  If an
// intersection occurs, the returned values are the parameter values for the
// point(s) of intersection with respect to the first object parameterized
// by P+t*D (t in [-inf,+inf] for line, t in [0,+inf] for ray, t in [0,1]
// for segment).  The returned quantity is 1 or 2.  If 2, then the returned
// values are the end points of an interval of intersection.

template <class Real>
WML_ITEM bool FindIntersection (const Line2<Real>& rkLine0,
    const Line2<Real>& rkLine1, int& riQuantity, Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line2<Real>& rkLine,
    const Ray2<Real>& rkRay, int& riQuantity, Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line2<Real>& rkLine,
    const Segment2<Real>& rkSegment, int& riQuantity, Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray2<Real>& rkRay0,
    const Ray2<Real>& rkRay1, int& riQuantity, Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray2<Real>& rkRay,
    const Segment2<Real>& rkSegment, int& riQuantity, Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Segment2<Real>& rkSegment0,
    const Segment2<Real>& rkSegment1, int& riQuantity, Real afT[2]);

}

#endif
