// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3TRI3_H
#define WMLINTRLIN3TRI3_H

#include "WmlTriangle3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// The return value is 'true' if and only if the objects intersect.

// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Triangle3<Real>& rkTriangle);

template <class Real>
WML_ITEM bool TestIntersection (const Ray3<Real>& rkRay,
    const Triangle3<Real>& rkTriangle);

template <class Real>
WML_ITEM bool TestIntersection (const Line3<Real>& rkLine,
    const Triangle3<Real>& rkTriangle);

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Triangle3<Real>& rkTriangle, Vector3<Real>& rkPoint);

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
    const Triangle3<Real>& rkTriangle, Vector3<Real>& rkPoint);

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Triangle3<Real>& rkTriangle, Vector3<Real>& rkPoint);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Triangle3<Real>& rkTriangle,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity,  const Triangle3<Real>& rkTriangle,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[2]);

}

#endif
