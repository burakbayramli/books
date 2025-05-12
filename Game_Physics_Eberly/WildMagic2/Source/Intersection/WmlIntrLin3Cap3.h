// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3CAP3_H
#define WMLINTRLIN3CAP3_H

#include "WmlCapsule3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// return value is 'true' if and only if objects intersect

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Capsule3<Real>& rkCapsule);

template <class Real>
WML_ITEM bool TestIntersection (const Ray3<Real>& rkRay,
    const Capsule3<Real>& rkCapsule);

template <class Real>
WML_ITEM bool TestIntersection (const Line3<Real>& rkLine,
    const Capsule3<Real>& rkCapsule);

// The returned values afT[i] correspond to measurements along the input
// linear component:  origin + afT[i]*direction

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Capsule3<Real>& rkCapsule, int& riQuantity,
    Vector3<Real> akPoint[2], Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
    const Capsule3<Real>& rkCapsule, int& riQuantity,
    Vector3<Real> akPoint[2], Real afT[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Capsule3<Real>& rkCapsule, int& riQuantity,
    Vector3<Real> akPoint[2], Real afT[2]);

}

#endif
