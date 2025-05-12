// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3ELD3_H
#define WMLINTRLIN3ELD3_H

#include "WmlEllipsoid3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// return value is 'true' if and only if objects intersect

template <class Real>
WML_ITEM bool TestIntersection (const Segment3<Real>& rkSegment,
    const Ellipsoid3<Real>& rkEllipsoid);

template <class Real>
WML_ITEM bool TestIntersection (const Ray3<Real>& rkRay,
    const Ellipsoid3<Real>& rkEllipsoid);

template <class Real>
WML_ITEM bool TestIntersection (const Line3<Real>& rkLine,
    const Ellipsoid3<Real>& rkEllipsoid);


template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Ellipsoid3<Real>& rkEllipsoid, int& riQuantity,
    Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
    const Ellipsoid3<Real>& rkEllipsoid, int& riQuantity,
    Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Ellipsoid3<Real>& rkEllipsoid, int& riQuantity,
    Vector3<Real> akPoint[2]);

}

#endif
