// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3CYL3_H
#define WMLINTRLIN3CYL3_H

#include "WmlCylinder3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// These intersection routines are for bounded cylinders, that is, those for
// which the height is finite.  The return value is 'true' if and only if
// the objects intersect.  The caller should make sure that the cylinder is
// appropriately tagged as capped or hollow.  This is accomplished by using
// the Cylinder3<Real>::Capped member function.

template <class Real>
WML_ITEM bool FindIntersection (const Segment3<Real>& rkSegment,
    const Cylinder3<Real>& rkCylinder, int& riQuantity,
    Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
    const Cylinder3<Real>& rkCylinder, int& riQuantity,
    Vector3<Real> akPoint[2]);

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Cylinder3<Real>& rkCylinder, int& riQuantity,
    Vector3<Real> akPoint[2]);

}

#endif
