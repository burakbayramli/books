// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRTRI3SPH3_H
#define WMLINTRTRI3SPH3_H

#include "WmlSphere3.h"
#include "WmlTriangle3.h"

namespace Wml
{

// Determine if triangle transversely intersects sphere.  Return value is
// 'true' if and only if they intersect.  The unction does not indicate an
// intersection if one or more vertices are the only points of intersection
// or if the triangle is tangent to the sphere.

template <class Real>
WML_ITEM bool TestIntersection (const Triangle3<Real>& rkTri,
    const Sphere3<Real>& rkSphere);


// moving objects

template <class Real>
WML_ITEM bool FindIntersection (const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6]);

} 

#endif
