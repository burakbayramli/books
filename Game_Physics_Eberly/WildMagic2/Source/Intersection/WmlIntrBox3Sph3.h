// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRBOX3SPH3_H
#define WMLINTRBOX3SPH3_H

#include "WmlBox3.h"
#include "WmlSphere3.h"

namespace Wml
{

// The box is treated as a solid.  The extents must be positive; that is, no
// flat boxes are allowed.  The FindIntersection query returns 0 or 1 point.
// An initially intersecting box and sphere returns 'false'.  (The set of
// intersection can be quite complicated in this configuration.)

// box and sphere are stationary
template <class Real>
WML_ITEM bool TestIntersection (const Box3<Real>& rkBox,
    const Sphere3<Real>& rkSphere);

// box and sphere are moving
template <class Real>
WML_ITEM bool FindIntersection (const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real>& rkP);

}

#endif
