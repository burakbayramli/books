// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRCIR2BOX2_H
#define WMLINTRCIR2BOX2_H

#include "WmlCircle2.h"
#include "WmlBox2.h"

namespace Wml
{

// Find first time/point of contact of moving circle and oriented rectangle.
//
// Inputs are:
//   rkCircle:  circle (center and radius)
//   rkV:       circle velocity
//   rkBox:     oriented rectangle
//   rfTFirst:  first time of contact
//   rkIntr:    first point of contact
//
// Return -1 if initially intersecting, 0 if no intersection, +1 if intersects
// at some positive time.  First time/point of contact are only valid if the
// return value is +1.

template <class Real>
WML_ITEM int FindIntersection (const Circle2<Real>& rkCircle,
    const Vector2<Real>& rkV, const Box2<Real>& rkBox, Real& rfTFirst,
    Vector2<Real>& rkIntr);

}

#endif
