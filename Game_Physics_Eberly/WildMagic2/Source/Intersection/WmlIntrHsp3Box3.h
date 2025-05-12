// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRHSP3BOX3_H
#define WMLINTRHSP3BOX3_H

#include "WmlHalfSpace3.h"
#include "WmlBox3.h"

namespace Wml
{

// A half space is the set of points on the side of a plane to which the
// plane normal points.  In the FindIntersection query where the objects have
// velocity, if the box is already intersecting the half space, the return
// value is 'false'.  The idea is to find first time of contact.


// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Box3<Real>& rkBox);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[4]);

}

#endif
