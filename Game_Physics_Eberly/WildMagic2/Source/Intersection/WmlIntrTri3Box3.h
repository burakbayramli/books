// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRTRI3BOX3_H
#define WMLINTRTRI3BOX3_H

#include "WmlTriangle3.h"
#include "WmlBox3.h"

namespace Wml
{

// The return value is 'true' if and only if the objects intersect.

// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const Triangle3<Real>& rkTri,
    const Box3<Real>& rkBox);

template <class Real>
WML_ITEM bool FindIntersection (const Triangle3<Real>& rkTri,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akP[6]);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real fTMax, Real& rfTFirst,
    Real& rfTLast);

template <class Real>
WML_ITEM bool FindIntersection (const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6]);

}

#endif
