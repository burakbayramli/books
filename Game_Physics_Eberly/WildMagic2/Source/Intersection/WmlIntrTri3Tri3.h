// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRTRI3TRI3_H
#define WMLINTRTRI3TRI3_H

#include "WmlTriangle3.h"

namespace Wml
{

// stationary objects

template <class Real>
WML_ITEM bool TestIntersection (const Triangle3<Real>& rkUTri,
    const Triangle3<Real>& rkVTri);

template <class Real>
WML_ITEM bool FindIntersection (const Triangle3<Real>& rkUTri,
    const Triangle3<Real>& rkVTri, int& riQuantity, Vector3<Real> akP[6]);


// moving objects

template <class Real>
WML_ITEM bool TestIntersection (const Triangle3<Real>& rkUTri,
    const Vector3<Real>& rkUVelocity, const Triangle3<Real>& rkVTri,
    const Vector3<Real>& rkVVelocity, Real& rfTFirst, Real fTMax);

template <class Real>
WML_ITEM bool FindIntersection (const Triangle3<Real>& rkUTri,
    const Vector3<Real>& rkUVelocity, const Triangle3<Real>& rkVTri,
    const Vector3<Real>& rkVVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6]);

}

#endif
