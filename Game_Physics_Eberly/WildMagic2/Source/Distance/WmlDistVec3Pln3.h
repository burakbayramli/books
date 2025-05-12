// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3PLN3_H
#define WMLDISTVEC3PLN3_H

// Squared distance from point to plane.  First function handles
// Dot(N,X-A) = NULL.  Second function handles planes Dot(N,X) = c.  The
// plane normal N must be unit length.

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Vector3<Real>& rkNormal, const Vector3<Real>& rkOrigin,
    Vector3<Real>* pkClosest = NULL);

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Vector3<Real>& rkNormal, Real fConstant,
    Vector3<Real>* pkClosest = NULL);

}

#endif
