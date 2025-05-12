// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRINTERVAL_H
#define WMLINTRINTERVAL_H

#include "WmlSystem.h"

namespace Wml
{

// Compute the intersection of [u0,u1] and [v0,v1] where u0 < u1 and
// v0 < v1.  Values of +MAX_REAL or -MAX_REAL are allowed.  The returned
// quantity is
//   0:  no intersection (w[0] and w[1] are invalid)
//   1:  intersection contains one point (w[0] is either u0 or u1)
//   2:  intersection is an interval, [w[0],w[1]]
template <class Real>
WML_ITEM int FindIntersection (Real fU0, Real fU1, Real fV0, Real fV1,
    Real& rfW0, Real& rfW1);

// The intersection is stored in-place (for convenience in calling).
template <class Real>
WML_ITEM int FindIntersection (Real fU0, Real fU1, Real& rfV0, Real& rfV1);

}

#endif
