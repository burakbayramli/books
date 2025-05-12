// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC2QDR2_H
#define WMLDISTVEC2QDR2_H

// Given a quadratic curve defined implicitly by
//   0 = C[0] + C[1]*X + C[2]*Y + C[3]*X^2 + C[4]*Y^2 + C[5]*X*Y
// find distance from point to the curve.

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
WML_ITEM Real SqrDistance (const Vector2<Real>& rkPoint, const Real afQuad[6],
    Vector2<Real>& rkClosest);

template <class Real>
WML_ITEM Real Distance (const Vector2<Real>& rkPoint, const Real afQuad[6],
    Vector2<Real>& rkClosest);

}

#endif
