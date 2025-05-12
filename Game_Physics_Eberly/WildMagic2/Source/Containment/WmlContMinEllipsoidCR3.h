// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTMINELLIPSOIDCR3_H
#define WMLCONTMINELLIPSOIDCR3_H

#include "WmlMatrix3.h"

namespace Wml
{

// The ellipsoid in general form is  X^t A X + B^t X + C = 0 where
// A is a positive definite 3x3 matrix, B is a 3x1 vector, C is a
// scalar, and X is a 3x1 vector.  Completing the square,
// (X-U)^t A (X-U) = U^t A U - C where U = -0.5 A^{-1} B.  Define
// M = A/(U^t A U - C).  The ellipsoid is (X-U)^t M (X-U) = 1.  Factor
// M = R^t D R where R is orthonormal and D is diagonal with positive
// diagonal terms.  If Y = R(X-U), then the ellipsoid is 1 = Y^t D Y =
// d1*y1^2+d2*y2^2+d3*y3^2.  For an ellipsoid (x/a)^2+(y/b)^2+(z/c)^2
// = 1, the volume is (4*pi/3)*a*b*c.  For Y^t D Y = 1, the volume is
// therefore (4*pi/3)/sqrt(d1*d2*d3).  Finally, note that det(M) =
// det(D) = d1*d2*d3, so the volume of the ellipsoid is
// (4*pi/3)/sqrt(det(M)).

// Compute minimal volume ellipsoid (X-C)^t R^t D R (X-C) = 1 given center
// C and orientation matrix R by finding diagonal D.  Minimal volume is
// (4*pi/3)/sqrt(D[0]*D[1]*D[2]).

template <class Real>
WML_ITEM void MinEllipsoidCR3 (int iQuantity, const Vector3<Real>* akPoint,
    const Vector3<Real>& rkC, const Matrix3<Real>& rkR, Real afD[3]);

}

#endif
