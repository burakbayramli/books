// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTMINELLIPSECR2_H
#define WMLCONTMINELLIPSECR2_H

#include "WmlMatrix2.h"

// The ellipse in general form is  X^t A X + B^t X + C = 0 where
// A is a positive definite 2x2 matrix, B is a 2x1 vector, C is a
// scalar, and X is a 2x1 vector X.  Completing the square,
// (X-U)^t A (X-U) = U^t A U - C where U = -0.5 A^{-1} B.  Define
// M = A/(U^t A U - C).  The ellipse is (X-U)^t M (X-U) = 1.  Factor
// M = R^t D R where R is orthonormal and D is diagonal with positive
// diagonal terms.  If Y = R(X-U), then the ellipse is 1 = Y^t D Y =
// d1*y1^2+d2*y2^2.  For an ellipse (x/a)^2+(y/b)^2 = 1, the area is
// pi*a*b.  For Y^t D Y = 1, the area is therefore pi/sqrt(d1*d2).
// Finally, note that det(M) = det(D) = d1*d2, so the area of the
// ellipse is pi/sqrt(det(M)).

// Compute minimal area ellipse (X-C)^t R^t D R (X-C) = 1 given center
// C and orientation matrix R by finding diagonal D.  Minimal area is
// pi/sqrt(D[0]*D[1]).

namespace Wml
{

template <class Real>
WML_ITEM void MinEllipseCR2 (int iQuantity, const Vector2<Real>* akPoint,
    const Vector2<Real>& rkC, const Matrix2<Real>& rkR, Real afD[2]);

}

#endif
