// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRELP2ELP2_H
#define WMLINTRELP2ELP2_H

#include "WmlVector2.h"

namespace Wml
{

// An ellipse has a center C, two unit-length and orthogonal axis directions
// U[0] and U[1], and axis half-lengths L[0] and L[1].  In parametric form
// the ellipse is
//   X(t) = C + cos(t)*L[0]*U[0] + sin(t)*L[1]*U[1]
// for 0 <= t < 2*pi.
//
// In vector-matrix form the ellipse is
//   (X-C)^T * M * (X-C) = 1
// where superscript T denotes transpose.  M is the 2-by-2 symmetric matrix
//   M = U[0]*U[0]^T/L[0]^2 + U[1]*U[1]^T/L[1]^2
// The vector U[i] is 2-by-1, so U[i]*U[i]^T is a 2-by-2 matrix.
//
// As a quadratic equation in two variables, the ellipse is
//   Q(x,y) = a[0]*x^2 + a[1]*x*y + a[2]*y^2 + a[3]*x + a[4]*y + a[5] = 0
// where a[1]*a[1] < 4*a[0]*a[2] is required for this to represent an ellipse.

// compute the quadratic equation for the ellipse
template <class Real>
WML_ITEM void ConvertEllipseToQuadratic (const Vector2<Real>& rkC,
    const Vector2<Real> akAxis[2], const Real afL[2], Real afQuad[6]);

// On return, riQuantity is the number of intersections (in {0,1,2,3,4}) and
// akP[0],...,akP[riQuantity-1] are the points of intersection.  The return
// value is 'true' if and only if riQuantity > 0.
template <class Real>
WML_ITEM bool FindIntersection (const Vector2<Real>& rkC0,
    const Vector2<Real> akAxis0[2], const Real afL0[2],
    const Vector2<Real>& rkC1, const Vector2<Real> akAxis1[2],
    const Real afL1[2], int& riQuantity, Vector2<Real> akP[4]);

// Return value is 'true' if and only if there is an intersection.  If there
// is no intersection, the ellipses are either separated by a line or one is
// properly contained in the other.
template <class Real>
WML_ITEM bool TestIntersection (const Vector2<Real>& rkC0,
    const Vector2<Real> akAxis0[2], const Real afL0[2],
    const Vector2<Real>& rkC1, const Vector2<Real> akAxis1[2],
    const Real afL1[2]);

template <class Real>
WML_ITEM bool TestIntersection (const Real afQP0[6], const Real afQP1[6]);

}

#endif
