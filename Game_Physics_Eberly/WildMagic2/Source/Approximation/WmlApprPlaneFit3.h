// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRPLANEFIT3_H
#define WMLAPPRPLANEFIT3_H

#include "WmlVector3.h"

namespace Wml
{

// Least-squares fit of a plane to (x,y,f(x,y)) data by using distance
// measurements in the z-direction.  The resulting plane is represented by
// z = A*x + B*y + C.  The return value is 'false' if the 3x3 coefficient
// matrix in the linear system that defines A, B, and C is nearly singular.

template <class Real>
WML_ITEM bool HeightPlaneFit (int iQuantity, Vector3<Real>* akPoint,
    Real& rfA, Real& rfB, Real& rfC);


// Least-squares fit of a plane to (x,y,z) data by using distance measurements
// orthogonal to the proposed plane.  The resulting plane is represented by
// Normal.Dot(X - Offset) = 0.  The return value of the function is the
// associated minimum in the function that was minimized.

template <class Real>
WML_ITEM Real OrthogonalPlaneFit (int iQuantity, Vector3<Real>* akPoint,
    Vector3<Real>& rkOffset, Vector3<Real>& rkNormal);

}

#endif
