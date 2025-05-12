// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPR2DLINEFIT_H
#define WMLAPPR2DLINEFIT_H

#include "WmlVector2.h"

namespace Wml
{

// Least-squares fit of a line to (x,f(x)) data by using distance
// measurements in the y-direction.  The resulting line is represented by
// y = A*x + B.  The return value is 'false' if the 2x2 coefficient matrix
// in the linear system that defines A and B is nearly singular.

template <class Real>
WML_ITEM bool HeightLineFit (int iQuantity, const Vector2<Real>* akPoint,
    Real& rfA, Real& rfB);


// Least-squares fit of a line to (x,y) data by using distance measurements
// orthogonal to the proposed line.  The resulting line is represented by
// Offset + t*Direction where the returned direction is a unit-length vector.

template <class Real>
WML_ITEM void OrthogonalLineFit (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkOffset, Vector2<Real>& rkDirection);


// This function allows for selection of vertices from a pool.  The return
// value is 'true' if and only if at least one vertex is valid.

template <class Real>
WML_ITEM bool OrthogonalLineFit (int iQuantity, const Vector2<Real>* akPoint,
    const bool* abValid, Vector2<Real>& rkOffset, Vector2<Real>& rkDirection);

}

#endif
