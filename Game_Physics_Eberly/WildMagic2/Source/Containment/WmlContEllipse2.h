// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTELLIPSE2_H
#define WMLCONTELLIPSE2_H

// The input points are fit with a Gaussian distribution.  The center C of the
// ellipse is chosen to be the mean of the distribution.  The axes of the
// ellipse are chosen to be the eigenvectors of the covariance matrix M.  The
// returned ellipse is of the form (X-C)^T*(M^{-1}/V)*(X-C) = 1 for an
// appropriately chosen V > 0.
//
// WARNING.  The construction is ill-conditioned if the points are (nearly)
// collinear.  In this case M has a (nearly) zero eigenvalue, so inverting M
// is problematic.

#include "WmlEllipse2.h"

namespace Wml
{

template <class Real>
WML_ITEM Ellipse2<Real> ContEllipse (int iQuantity,
    const Vector2<Real>* akPoint);

}

#endif
