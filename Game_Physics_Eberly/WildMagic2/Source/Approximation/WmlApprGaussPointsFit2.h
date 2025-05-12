// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRGAUSSPOINTSFIT2_H
#define WMLAPPRGAUSSPOINTSFIT2_H

// Fit points with a Gaussian distribution.  The center is the mean of the
// points, the axes are the eigenvectors of the covariance matrix, and the
// extents are the eigenvalues of the covariance matrix and are returned in
// increasing order.  The last function allows selection of valid vertices
// from a pool.  The return value is 'true' if and only if at least
// one vertex was valid.

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
WML_ITEM void GaussPointsFit (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkCenter, Vector2<Real> akAxis[2], Real afExtent[2]);

template <class Real>
WML_ITEM bool GaussPointsFit (int iQuantity, const Vector2<Real>* akPoint,
    const bool* abValid, Vector2<Real>& rkCenter, Vector2<Real> akAxis[2],
    Real afExtent[2]);

}

#endif
