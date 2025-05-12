// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPERSPPROJELLIPSOID_H
#define WMLPERSPPROJELLIPSOID_H

#include "WmlEllipse2.h"
#include "WmlEllipsoid3.h"
#include "WmlPlane3.h"

namespace Wml
{

// Input:
//   eyepoint, E
//   view plane, N^t X = D, planar vectors U and V so that {U,V,N} is an
//      orthonormal set of vectors
//   ellipsoid, X^t A X + B^t X + C = 0
// where E, N, B, and X are 3x1, A is symmetric 3x3, C and D are scalars
//
// Output:
//   plane origin, P = E + (D - N^t E)*N
//   projected ellipse, Y^t A Y + B^t Y + C = 0
// where Y and B are 2x1, A is symmetric 2x2, and C is a scalar.  The
// projected ellipse variable Y = (s,t) are the coordinates of the X
// ellipse variable specified as X = P + s*U + t*V where {U,V,N} is a
// right-handed orthonormal system and P the perspective projection of
// E onto the view plane.

template <class Real>
WML_ITEM void PerspProjEllipsoid (const EllipsoidGeneral3<Real>& rkEllipsoid,
    const Vector3<Real>& rkEye, const Plane3<Real>& rkPlane,
    const Vector3<Real>& rkU, const Vector3<Real>& rkV, Vector3<Real>& rkP,
    EllipseGeneral2<Real>& rkEllipse);


// Compute center, axes, and axis half lengths for the ellipse
// Y^t A Y + B^t Y + C = 0.  If the center is K = (sk,tk), then in the
// original 3D coordinates the center is P+sk*U+tk*V.  If an axis is
// W = (sw,tw), then in the original 3D coordinates it is sw*U+tw*V.

template <class Real>
WML_ITEM void ConvertEllipse (EllipseGeneral2<Real>& rkEllipse,
    Vector2<Real>& rkCenter, Vector2<Real>& rkAxis0, Vector2<Real>& rkAxis1,
    Real& rfHalfLength0, Real& rfHalfLength1);

}

#endif
