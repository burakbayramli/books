// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTELLIPSOID3_H
#define WMLCONTELLIPSOID3_H

#include "WmlEllipsoid3.h"

namespace Wml
{

// The input points are fit with a Gaussian distribution.  The center C of the
// ellipsoid is chosen to be the mean of the distribution.  The axes of the
// ellipsoid are chosen to be the eigenvectors of the covariance matrix M.
// The returned ellipsoid is of the form (X-C)^T*(M^{-1}/V)*(X-C) = 1 for an
// appropriately chosen V > 0.
//
// WARNING.  The construction is ill-conditioned if the points are (nearly)
// collinear or (nearly) planar.  In this case M has a (nearly) zero
// eigenvalue, so inverting M is problematic.
template <class Real>
WML_ITEM Ellipsoid3<Real> ContEllipsoid (int iQuantity,
    const Vector3<Real>* akPoint);

// An ellipsoid has center C; unit-length axis directions U0, U1, U2; and
// axis lengths L0, L1, and L2.  The set {U0,U1,U2} is orthonormal.  Any point
// X can be represented as
//   X = C + y0*U0 + y1*U1 + y2*U2
// where (y0/L0)^2 + (y1/L1)^2 + (y2/L2)^2 = 1.  If R = [U0 U1 U2] is a 3x3
// rotation matrix whose columns are the Ui vectors, and if Y is the 3x1
// vector whose entries are the yi values, then X = C + R*Y.  Consequently
// Y = R^T*(X-C) where R^T denotes the transpose of R, and
//   1 = Y^T * D * Y = (X-C)^T*R*D*R^T*(X-C) = (X-C)^T*M*(X-C)
// where D = diagonal(1/L0^2,1/L1^2,1/L2^2) and M = R*D*R^T.  The ellipsoid
// can be represented as a center C, a rotation matrix R, and a diagonal
// matrix D.  Observe that the inverse matrix M^{-1} = R*D^{-1}*R^T where
// D^{-1} = diagonal(L0^2,L1^2,L2^2).

// Project an ellipsoid onto a line.  The line contains the point rkOrigin
// and has unit-length direction rkDir.  The ellipsoid has center C (input
// rkC), orientation R (input rkR), and inverse lengths D^{-1} (input afInvD).
// Observe that you must pass D^{-1}, not D.  This is useful if you have to
// project an ellipsoid onto multiple axes.
template <class Real>
WML_ITEM void ProjectEllipsoid (const Vector3<Real>& rkOrigin,
   const Vector3<Real>& rkDir, const Vector3<Real>& rkC,
   const Matrix3<Real>& rkR, const Real afInvD[3], Real& rfMin, Real& rfMax);

// Construct a bounding ellipsoid for the two input ellipsoids.
template <class Real>
WML_ITEM void MergeEllipsoids (const Vector3<Real>& rkC0,
    const Matrix3<Real>& rkR0, const Real afD0[3], const Vector3<Real>& rkC1,
    const Matrix3<Real>& rkR1, const Real afD1[3], Vector3<Real>& rkC,
    Matrix3<Real>& rkR, Real afD[3]);

}

#endif
