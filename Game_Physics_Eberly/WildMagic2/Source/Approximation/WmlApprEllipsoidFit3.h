// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRELLIPSOIDFIT3_H
#define WMLAPPRELLIPSOIDFIT3_H

// The ellipsoid in general form is  X^t A X + B^t X + C = 0 where
// A is a positive definite 3x3 matrix, B is a 3x1 vector, C is a
// scalar, and X is a 3x1 vector.  Completing the square,
// (X-U)^t A (X-U) = U^t A U - C where U = -0.5 A^{-1} B.  Define
// M = A/(U^t A U - C).  The ellipsoid is (X-U)^t M (X-U) = 1.  Factor
// M = R^t D R where R is orthonormal and D is diagonal with positive
// diagonal terms.  The ellipsoid in factored form is
//     (X-U)^t R^t D^t R (X-U) = 1
//
// Find the least squares fit of a set of N points P[0] through P[N-1].
// Return value is the least-squares energy function at (U,R,D).

#include "WmlMatrix3.h"

namespace Wml
{

template <class Real>
class WML_ITEM EllipsoidFit3
{
public:
    EllipsoidFit3 (int iQuantity, const Vector3<Real>* akPoint,
        Vector3<Real>& rkU, Matrix3<Real>& rkR, Real afD[3], Real& rfError);

private:
    static void InitialGuess (int iQuantity, const Vector3<Real>* akPoint,
        Vector3<Real>& rkU, Matrix3<Real>& rkR, Real afD[3]);

    static Real Energy (const Real* afV, void* pvData);

    static void MatrixToAngles (const Matrix3<Real>& rkR, Real* afAngle);
    static void AnglesToMatrix (const Real* afAngle, Matrix3<Real>& rkR);

    int m_iQuantity;
    const Vector3<Real>* m_akPoint;
    Vector3<Real>* m_akTemp;
};

typedef EllipsoidFit3<float> EllipsoidFit3f;
typedef EllipsoidFit3<double> EllipsoidFit3d;

}

#endif
