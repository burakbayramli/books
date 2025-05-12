// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLELLIPSOID3_H
#define WMLELLIPSOID3_H

#include "WmlMatrix3.h"

namespace Wml
{

template <class Real>
class WML_ITEM EllipsoidStandard3
{
public:
    // (x/a)^2 + (y/b)^2 + (z/c)^2 = 1

    EllipsoidStandard3 ();

    Real& Extent (int i);
    const Real& Extent (int i) const;
    Real* Extents ();
    const Real* Extents () const;

protected:
    Real m_afExtent[3];
};

typedef EllipsoidStandard3<float> EllipsoidStandard3f;
typedef EllipsoidStandard3<double> EllipsoidStandard3d;


template <class Real>
class WML_ITEM Ellipsoid3
{
public:
    // center-matrix form, (X-C)^T A (X-C) = 1, where A is a positive
    // definite matrix

    Ellipsoid3 ();

    Vector3<Real>& Center ();
    const Vector3<Real>& Center () const;

    Matrix3<Real>& A ();
    const Matrix3<Real>& A () const;

    Matrix3<Real>& InverseA ();
    const Matrix3<Real>& InverseA () const;

protected:
    Vector3<Real> m_kCenter;
    Matrix3<Real> m_kA;
    Matrix3<Real> m_kInverseA;  // for intersection and culling support
};

typedef Ellipsoid3<float> Ellipsoid3f;
typedef Ellipsoid3<double> Ellipsoid3d;


template <class Real>
class WML_ITEM EllipsoidGeneral3
{
public:
    // Ellipsoid is X^T A X + B^T X + C = 0 where A is symmetric 3x3, B and
    // X are 3x1, and C is a scalar.  Since A is symmetric, it is not
    // necessary to store all 9 values, but I use it for convenience of doing
    // matrix/vector arithmetic.
    Matrix3<Real> m_kA;
    Vector3<Real> m_kB;
    Real m_fC;
};

typedef EllipsoidGeneral3<float> EllipsoidGeneral3f;
typedef EllipsoidGeneral3<double> EllipsoidGeneral3d;

}

#endif
