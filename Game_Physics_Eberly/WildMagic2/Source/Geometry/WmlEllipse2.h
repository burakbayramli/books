// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLELLIPSE2_H
#define WMLELLIPSE2_H

#include "WmlMatrix2.h"

namespace Wml
{

template <class Real>
class WML_ITEM EllipseStandard2
{
public:
    // (x/a)^2 + (y/b)^2 = 1

    EllipseStandard2 ();

    Real& Extent (int i);
    const Real& Extent (int i) const;
    Real* Extents ();
    const Real* Extents () const;

protected:
    Real m_afExtent[2];
};

typedef EllipseStandard2<float> EllipseStandard2f;
typedef EllipseStandard2<double> EllipseStandard2d;


template <class Real>
class WML_ITEM Ellipse2
{
public:
    // center-matrix form, (X-C)^T A (X-C) = 1, where A is a positive
    // definite matrix

    Ellipse2 ();

    Vector2<Real>& Center ();
    const Vector2<Real>& Center () const;

    Matrix2<Real>& A ();
    const Matrix2<Real>& A () const;

protected:
    Vector2<Real> m_kCenter;
    Matrix2<Real> m_kA;
};

typedef Ellipse2<float> Ellipse2f;
typedef Ellipse2<double> Ellipse2d;


template <class Real>
class WML_ITEM EllipseGeneral2
{
public:
    // Ellipse is X^T A X + B^T X + C = 0 where A is symmetric 2x2, B and
    // X are 2x1, and C is a scalar.  Since A is symmetric, it is not
    // necessary to store all 4 values, but I use it for convenience of doing
    // matrix/vector arithmetic.
    Matrix2<Real> m_kA;
    Vector2<Real> m_kB;
    Real m_fC;
};

typedef EllipseGeneral2<float> EllipseGeneral2f;
typedef EllipseGeneral2<double> EllipseGeneral2d;

}

#endif
