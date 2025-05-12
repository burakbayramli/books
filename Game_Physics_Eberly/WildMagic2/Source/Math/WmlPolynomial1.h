// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPOLYNOMIAL1_H
#define WMLPOLYNOMIAL1_H

#include "WmlMath.h"

namespace Wml
{

template <class Real>
class WML_ITEM Polynomial1
{
public:
    // construction and destruction
    Polynomial1 (int iDegree = -1);
    Polynomial1 (const Polynomial1& rkPoly);
    ~Polynomial1 ();

    // member access
    void SetDegree (int iDegree);
    int GetDegree () const;
    operator const Real* () const;
    operator Real* ();
    Real operator[] (int i) const;
    Real& operator[] (int i);

    // assignment
    Polynomial1& operator= (const Polynomial1& rkPoly);

    // evaluation
    Real operator() (Real fT) const;

    // arithmetic operations
    Polynomial1 operator+ (const Polynomial1& rkPoly) const;
    Polynomial1 operator- (const Polynomial1& rkPoly) const;
    Polynomial1 operator* (const Polynomial1& rkPoly) const;
    Polynomial1 operator+ (Real fScalar) const;  // input is degree 0 poly
    Polynomial1 operator- (Real fScalar) const;  // input is degree 0 poly
    Polynomial1 operator* (Real fScalar) const;
    Polynomial1 operator/ (Real fScalar) const;
    Polynomial1 operator- () const;

    // arithmetic updates
    Polynomial1& operator += (const Polynomial1& rkPoly);
    Polynomial1& operator -= (const Polynomial1& rkPoly);
    Polynomial1& operator *= (const Polynomial1& rkPoly);
    Polynomial1& operator += (Real fScalar);  // input is degree 0 poly
    Polynomial1& operator -= (Real fScalar);  // input is degree 0 poly
    Polynomial1& operator *= (Real fScalar);
    Polynomial1& operator /= (Real fScalar);

    // derivation
    Polynomial1 GetDerivative () const;

    // inversion ( invpoly[i] = poly[degree-i] for 0 <= i <= degree )
    Polynomial1 GetInversion () const;

    // Reduce degree by eliminating all (nearly) zero leading coefficients
    // and by making the leading coefficient one.  The input parameter is
    // the threshold for specifying that a coefficient is effectively zero.
    void Compress (Real fEpsilon);

    // If 'this' is P(t) and the divisor is D(t) with degree(P) >= degree(D),
    // then P(t) = Q(t)*D(t)+R(t) where Q(t) is the quotient with
    // degree(Q) = degree(P) - degree(D) and R(t) is the remainder with
    // degree(R) < degree(D).  If this routine is called with
    // degree(P) < degree(D), then Q = 0 and R = P are returned.  The value
    // of epsilon is used as a threshold on the coefficients of the remainder
    // polynomial.  If smaller, the coefficient is assumed to be zero.
    void Divide (const Polynomial1& rkDiv, Polynomial1& rkQuot,
        Polynomial1& rkRem, Real fEpsilon) const;

protected:
    int m_iDegree;
    Real* m_afCoeff;
};

template <class Real>
WML_ITEM Polynomial1<Real> operator* (Real fScalar,
    const Polynomial1<Real>& rkPoly);

typedef Polynomial1<float> Polynomial1f;
typedef Polynomial1<double> Polynomial1d;

}

#endif
