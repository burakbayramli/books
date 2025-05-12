// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPAKIMA1_H
#define WMLINTPAKIMA1_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpAkima1
{
public:
    // abstract base class
    virtual ~IntpAkima1 ();

    class WML_ITEM Polynomial
    {
    public:
        // P(x) = A[0]+A[1]*x+A[2]*x^2+A[3]*x^3
        Real& operator[] (int i) { return m_afA[i]; }

        Real operator() (Real fX) const
        {
            return m_afA[0]+fX*(m_afA[1]+fX*(m_afA[2]+fX*m_afA[3]));
        }

        Real operator() (int iOrder, Real fX) const
        {
            switch ( iOrder )
            {
            case 0:
                return m_afA[0]+fX*(m_afA[1]+fX*(m_afA[2]+fX*m_afA[3]));
            case 1:
                return m_afA[1]+fX*(((Real)2.0)*m_afA[2] +
                    fX*((Real)3.0)*m_afA[3]);
            case 2:
                return ((Real)2.0)*m_afA[2]+fX*((Real)6.0)*m_afA[3];
            case 3:
                return ((Real)6.0)*m_afA[3];
            default:
                return (Real)0.0f;
            }
        }

    private:
        Real m_afA[4];
    };

    int GetQuantity () const;
    const Real* GetF () const;
    const Polynomial* GetPolynomials () const;
    const Polynomial& GetPolynomial (int i) const;

    virtual Real GetXMin () const = 0;
    virtual Real GetXMax () const = 0;

    // Evaluate the function and its derivatives.  The application is
    // responsible for ensuring that xmin <= x <= xmax.  If x is outside the
    // extremes, the function returns MAXREAL.  The first operator is for
    // function evaluation.  The second operator is for function or derivative
    // evaluations.  The uiOrder argument is the order of the derivative, zero
    // for the function itself.
    Real operator() (Real fX) const;
    Real operator() (int iOrder, Real fX) const;

protected:
    // Construction.  IntpAkima1 does not accept responsibility for
    // deleting the input array.  The application must do so.
    IntpAkima1 (int iQuantity, Real* afF);

    Real ComputeDerivative (Real* afSlope) const;
    virtual bool Lookup (Real fX, int& riIndex, Real& rfDX) const = 0;

    int m_iQuantity;
    Real* m_afF;
    Polynomial* m_akPoly;
};

typedef IntpAkima1<float> IntpAkima1f;
typedef IntpAkima1<double> IntpAkima1d;

}

#endif
