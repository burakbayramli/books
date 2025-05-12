// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntegrate1.h"
namespace Wml
{
template <class Real>
class Integrate1InitTerm
{
public:
    Integrate1InitTerm () { Integrate1<Real>::Initialize(); }
    ~Integrate1InitTerm () { Integrate1<Real>::Terminate(); }
};
}
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Integrate1<Real>::Initialize ()
{
    ms_iOrder = 5;
    ms_apfRom[0] = new Real[ms_iOrder];
    ms_apfRom[1] = new Real[ms_iOrder];
}
//----------------------------------------------------------------------------
template <class Real>
void Integrate1<Real>::Terminate ()
{
    delete[] ms_apfRom[0];
    delete[] ms_apfRom[1];
    ms_apfRom[0] = NULL;
    ms_apfRom[1] = NULL;
}
//----------------------------------------------------------------------------
template <class Real>
void Integrate1<Real>::SetOrder (int iOrder)
{
    ms_iOrder = iOrder;
    delete[] ms_apfRom[0];
    delete[] ms_apfRom[1];
    ms_apfRom[0] = new Real[ms_iOrder];
    ms_apfRom[1] = new Real[ms_iOrder];
}
//----------------------------------------------------------------------------
template <class Real>
int Integrate1<Real>::GetOrder ()
{
    return ms_iOrder;
}
//----------------------------------------------------------------------------
template <class Real>
Real Integrate1<Real>::RombergIntegral (Real fA, Real fB, Function oF,
    void* pvUserData)
{
    Real fH = fB - fA;

    ms_apfRom[0][0] = ((Real)0.5)*fH*(oF(fA,pvUserData)+oF(fB,pvUserData));
    for (int i0=2, iP0=1; i0 <= ms_iOrder; i0++, iP0 *= 2, fH *= (Real)0.5)
    {
        // approximations via the trapezoid rule
        Real fSum = (Real)0.0;
        int i1;
        for (i1 = 1; i1 <= iP0; i1++)
            fSum += oF(fA + fH*(i1-((Real)0.5)),pvUserData);

        // Richardson extrapolation
        ms_apfRom[1][0] = ((Real)0.5)*(ms_apfRom[0][0] + fH*fSum);
        for (int i2 = 1, iP2 = 4; i2 < i0; i2++, iP2 *= 4)
        {
            ms_apfRom[1][i2] =
                (iP2*ms_apfRom[1][i2-1] - ms_apfRom[0][i2-1])/(iP2-1);
        }

        for (i1 = 0; i1 < i0; i1++)
            ms_apfRom[0][i1] = ms_apfRom[1][i1];
    }

    return ms_apfRom[0][ms_iOrder-1];
}
//----------------------------------------------------------------------------
template <class Real>
Real Integrate1<Real>::GaussianQuadrature (Real fA, Real fB, Function oF,
    void* pvUserData)
{
    // Legendre polynomials
    // P_0(x) = 1
    // P_1(x) = x
    // P_2(x) = (3x^2-1)/2
    // P_3(x) = x(5x^2-3)/2
    // P_4(x) = (35x^4-30x^2+3)/8
    // P_5(x) = x(63x^4-70x^2+15)/8

    // generation of polynomials
    //   d/dx[ (1-x^2) dP_n(x)/dx ] + n(n+1) P_n(x) = 0
    //   P_n(x) = sum_{k=0}^{floor(n/2)} c_k x^{n-2k}
    //     c_k = (-1)^k (2n-2k)! / [ 2^n k! (n-k)! (n-2k)! ]
    //   P_n(x) = ((-1)^n/(2^n n!)) d^n/dx^n[ (1-x^2)^n ]
    //   (n+1)P_{n+1}(x) = (2n+1) x P_n(x) - n P_{n-1}(x)
    //   (1-x^2) dP_n(x)/dx = -n x P_n(x) + n P_{n-1}(x)

    // roots of the Legendre polynomial of specified degree
    const int iDegree = 5;
    static const Real s_afRoot[iDegree] =
    {
        (Real)-0.9061798459,
        (Real)-0.5384693101,
        (Real) 0.0,
        (Real)+0.5384693101,
        (Real)+0.9061798459
    };
    static const Real s_afCoeff[iDegree] =
    {
        (Real)0.2369268850,
        (Real)0.4786286705,
        (Real)0.5688888889,
        (Real)0.4786286705,
        (Real)0.2369268850
    };

    // Need to transform domain [a,b] to [-1,1].  If a <= x <= b and
    // -1 <= t <= 1, then x = ((b-a)*t+(b+a))/2.
    Real fRadius = ((Real)0.5)*(fB - fA);
    Real fCenter = ((Real)0.5)*(fB + fA);

    Real fResult = (Real)0.0;
    for (int i = 0; i < iDegree; i++)
        fResult += s_afCoeff[i]*oF(fRadius*s_afRoot[i]+fCenter,pvUserData);
    fResult *= fRadius;

    return fResult;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Integrate1<float>;
int Integrate1f::ms_iOrder;
float* Integrate1f::ms_apfRom[2];
static Integrate1InitTerm<float> gs_kForceIntegrateInitTermf;

template class WML_ITEM Integrate1<double>;
int Integrate1d::ms_iOrder;
double* Integrate1d::ms_apfRom[2];
static Integrate1InitTerm<double> gs_kForceIntegrateInitTermd;
}
//----------------------------------------------------------------------------
