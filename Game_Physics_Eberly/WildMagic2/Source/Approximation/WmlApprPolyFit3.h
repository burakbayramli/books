// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRPOLYFIT3_H
#define WMLAPPRPOLYFIT3_H

#include "WmlSystem.h"

namespace Wml
{

// The samples are (x[i],y[i],z[i],w[i]) for 0 <= i < S.  Think of w as a
// function of x, y, and z, say w = f(x,y,z).  The function fits the samples
// with a polynomial of degree dx in x, degree dy in y, and degree dz in z,
// say
//   w = sum_{i=0}^{dx} sum_{j=0}^{dy} sum_{k=0}^{dz} c[i][j][k]*x^i*y^j*z^k
// The method is a least-squares fitting algorithm.  The output parameter
// riQuantity is (dx+1)*(dy+1)*(dz+1), the number of polynomial coefficients.
// The output array rafC[] stores the c[i][j][k] values according to
//   rafC[n] = c[i][j][k], n = i+(dx+1)*(j+(dy+1)*k)
// The caller is responsible for deleting the input arrays if they were
// dynamically allocated.  The caller is also responsible for deleting the
// output array.

template <class Real>
WML_ITEM void PolyFit3 (int iSamples, const Real* afX, const Real* afY,
    const Real* afZ, const Real* afW, int iXDegree, int iYDegree,
    int iZDegree, int& riQuantity, Real*& rafC);

}

#endif
