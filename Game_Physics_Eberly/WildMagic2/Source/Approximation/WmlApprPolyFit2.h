// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRPOLYFIT2_H
#define WMLAPPRPOLYFIT2_H

#include "WmlSystem.h"

namespace Wml
{

// The samples are (x[i],y[i],w[i]) for 0 <= i < S.  Think of w as a function
// of x and y, say w = f(x,y).  The function fits the samples with a
// polynomial of degree dx in x and degree dy in y, say
//   w = sum_{i=0}^{dx} sum_{j=0}^{dy} c[i][j]*x^i*y^j
// The method is a least-squares fitting algorithm.  The output parameter
// riQuantity is (dx+1)*(dy+1), the number of polynomial coefficients.  The
// output array rafC[] stores the c[i][j] values according to
//   rafC[n] = c[i][j], n = i+(dx+1)*j
// The caller is responsible for deleting the input arrays if they were
// dynamically allocated.  The caller is also responsible for deleting the
// output array.

template <class Real>
WML_ITEM void PolyFit2 (int iSamples, const Real* afX, const Real* afY,
    const Real* afW, int iXDegree, int iYDegree, int& riQuantity,
    Real*& rafC);

}

#endif
