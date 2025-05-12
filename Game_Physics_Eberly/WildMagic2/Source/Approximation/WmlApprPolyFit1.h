// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRPOLYFIT1_H
#define WMLAPPRPOLYFIT1_H

#include "WmlSystem.h"

namespace Wml
{

// The samples are (x[i],w[i]) for 0 <= i < S.  Think of w as a function of
// x, say w = f(x).  The function fits the samples with a polynomial of
// degree d, say w = sum_{i=0}^d c[i]*x^i.  The method is a least-squares
// fitting algorithm.  The output parameter riQuantity is d+1.  The output
// array rafC[] stores the c[i] values.  The caller is responsible for
// deleting the input arrays if they were dynamically allocated.  The caller
// is also responsible for deleting the output array.

template <class Real>
WML_ITEM void PolyFit1 (int iSamples, const Real* afX, const Real* afW,
    int iXDegree, int& riQuantity, Real*& rafC);

}

#endif
