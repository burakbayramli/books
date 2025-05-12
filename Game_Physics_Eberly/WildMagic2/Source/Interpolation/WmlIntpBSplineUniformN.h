// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPBSPLINEUNIFORMN_H
#define WMLINTPBSPLINEUNIFORMN_H

#include "WmlIntpBSplineUniform.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpBSplineUniformN : public IntpBSplineUniform<Real>
{
public:
    // Construction and destruction.  IntpBSplineUniformN accepts
    // responsibility for deleting the input array afData.  The input array
    // aiDim is copied.
    IntpBSplineUniformN (int iDims, int iDegree, const int* aiDim,
        Real* afData);
    virtual ~IntpBSplineUniformN ();

    int Index (int* aiI) const;

    // spline evaluation for function interpolation (no derivatives)
    virtual Real operator() (Real* afX);

    // spline evaluation, derivative counts given in aiDx[]
    virtual Real operator() (int* aiDx, Real* afX);

private:
    int* m_aiEvI;
    int* m_aiCiLoop;
    int* m_aiCiDelta;
    int* m_aiOpI;
    int* m_aiOpJ;
    int* m_aiOpDelta;

    void EvaluateUnknownData ();
    void ComputeIntermediate ();
};

typedef IntpBSplineUniformN<float> IntpBSplineUniformNf;
typedef IntpBSplineUniformN<double> IntpBSplineUniformNd;

}

#endif
