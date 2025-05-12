// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPBSPLINEUNIFORM2_H
#define WMLINTPBSPLINEUNIFORM2_H

#include "WmlIntpBSplineUniform.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpBSplineUniform2 : public IntpBSplineUniform<Real>
{
public:
    // Construction.  IntpBSplineUniform2 accepts responsibility for
    // deleting the input array afData.
    IntpBSplineUniform2 (int iDegree, const int* aiDim, Real* afData);

    int Index (int iX, int iY) const;

    // spline evaluation for function interpolation (no derivatives)
    Real operator() (Real fX, Real fY);
    virtual Real operator() (Real* afX);

    // spline evaluation, derivative counts given in iDx, iDy, aiDx[]
    Real operator() (int iDx, int iDy, Real fX, Real fY);
    virtual Real operator() (int* aiDx, Real* afX);

private:
    void EvaluateUnknownData ();
    void ComputeIntermediate ();
};

typedef IntpBSplineUniform2<float> IntpBSplineUniform2f;
typedef IntpBSplineUniform2<double> IntpBSplineUniform2d;

}

#endif
