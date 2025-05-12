// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPBSPLINE4_H
#define WMLINTPBSPLINE4_H

#include "WmlIntpBSplineUniform.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpBSplineUniform4 : public IntpBSplineUniform<Real>
{
public:
    // Construction.  IntpBSplineUniform4 accepts responsibility for
    // deleting the input array afData.
    IntpBSplineUniform4 (int iDegree, const int* aiDim, Real* afData);

    int Index (int iX, int iY, int iZ, int iW) const;

    // spline evaluation for function interpolation (no derivatives)
    Real operator() (Real fX, Real fY, Real fZ, Real fW);
    virtual Real operator() (Real* afX);

    // spline evaluation, derivative counts given in iDx, iDy, iDz, iDw,
    // aiDx[]
    Real operator() (int iDx, int iDy, int iDz, int iDw, Real fX, Real fY,
        Real fZ, Real fW);
    virtual Real operator() (int* aiDx, Real* afX);

private:
    void EvaluateUnknownData ();
    void ComputeIntermediate ();
};

typedef IntpBSplineUniform4<float> IntpBSplineUniform4f;
typedef IntpBSplineUniform4<double> IntpBSplineUniform4d;

}

#endif
