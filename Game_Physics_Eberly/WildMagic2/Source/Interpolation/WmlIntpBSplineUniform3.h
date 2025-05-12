// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPBSPLINEUNIFORM3_H
#define WMLINTPBSPLINEUNIFORM3_H

#include "WmlIntpBSplineUniform.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpBSplineUniform3 : public IntpBSplineUniform<Real>
{
public:
    // Construction.  IntpBSplineUniform3 accepts responsibility for
    // deleting the input array afData.
    IntpBSplineUniform3 (int iDegree, const int* aiDim, Real* afData);

    int Index (int iX, int iY, int iZ) const;

    // spline evaluation for function interpolation (no derivatives)
    Real operator() (Real fX, Real fY, Real fZ);
    virtual Real operator() (Real* afX);

    // spline evaluation, derivative counts given in iDx, iDy, iDz, aiDx[]
    Real operator() (int iDx, int iDy, int iDz, Real fX, Real fY, Real fZ);
    virtual Real operator() (int* aiDx, Real* afX);

private:
    void EvaluateUnknownData ();
    void ComputeIntermediate ();
};

typedef IntpBSplineUniform3<float> IntpBSplineUniform3f;
typedef IntpBSplineUniform3<double> IntpBSplineUniform3d;

}

#endif
