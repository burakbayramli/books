// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTPVECTORFIELD2_H
#define WMLINTPVECTORFIELD2_H

// Given points (x0[i],y0[i]) which are mapped to (x1[i],y1[i]) for
// 0 <= i < N, interpolate positions (xIn,yIn) to (xOut,yOut).

#include "WmlIntpQdrNonuniform2.h"

namespace Wml
{

template <class Real>
class WML_ITEM IntpVectorField2
{
public:
    // Construction and destruction.  IntpVectorField2 accepts ownership
    // of the input arrays and will delete them during destruction.
    IntpVectorField2 (int iQuantity, Vector2<Real>* akDomain,
        Vector2<Real>* akRange);
    ~IntpVectorField2 ();

    // Return 'true' if and only if (xIn,yIn) is in the convex hull of the
    // input points.  In this case, (xOut,yOut) is valid.  If the return
    // value is 'false', (xOut,yOut) is invalid and should not be used.
    bool Evaluate (const Vector2<Real>& rkInput, Vector2<Real>& rkOutput); 

protected:
    IntpQdrNonuniform2<Real>* m_pkXInterp;
    IntpQdrNonuniform2<Real>* m_pkYInterp;
};

typedef IntpVectorField2<float> IntpVectorField2f;
typedef IntpVectorField2<double> IntpVectorField2d;

}

#endif
