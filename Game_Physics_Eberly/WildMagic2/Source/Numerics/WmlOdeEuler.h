// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLODEEULER_H
#define WMLODEEULER_H

#include "WmlOdeSolver.h"

namespace Wml
{

template <class Real>
class WML_ITEM OdeEuler : public OdeSolver<Real>
{
public:
    OdeEuler (int iDim, Real fStep, typename OdeSolver<Real>::Function* aoF,
        void* pvData = NULL);

    virtual void Update (Real fTIn, Real* afXIn, Real& rfTOut,
        Real* afXOut);

    virtual void SetStepSize (Real fStep);
};

typedef OdeEuler<float> OdeEulerf;
typedef OdeEuler<double> OdeEulerd;

}

#endif
