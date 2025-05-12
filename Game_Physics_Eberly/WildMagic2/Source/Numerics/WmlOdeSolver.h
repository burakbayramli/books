// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLODESOLVER_H
#define WMLODESOLVER_H

#include "WmlSystem.h"

namespace Wml
{

template <class Real>
class WML_ITEM OdeSolver
{
public:
    // abstract base class
    virtual ~OdeSolver ();

    // for dx/dt = F(t,x) , the void* parameter is for user data
    typedef Real (*Function)(Real,Real*,void*);
    virtual void Update (Real fTIn, Real* afXIn, Real& rfTOut,
        Real* afXOut) = 0;

    virtual void SetStepSize (Real fStep) = 0;
    Real GetStepSize () const;

    void SetData (void* pvData);
    void* GetData () const;

protected:
    OdeSolver (int iDim, Real fStep, Function* aoF, void* pvData = NULL);

    int m_iDim;
    Real m_fStep;
    Function* m_aoF;
    void* m_pvData;
};

typedef OdeSolver<float> OdeSolverf;
typedef OdeSolver<double> OdeSolverd;

}

#endif
