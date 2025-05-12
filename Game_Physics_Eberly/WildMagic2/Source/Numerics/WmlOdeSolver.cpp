// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlOdeSolver.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
OdeSolver<Real>::OdeSolver (int iDim, Real fStep, Function* aoF,
    void* pvData)
{
    m_iDim = iDim;
    m_fStep = fStep;
    m_aoF = aoF;
    m_pvData = pvData;
}
//----------------------------------------------------------------------------
template <class Real>
OdeSolver<Real>::~OdeSolver ()
{
}
//----------------------------------------------------------------------------
template <class Real>
Real OdeSolver<Real>::GetStepSize () const
{
    return m_fStep;
}
//----------------------------------------------------------------------------
template <class Real>
void OdeSolver<Real>::SetData (void* pvData)
{
    m_pvData = pvData;
}
//----------------------------------------------------------------------------
template <class Real>
void* OdeSolver<Real>::GetData () const
{
    return m_pvData;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM OdeSolver<float>;
template class WML_ITEM OdeSolver<double>;
}
//----------------------------------------------------------------------------
