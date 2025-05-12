// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlOdeMidpoint.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
OdeMidpoint<Real>::OdeMidpoint (int iDim, Real fStep,
    typename OdeSolver<Real>::Function* aoF, void* pvData)
    :
    OdeSolver<Real>(iDim,fStep,aoF,pvData)
{
    m_fHalfStep = ((Real)0.5)*fStep;
    m_afXTemp = new Real[iDim];
}
//----------------------------------------------------------------------------
template <class Real>
OdeMidpoint<Real>::~OdeMidpoint ()
{
    delete[] m_afXTemp;
}
//----------------------------------------------------------------------------
template <class Real>
void OdeMidpoint<Real>::Update (Real fTIn, Real* afXIn, Real& rfTOut,
    Real* afXOut)
{
    // first step
    int i;
    for (i = 0; i < m_iDim; i++)
        m_afXTemp[i] = afXIn[i] + m_fHalfStep*m_aoF[i](fTIn,afXIn,m_pvData);

    // second step
    Real fHalfT = fTIn + m_fHalfStep;
    for (i = 0; i < m_iDim; i++)
        afXOut[i] = afXIn[i] + m_fStep*m_aoF[i](fHalfT,m_afXTemp,m_pvData);

    rfTOut = fTIn + m_fStep;
}
//----------------------------------------------------------------------------
template <class Real>
void OdeMidpoint<Real>::SetStepSize (Real fStep)
{
    m_fStep = fStep;
    m_fHalfStep = ((Real)0.5)*fStep;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM OdeMidpoint<float>;
template class WML_ITEM OdeMidpoint<double>;
}
//----------------------------------------------------------------------------
