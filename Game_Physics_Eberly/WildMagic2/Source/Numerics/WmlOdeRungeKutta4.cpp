// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlOdeRungeKutta4.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
OdeRungeKutta4<Real>::OdeRungeKutta4 (int iDim, Real fStep,
    typename OdeSolver<Real>::Function* aoF, void* pvData)
    :
    OdeSolver<Real>(iDim,fStep,aoF,pvData)
{
    m_fHalfStep = ((Real)0.5)*fStep;
    m_fSixthStep = fStep/((Real)6.0);
    m_afTemp1 = new Real[iDim];
    m_afTemp2 = new Real[iDim];
    m_afTemp3 = new Real[iDim];
    m_afTemp4 = new Real[iDim];
    m_afXTemp = new Real[iDim];
}
//----------------------------------------------------------------------------
template <class Real>
OdeRungeKutta4<Real>::~OdeRungeKutta4 ()
{
    delete[] m_afTemp1;
    delete[] m_afTemp2;
    delete[] m_afTemp3;
    delete[] m_afTemp4;
    delete[] m_afXTemp;
}
//----------------------------------------------------------------------------
template <class Real>
void OdeRungeKutta4<Real>::Update (Real fTIn, Real* afXIn, Real& rfTOut,
    Real* afXOut)
{
    // first step
    int i;
    for (i = 0; i < m_iDim; i++)
        m_afTemp1[i] = m_aoF[i](fTIn,afXIn,m_pvData);
    for (i = 0; i < m_iDim; i++)
        m_afXTemp[i] = afXIn[i] + m_fHalfStep*m_afTemp1[i];

    // second step
    Real fHalfT = fTIn + m_fHalfStep;
    for (i = 0; i < m_iDim; i++)
        m_afTemp2[i] = m_aoF[i](fHalfT,m_afXTemp,m_pvData);
    for (i = 0; i < m_iDim; i++)
        m_afXTemp[i] = afXIn[i] + m_fHalfStep*m_afTemp2[i];

    // third step
    for (i = 0; i < m_iDim; i++)
        m_afTemp3[i] = m_aoF[i](fHalfT,m_afXTemp,m_pvData);
    for (i = 0; i < m_iDim; i++)
        m_afXTemp[i] = afXIn[i] + m_fStep*m_afTemp3[i];

    // fourth step
    rfTOut = fTIn + m_fStep;
    for (i = 0; i < m_iDim; i++)
        m_afTemp4[i] = m_aoF[i](rfTOut,m_afXTemp,m_pvData);
    for (i = 0; i < m_iDim; i++)
    {
        afXOut[i] = afXIn[i] + m_fSixthStep*(m_afTemp1[i] +
            ((Real)2.0)*(m_afTemp2[i] + m_afTemp3[i]) + m_afTemp4[i]);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void OdeRungeKutta4<Real>::SetStepSize (Real fStep)
{
    m_fStep = fStep;
    m_fHalfStep = ((Real)0.5)*fStep;
    m_fSixthStep = fStep/((Real)6.0);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM OdeRungeKutta4<float>;
template class WML_ITEM OdeRungeKutta4<double>;
}
//----------------------------------------------------------------------------
