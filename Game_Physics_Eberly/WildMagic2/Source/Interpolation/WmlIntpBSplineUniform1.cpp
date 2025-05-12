// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpBSplineUniform1.h"
#include "WmlMath.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpBSplineUniform1<Real>::IntpBSplineUniform1 (int iDegree, int iDim,
    Real* afData)
    :
    IntpBSplineUniform<Real>(1,iDegree,&iDim,afData)
{
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBSplineUniform1<Real>::operator() (Real* afX)
{
    return (*this)(*afX);
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBSplineUniform1<Real>::operator() (int* aiDx, Real* afX)
{
    return (*this)(*aiDx,*afX);
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBSplineUniform1<Real>::operator() (Real fX)
{
    m_aiBase[0] = (int)Math<Real>::Floor(fX);
    if ( m_aiOldBase[0] != m_aiBase[0] )
    {
        // switch to new local grid
        m_aiOldBase[0] = m_aiBase[0];
        m_aiGridMin[0] = m_aiBase[0] - 1;
        m_aiGridMax[0] = m_aiGridMin[0] + m_iDegree;

        // fill in missing grid data if necessary
        if ( m_oEvaluateCallback )
            EvaluateUnknownData();

        ComputeIntermediate();
    }

    SetPolynomial(0,fX-m_aiBase[0],m_aafPoly[0]);

    Real fResult = (Real)0.0;
    for (int k = 0; k <= m_iDegree; k++)
        fResult += m_aafPoly[0][k]*m_afInter[k];
    return fResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBSplineUniform1<Real>::operator() (int iDx, Real fX)
{
    m_aiBase[0] = (int)Math<Real>::Floor(fX);
    if ( m_aiOldBase[0] != m_aiBase[0] )
    {
        // switch to new local grid
        m_aiOldBase[0] = m_aiBase[0];
        m_aiGridMin[0] = m_aiBase[0] - 1;
        m_aiGridMax[0] = m_aiGridMin[0] + m_iDegree;

        // fill in missing grid data if necessary
        if ( m_oEvaluateCallback )
            EvaluateUnknownData();

        ComputeIntermediate();
    }

    SetPolynomial(iDx,fX-m_aiBase[0],m_aafPoly[0]);

    Real fResult = (Real)0.0;
    for (int k = iDx; k <= m_iDegree; k++)
        fResult += m_aafPoly[0][k]*m_afInter[k];
    return fResult;
}
//----------------------------------------------------------------------------
template <class Real>
void IntpBSplineUniform1<Real>::EvaluateUnknownData ()
{
    for (int k = m_aiGridMin[0]; k <= m_aiGridMax[0]; k++)
    {
        if ( m_afData[k] == Math<Real>::MAX_REAL )
            m_afData[k] = m_oEvaluateCallback(k);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void IntpBSplineUniform1<Real>::ComputeIntermediate ()
{
    for (int k = 0; k <= m_iDegree; k++)
    {
        m_afInter[k] = (Real)0.0;
        for (int i = 0, j = m_aiBase[0]-1; i <= m_iDegree; i++, j++)
            m_afInter[k] += m_afData[j]*m_aafMatrix[i][k];
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpBSplineUniform1<float>;
template class WML_ITEM IntpBSplineUniform1<double>;
}
//----------------------------------------------------------------------------
