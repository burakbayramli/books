// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMultipleCurve2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
MultipleCurve2<Real>::MultipleCurve2 (int iSegments, Real* afTime)
    :
    Curve2<Real>(afTime[0],afTime[iSegments])
{
    m_iSegments = iSegments;
    m_afTime = afTime;
    m_afLength = NULL;
    m_afAccumLength = NULL;
}
//----------------------------------------------------------------------------
template <class Real>
MultipleCurve2<Real>::~MultipleCurve2 ()
{
    delete[] m_afTime;
    delete[] m_afLength;
    delete[] m_afAccumLength;
}
//----------------------------------------------------------------------------
template <class Real>
int MultipleCurve2<Real>::GetSegments () const
{
    return m_iSegments;
}
//----------------------------------------------------------------------------
template <class Real>
const Real* MultipleCurve2<Real>::GetTimes () const
{
    return m_afTime;
}
//----------------------------------------------------------------------------
template <class Real>
void MultipleCurve2<Real>::GetKeyInfo (Real fTime, int& riKey, Real& rfDt)
    const
{
    if ( fTime <= m_afTime[0] )
    {
        riKey = 0;
        rfDt = (Real)0.0;
    }
    else if ( fTime >= m_afTime[m_iSegments] )
    {
        riKey = m_iSegments-1;
        rfDt = m_afTime[m_iSegments] - m_afTime[m_iSegments-1];
    }
    else
    {
        for (int i = 0; i < m_iSegments; i++)
        {
            if ( fTime < m_afTime[i+1] )
            {
                riKey = i;
                rfDt = fTime - m_afTime[i];
                break;
            }
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real MultipleCurve2<Real>::GetSpeedWithData (Real fTime, void* pvData)
{
    MultipleCurve2* pvThis = *(MultipleCurve2**) pvData;
    int iKey = *(int*)((char*)pvData + sizeof(pvThis));
    return pvThis->GetSpeedKey(iKey,fTime);
}
//----------------------------------------------------------------------------
template <class Real>
void MultipleCurve2<Real>::InitializeLength () const
{
    m_afLength = new Real[m_iSegments];
    m_afAccumLength = new Real[m_iSegments];

    // arc lengths of the segments
    int iKey;
    for (iKey = 0; iKey < m_iSegments; iKey++)
    {
        m_afLength[iKey] = GetLengthKey(iKey,(Real)0.0,
            m_afTime[iKey+1]-m_afTime[iKey]);
    }

    // accumulative arc length
    m_afAccumLength[0] = m_afLength[0];
    for (iKey = 1; iKey < m_iSegments; iKey++)
        m_afAccumLength[iKey] = m_afAccumLength[iKey-1] + m_afLength[iKey];
}
//----------------------------------------------------------------------------
template <class Real>
Real MultipleCurve2<Real>::GetLength (Real fT0, Real fT1) const
{
    assert( m_fTMin <= fT0 && fT0 <= m_fTMax );
    assert( m_fTMin <= fT1 && fT1 <= m_fTMax );
    assert( fT0 <= fT1 );

    if ( !m_afLength )
        InitializeLength();

    int iKey0, iKey1;
    Real fDt0, fDt1;
    GetKeyInfo(fT0,iKey0,fDt0);
    GetKeyInfo(fT1,iKey1,fDt1);

    Real fLength;
    if ( iKey0 < iKey1 )
    {
        // accumulate full-segment lengths
        fLength = (Real)0.0;
        for (int i = iKey0+1; i < iKey1; i++)
            fLength += m_afLength[i];
        
        // add on partial first segment
        fLength += GetLengthKey(iKey0,fDt0,m_afTime[iKey0+1]-m_afTime[iKey0]);
        
        // add on partial last segment
        fLength += GetLengthKey(iKey1,(Real)0.0,fDt1);
    }
    else
    {
        fLength = GetLengthKey(iKey0,fDt0,fDt1);
    }

    return fLength;
}
//----------------------------------------------------------------------------
template <class Real>
Real MultipleCurve2<Real>::GetTime (Real fLength, int iIterations,
    Real fTolerance) const
{
    if ( !m_afLength )
        InitializeLength();

    if ( fLength <= (Real)0.0 )
        return m_fTMin;

    if ( fLength >= m_afAccumLength[m_iSegments-1] )
        return m_fTMax;

    int iKey;
    for (iKey = 0; iKey < m_iSegments; iKey++)
    {
        if ( fLength < m_afAccumLength[iKey] )
            break;
    }
    if ( iKey >= m_iSegments )
        return m_afTime[m_iSegments];

    // try Newton's method first for rapid convergence
    Real fL0, fL1;
    if ( iKey == 0 )
    {
        fL0 = fLength;
        fL1 = m_afAccumLength[0];
    }
    else
    {
        fL0 = fLength - m_afAccumLength[iKey-1];
        fL1 = m_afAccumLength[iKey] - m_afAccumLength[iKey-1];
    }

    // use Newton's method to invert the arc length integral
    Real fDt1 = m_afTime[iKey+1] - m_afTime[iKey];
    Real fDt0 = fDt1*fL0/fL1;
    for (int i = 0; i < iIterations; i++)
    {
        Real fDifference = GetLengthKey(iKey,(Real)0.0,fDt0) - fL0;
        if ( Math<Real>::FAbs(fDifference) <= fTolerance )
            return m_afTime[iKey] + fDt0;

        fDt0 -= fDifference/GetSpeedKey(iKey,fDt0);
    }

    // Newton's method failed.  If this happens, increase iterations or
    // tolerance or integration accuracy.
    return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------
template <class Real>
Real MultipleCurve2<Real>::GetVariation (Real fT0, Real fT1,
    const Vector2<Real>* pkP0, const Vector2<Real>* pkP1) const
{
    assert( m_fTMin <= fT0 && fT0 <= m_fTMax );
    assert( m_fTMin <= fT1 && fT1 <= m_fTMax );
    assert( fT0 <= fT1 );

    // construct line segment, A + (t-t0)*B
    Vector2<Real> kP0, kP1;
    if ( !pkP0 )
    {
        kP0 = GetPosition(fT0);
        pkP0 = &kP0;
    }
    if ( !pkP1 )
    {
        kP1 = GetPosition(fT1);
        pkP1 = &kP1;
    }
    Real fInvDT = ((Real)1.0)/(fT1 - fT0);
    Vector2<Real> kA, kB = fInvDT*(*pkP1 - *pkP0);

    int iKey0, iKey1;
    Real fDt0, fDt1;
    GetKeyInfo(fT0,iKey0,fDt0);
    GetKeyInfo(fT1,iKey1,fDt1);

    Real fVariation;
    if ( iKey0 < iKey1 )
    {
        // accumulate full-segment variations
        fVariation = (Real)0.0;
        for (int i = iKey0+1; i < iKey1; i++)
        {
            kA = *pkP0 + (m_afTime[i] - fT0)*kB;
            fVariation += GetVariationKey(i,(Real)0.0,
                m_afTime[i+1]-m_afTime[i],kA,kB);
        }
        
        // add on partial first segment
        kA = *pkP0 + (m_afTime[iKey0] - fT0)*kB;
        fVariation += GetVariationKey(iKey0,fDt0,
            m_afTime[iKey0+1]-m_afTime[iKey0],kA,kB);
        
        // add on partial last segment
        kA = *pkP0 + (m_afTime[iKey1] - fT0)*kB;
        fVariation += GetVariationKey(iKey1,(Real)0.0,fDt1,kA,kB);
    }
    else
    {
        kA = *pkP0 + (m_afTime[iKey0] - fT0)*kB;
        fVariation = GetVariationKey(iKey0,fDt0,fDt1,kA,kB);
    }

    return fVariation;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM MultipleCurve2<float>;
template class WML_ITEM MultipleCurve2<double>;
}
//----------------------------------------------------------------------------
