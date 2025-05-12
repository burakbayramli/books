// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlSingleCurve3.h"
#include "WmlIntegrate1.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
SingleCurve3<Real>::SingleCurve3 (Real fTMin, Real fTMax)
    :
    Curve3<Real>(fTMin,fTMax)
{
}
//----------------------------------------------------------------------------
template <class Real>
Real SingleCurve3<Real>::GetSpeedWithData (Real fTime, void* pvData)
{
    return ((Curve3<Real>*)pvData)->GetSpeed(fTime);
}
//----------------------------------------------------------------------------
template <class Real>
Real SingleCurve3<Real>::GetLength (Real fT0, Real fT1) const
{
    assert( m_fTMin <= fT0 && fT0 <= m_fTMax );
    assert( m_fTMin <= fT1 && fT1 <= m_fTMax );
    assert( fT0 <= fT1 );

    return Integrate1<Real>::RombergIntegral(fT0,fT1,GetSpeedWithData,
        (void*)this);
}
//----------------------------------------------------------------------------
template <class Real>
Real SingleCurve3<Real>::GetTime (Real fLength, int iIterations,
    Real fTolerance) const
{
    if ( fLength <= (Real)0.0 )
        return m_fTMin;

    if ( fLength >= GetTotalLength() )
        return m_fTMax;

    // initial guess for Newton's method
    Real fRatio = fLength/GetTotalLength();
    Real fOmRatio = (Real)1.0 - fRatio;
    Real fTime = fOmRatio*m_fTMin + fRatio*m_fTMax;

    for (int i = 0; i < iIterations; i++)
    {
        Real fDifference = GetLength(m_fTMin,fTime) - fLength;
        if ( Math<Real>::FAbs(fDifference) < fTolerance )
            return fTime;

        fTime -= fDifference/GetSpeed(fTime);
    }

    // Newton's method failed.  If this happens, increase iterations or
    // tolerance or integration accuracy.
    return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM SingleCurve3<float>;
template class WML_ITEM SingleCurve3<double>;
}
//----------------------------------------------------------------------------
