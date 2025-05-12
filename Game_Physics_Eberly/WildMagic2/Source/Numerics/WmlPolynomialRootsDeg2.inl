// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindA (Real fC0, Real fC1, Real fC2)
{
    if ( Math<Real>::FAbs(fC2) <= m_fEpsilon )
    {
        // polynomial is linear
        return FindA(fC0,fC1);
    }

    Real fDiscr = fC1*fC1 - 4.0f*fC0*fC2;
    if ( Math<Real>::FAbs(fDiscr) <= m_fEpsilon )
        fDiscr = (Real)0.0;

    if ( fDiscr < (Real)0.0 )
    {
        m_iCount = 0;
        return false;
    }

    Real fTmp = ((Real)0.5)/fC2;

    if ( fDiscr > (Real)0.0 )
    {
        fDiscr = Math<Real>::Sqrt(fDiscr);
        m_afRoot[0] = fTmp*(-fC1 - fDiscr);
        m_afRoot[1] = fTmp*(-fC1 + fDiscr);
        m_iCount = 2;
    }
    else
    {
        m_afRoot[0] = -fTmp*fC1;
        m_iCount = 1;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetBound (Real fC0, Real fC1, Real fC2)
{
    if ( Math<Real>::FAbs(fC2) <= m_fEpsilon )
    {
        // polynomial is linear
        return ( FindA(fC0,fC1) ? m_afRoot[0] : Math<Real>::MAX_REAL );
    }

    Real fInvC2 = ((Real)1.0)/fC2;
    Real fTmp0 = Math<Real>::FAbs(fC0)*fInvC2;
    Real fTmp1 = Math<Real>::FAbs(fC1)*fInvC2;
    Real fMax = ( fTmp0 >= fTmp1 ? fTmp0 : fTmp1 );
    return (Real)1.0 + fMax;
}
//----------------------------------------------------------------------------
