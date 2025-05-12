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
bool PolynomialRoots<Real>::FindA (Real fC0, Real fC1)
{
    if ( Math<Real>::FAbs(fC1) >= m_fEpsilon )
    {
        m_afRoot[0] = -fC0/fC1;
        m_iCount = 1;
        return true;
    }

    m_iCount = 0;
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetBound (Real fC0, Real fC1)
{
    if ( Math<Real>::FAbs(fC1) <= m_fEpsilon )
    {
        // polynomial is constant, return invalid bound
        return -(Real)1.0;
    }

    Real fInvC1 = ((Real)1.0)/fC1;
    Real fMax = Math<Real>::FAbs(fC0)*fInvC1;
    return (Real)1.0 + fMax;
}
//----------------------------------------------------------------------------
