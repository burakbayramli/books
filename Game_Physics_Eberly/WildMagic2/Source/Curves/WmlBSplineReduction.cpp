// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBSplineReduction.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <int N, class Real>
BSplineReduction<N,Real>::BSplineReduction (int iQuantity,
    const Vector<N,Real>* akCtrl, int iDegree, Real fFraction,
    int& riQuantityOut, Vector<N,Real>*& rakCtrlOut)
{
    // Check for valid input.  If invalid, return a "null" curve.
    assert( iQuantity >= 2 );
    assert( akCtrl );
    assert( 1 <= iDegree && iDegree < iQuantity );
    if ( iQuantity < 2 || !akCtrl || iDegree < 1 || iDegree >= iQuantity )
    {
        riQuantityOut = 0;
        rakCtrlOut = NULL;
        return;
    }

    // clamp the number of control points to [degree+1,quantity-1]
    riQuantityOut = (int)(fFraction*iQuantity);
    if ( riQuantityOut > iQuantity )
    {
        riQuantityOut = iQuantity;
        rakCtrlOut = new Vector<N,Real>[riQuantityOut];
        memcpy(rakCtrlOut,akCtrl,iQuantity*sizeof(Vector<N,Real>));
        return;
    }
    if ( riQuantityOut < iDegree+1 )
        riQuantityOut = iDegree+1;

    // allocate output control points and set all to the zero vector
    rakCtrlOut = new Vector<N,Real>[riQuantityOut];

    // Set up basis function parameters.  Function 0 corresponds to the
    // output curve.  Function 1 corresponds to the input curve.
    m_iDegree = iDegree;
    m_aiQuantity[0] = riQuantityOut;
    m_aiQuantity[1] = iQuantity;

    for (int j = 0; j <= 1; j++)
    {
        m_aiNumKnots[j] = m_aiQuantity[j] + m_iDegree + 1;
        m_aafKnot[j] = new Real[m_aiNumKnots[j]];

        int i;
        for (i = 0; i <= m_iDegree; i++)
            m_aafKnot[j][i] = (Real)0.0;

        Real fFactor = ((Real)1.0)/(Real)(m_aiQuantity[j]-m_iDegree);
        for (/**/; i < m_aiQuantity[j]; i++)
            m_aafKnot[j][i] = (i-m_iDegree)*fFactor;

        for (/**/; i < m_aiNumKnots[j]; i++)
            m_aafKnot[j][i] = (Real)1.0;
    }

    // construct matrix A (depends only on the output basis function)
    Real fValue, fTMin, fTMax;
    int i0, i1;

    m_aiBasis[0] = 0;
    m_aiBasis[1] = 0;

    BandedMatrix<Real> kA(m_aiQuantity[0],m_iDegree,m_iDegree);
    for (i0 = 0; i0 < m_aiQuantity[0]; i0++)
    {
        m_aiIndex[0] = i0;
        fTMax = MaxSupport(0,i0);

        for (i1 = i0; i1 <= i0 + m_iDegree && i1 < m_aiQuantity[0]; i1++)
        {
            m_aiIndex[1] = i1;
            fTMin = MinSupport(0,i1);

            fValue = Integrate1<Real>::RombergIntegral(fTMin,fTMax,
                Integrand,this);
            kA(i0,i1) = fValue;
            kA(i1,i0) = fValue;
        }
    }

    // construct A^{-1}
    GMatrix<Real> kInvA(m_aiQuantity[0],m_aiQuantity[0]);
    bool bResult = LinearSystem<Real>::Invert(kA,kInvA);
    assert( bResult );

    // construct B (depends on both input and output basis functions)
    m_aiBasis[1] = 1;
    GMatrix<Real> kB(m_aiQuantity[0],m_aiQuantity[1]);
    for (i0 = 0; i0 < m_aiQuantity[0]; i0++)
    {
        m_aiIndex[0] = i0;
        Real fTMin0 = MinSupport(0,i0);
        Real fTMax0 = MaxSupport(0,i0);

        for (i1 = 0; i1 < m_aiQuantity[1]; i1++)
        {
            m_aiIndex[1] = i1;
            Real fTMin1 = MinSupport(1,i1);
            Real fTMax1 = MaxSupport(1,i1);

            int iIQuantity = FindIntersection<Real>(fTMin0,fTMax0,fTMin1,
                fTMax1,fTMin,fTMax);

            if ( iIQuantity == 2 )
            {
                fValue = Integrate1<Real>::RombergIntegral(fTMin,fTMax,
                    Integrand,this);
                kB[i0][i1] = fValue;
            }
            else
            {
                kB[i0][i1] = (Real)0.0;
            }
        }
    }

    // construct A^{-1}*B
    GMatrix<Real> kProd = kInvA*kB;

    // construct the control points for the least-squares curve
    memset(rakCtrlOut,0,riQuantityOut*sizeof(Vector<N,Real>));
    for (i0 = 0; i0 < m_aiQuantity[0]; i0++)
    {
        for (i1 = 0; i1 < m_aiQuantity[1]; i1++)
            rakCtrlOut[i0] += kProd[i0][i1]*akCtrl[i1];
    }
}
//----------------------------------------------------------------------------
template <int N, class Real>
BSplineReduction<N,Real>::~BSplineReduction ()
{
    delete[] m_aafKnot[0];
    delete[] m_aafKnot[1];
}
//----------------------------------------------------------------------------
template <int N, class Real>
Real BSplineReduction<N,Real>::MinSupport (int iBasis, int i) const
{
    return m_aafKnot[iBasis][i];
}
//----------------------------------------------------------------------------
template <int N, class Real>
Real BSplineReduction<N,Real>::MaxSupport (int iBasis, int i) const
{
    return m_aafKnot[iBasis][i+1+m_iDegree];
}
//----------------------------------------------------------------------------
template <int N, class Real>
Real BSplineReduction<N,Real>::F (int iBasis, int i, int j, Real fTime)
{
    if ( j > 0 )
    {
        Real fResult = (Real)0.0;

        Real fDenom = m_aafKnot[iBasis][i+j] - m_aafKnot[iBasis][i];
        if ( fDenom > (Real)0.0 )
        {
            fResult += (fTime - m_aafKnot[iBasis][i]) *
                F(iBasis,i,j-1,fTime)/fDenom;
        }

        fDenom = m_aafKnot[iBasis][i+j+1] - m_aafKnot[iBasis][i+1];
        if ( fDenom > (Real)0.0 )
        {
            fResult += (m_aafKnot[iBasis][i+j+1] - fTime) *
                F(iBasis,i+1,j-1,fTime)/fDenom;
        }

        return fResult;
    }

    if ( m_aafKnot[iBasis][i] <= fTime && fTime < m_aafKnot[iBasis][i+1] )
        return (Real)1.0;
    else
        return (Real)0.0;
}
//----------------------------------------------------------------------------
template <int N, class Real>
Real BSplineReduction<N,Real>::Integrand (Real fTime, void* pvThis)
{
    BSplineReduction<N,Real>& rkSelf = *(BSplineReduction<N,Real>*)pvThis;

    Real fValue0 = rkSelf.F(rkSelf.m_aiBasis[0],rkSelf.m_aiIndex[0],
        rkSelf.m_iDegree,fTime);

    Real fValue1 = rkSelf.F(rkSelf.m_aiBasis[1],rkSelf.m_aiIndex[1],
        rkSelf.m_iDegree,fTime);

    return fValue0*fValue1;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM BSplineReduction<2,float>;
template class WML_ITEM BSplineReduction<2,double>;
template class WML_ITEM BSplineReduction<3,float>;
template class WML_ITEM BSplineReduction<3,double>;
}
//----------------------------------------------------------------------------
