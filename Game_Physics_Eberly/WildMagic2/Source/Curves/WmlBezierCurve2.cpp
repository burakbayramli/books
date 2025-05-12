// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBezierCurve2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
BezierCurve2<Real>::BezierCurve2 (int iDegree, Vector2<Real>* akCtrlPoint)
    :
    SingleCurve2<Real>(0.0,1.0)
{
    assert( iDegree >= 2 );

    int i, j;

    m_iDegree = iDegree;
    m_iNumCtrlPoints = m_iDegree + 1;
    m_akCtrlPoint = akCtrlPoint;

    // compute first-order differences
    m_akDer1CtrlPoint = new Vector2<Real>[m_iNumCtrlPoints-1];
    for (i = 0; i < m_iNumCtrlPoints-1; i++)
        m_akDer1CtrlPoint[i] = m_akCtrlPoint[i+1] - m_akCtrlPoint[i];

    // compute second-order differences
    m_akDer2CtrlPoint = new Vector2<Real>[m_iNumCtrlPoints-2];
    for (i = 0; i < m_iNumCtrlPoints-2; i++)
        m_akDer2CtrlPoint[i] = m_akDer1CtrlPoint[i+1] - m_akDer1CtrlPoint[i];

    // compute third-order differences
    if ( iDegree >= 3 )
    {
        m_akDer3CtrlPoint = new Vector2<Real>[m_iNumCtrlPoints-3];
        for (i = 0; i < m_iNumCtrlPoints-3; i++)
        {
            m_akDer3CtrlPoint[i] = m_akDer2CtrlPoint[i+1] -
                m_akDer2CtrlPoint[i];
        }
    }
    else
    {
        m_akDer3CtrlPoint = NULL;
    }

    // Compute combinatorial values Choose(N,K), store in m_aafChoose[N][K].
    // The values m_aafChoose[r][c] are invalid for r < c (use only the
    // entries for r >= c).
    m_aafChoose = new Real*[m_iNumCtrlPoints];
    m_aafChoose[0] = new Real[m_iNumCtrlPoints*m_iNumCtrlPoints];
    for (i = 1; i < m_iNumCtrlPoints; i++)
        m_aafChoose[i] = &m_aafChoose[0][i*m_iNumCtrlPoints];

    m_aafChoose[0][0] = (Real)1.0;
    m_aafChoose[1][0] = (Real)1.0;
    m_aafChoose[1][1] = (Real)1.0;
    for (i = 2; i <= m_iDegree; i++)
    {
        m_aafChoose[i][0] = (Real)1.0;
        m_aafChoose[i][i] = (Real)1.0;
        for (j = 1; j < i; j++)
            m_aafChoose[i][j] = m_aafChoose[i-1][j-1] + m_aafChoose[i-1][j];
    }

    // variation support
    m_iTwoDegree = 2*m_iDegree;
    m_iTwoDegreePlusOne = m_iTwoDegree + 1;
    m_afSigma = new Real[m_iTwoDegreePlusOne];
    m_afRecip = new Real[m_iTwoDegreePlusOne];
    for (i = 0; i <= m_iTwoDegree; i++)
    {
        m_afSigma[i] = (Real)0.0;
        int iHalf = ( i % 2 ? (i+1)/2 : i/2 );
        int iStart = ( i <= m_iDegree ? 0 : i - m_iDegree );
        Real fDot, fProd;
        for (j = iStart; j < iHalf; j++)
        {
            fDot = m_akCtrlPoint[j].Dot(m_akCtrlPoint[i-j]);
            fProd = m_aafChoose[m_iDegree][j]*m_aafChoose[m_iDegree][i-j];
            m_afSigma[i] += fDot*fProd;
        }
        m_afSigma[i] *= (Real)2.0;

        if ( (i % 2) == 0 )
        {
            fDot = m_akCtrlPoint[iHalf].Dot(m_akCtrlPoint[iHalf]);
            fProd = m_aafChoose[m_iDegree][iHalf];
            fProd *= fProd;
            m_afSigma[i] += fDot*fProd;
        }

        m_afRecip[i] = ((Real)1.0)/Real(m_iTwoDegreePlusOne-i);
    }

    int iTDp2 = m_iTwoDegreePlusOne+1;
    m_afPowT0 = new Real[iTDp2];
    m_afPowT0[0] = (Real)1.0;
    m_afPowT1 = new Real[iTDp2];
    m_afPowT1[0] = (Real)1.0;

    m_afPowOmT0 = new Real[iTDp2];
    m_afPowOmT0[0] = (Real)1.0;
    m_afPowOmT1 = new Real[iTDp2];
    m_afPowOmT1[0] = (Real)1.0;
}
//----------------------------------------------------------------------------
template <class Real>
BezierCurve2<Real>::~BezierCurve2 ()
{
    delete[] m_afPowOmT1;
    delete[] m_afPowOmT0;
    delete[] m_afPowT1;
    delete[] m_afPowT0;
    delete[] m_afSigma;
    delete[] m_afRecip;

    delete[] m_aafChoose[0];
    delete[] m_aafChoose;
    delete[] m_akDer3CtrlPoint;
    delete[] m_akDer2CtrlPoint;
    delete[] m_akDer1CtrlPoint;
    delete[] m_akCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
int BezierCurve2<Real>::GetDegree () const
{
    return m_iDegree;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>* BezierCurve2<Real>::GetControlPoints () const
{
    return m_akCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BezierCurve2<Real>::GetPosition (Real fTime) const
{
    Real fOmTime = (Real)1.0 - fTime;
    Real fPowTime = fTime;
    Vector2<Real> kResult = fOmTime*m_akCtrlPoint[0];

    for (int i = 1; i < m_iDegree; i++)
    {
        Real fCoeff = m_aafChoose[m_iDegree][i]*fPowTime;
        kResult = (kResult+fCoeff*m_akCtrlPoint[i])*fOmTime;
        fPowTime *= fTime;
    }

    kResult += fPowTime*m_akCtrlPoint[m_iDegree];

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BezierCurve2<Real>::GetFirstDerivative (Real fTime) const
{
    Real fOmTime = (Real)1.0 - fTime;
    Real fPowTime = fTime;
    Vector2<Real> kResult = fOmTime*m_akDer1CtrlPoint[0];

    int iDegreeM1 = m_iDegree - 1;
    for (int i = 1; i < iDegreeM1; i++)
    {
        Real fCoeff = m_aafChoose[iDegreeM1][i]*fPowTime;
        kResult = (kResult+fCoeff*m_akDer1CtrlPoint[i])*fOmTime;
        fPowTime *= fTime;
    }

    kResult += fPowTime*m_akDer1CtrlPoint[iDegreeM1];
    kResult *= Real(m_iDegree);

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BezierCurve2<Real>::GetSecondDerivative (Real fTime) const
{
    Real fOmTime = (Real)1.0 - fTime;
    Real fPowTime = fTime;
    Vector2<Real> kResult = fOmTime*m_akDer2CtrlPoint[0];

    int iDegreeM2 = m_iDegree - 2;
    for (int i = 1; i < iDegreeM2; i++)
    {
        Real fCoeff = m_aafChoose[iDegreeM2][i]*fPowTime;
        kResult = (kResult+fCoeff*m_akDer2CtrlPoint[i])*fOmTime;
        fPowTime *= fTime;
    }

    kResult += fPowTime*m_akDer2CtrlPoint[iDegreeM2];
    kResult *= Real(m_iDegree*(m_iDegree-1));

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BezierCurve2<Real>::GetThirdDerivative (Real fTime) const
{
    if ( m_iDegree < 3 )
        return Vector2<Real>::ZERO;

    Real fOmTime = (Real)1.0 - fTime;
    Real fPowTime = fTime;
    Vector2<Real> kResult = fOmTime*m_akDer3CtrlPoint[0];

    int iDegreeM3 = m_iDegree - 3;
    for (int i = 1; i < iDegreeM3; i++)
    {
        Real fCoeff = m_aafChoose[iDegreeM3][i]*fPowTime;
        kResult = (kResult+fCoeff*m_akDer3CtrlPoint[i])*fOmTime;
        fPowTime *= fTime;
    }

    kResult += fPowTime*m_akDer3CtrlPoint[iDegreeM3];
    kResult *= Real(m_iDegree*(m_iDegree-1)*(m_iDegree-2));

    return kResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real BezierCurve2<Real>::GetVariation (Real fT0, Real fT1,
    const Vector2<Real>* pkP0, const Vector2<Real>* pkP1) const
{
    int i, j, k;

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

    // compute powers of t0, t1, 1-t0, 1-t1
    Real fOmT0 = (Real)1.0 - fT0;
    Real fOmT1 = (Real)1.0 - fT1;
    for (i = 1, j = 0; i <= m_iTwoDegreePlusOne; i++, j++)
    {
        m_afPowT0[i] = fT0*m_afPowT0[j];
        m_afPowT1[i] = fT1*m_afPowT1[j];
        m_afPowOmT0[i] = fOmT0*m_afPowOmT0[j];
        m_afPowOmT1[i] = fOmT1*m_afPowOmT1[j];
    }

    // line segment is L(t) = P0 + ((t-t0)/(t1-t0))*(P1-P0)

    // var1 = integral(Dot(L,L))
    static const Real s_fOneThird = (Real)(1.0/3.0);
    Real fDT = fT1 - fT0;
    Real fP0P0 = pkP0->Dot(*pkP0);
    Real fP0P1 = pkP0->Dot(*pkP1);
    Real fP1P1 = pkP1->Dot(*pkP1);
    Real fVar1 = s_fOneThird*fDT*(fP0P0 + fP0P1 + fP1P1);

    // var2 = integral(Dot(X,P0))
    // var3 = integral(Dot(X,P1-P0)*(t-t0)/(t1-t0))
    Real fVar2 = (Real)0.0;
    Real fVar3 = (Real)0.0;
    Vector2<Real> kDir = *pkP1 - *pkP0;
    Real fIint = (Real)0.0;
    int iDp2 = m_iDegree+2, iDm1 = m_iDegree-1;
    Real fJint = (m_afPowOmT0[iDp2] - m_afPowOmT1[iDp2])*m_afRecip[iDm1];
    Real fProd0, fProd1, fDot;
    for (i = 0, j = m_iDegree, k = m_iDegree+1; i <= m_iDegree; i++, j++, k--)
    {
        // compute I[i]
        fProd0 = m_afPowT0[i]*m_afPowOmT0[k];
        fProd1 = m_afPowT1[i]*m_afPowOmT1[k];
        fIint = (fProd0 - fProd1 + i*fIint)*m_afRecip[j];

        // compute J[i]
        fProd0 = m_afPowT0[i+1]*m_afPowOmT0[k];
        fProd1 = m_afPowT1[i+1]*m_afPowOmT1[k];
        fJint = (fProd0 - fProd1 + (i+1)*fJint)*m_afRecip[j];

        // update partial variations
        fDot = pkP0->Dot(m_akCtrlPoint[i]);
        fProd0 = m_aafChoose[m_iDegree][i]*fDot;
        fVar2 += fProd0*fIint;
        fDot = kDir.Dot(m_akCtrlPoint[i]);
        fProd0 = m_aafChoose[m_iDegree][i]*fDot;
        fVar3 += fProd0*(fJint - fT0*fIint);
    }
    fVar3 /= fDT;

    // var4 = integral(Dot(X,X))
    Real fVar4 = (Real)0.0;
    Real fKint = (Real)0.0;
    for (i = 0, j = m_iTwoDegreePlusOne; i <= m_iTwoDegree; i++, j--)
    {
        // compute K[i]
        fProd0 = m_afPowT0[i]*m_afPowOmT0[j];
        fProd1 = m_afPowT1[i]*m_afPowOmT1[j];
        fKint = (fProd0 - fProd1 + i*fKint)*m_afRecip[i];

        // update partial variation
        fVar4 += m_afSigma[i]*fKint;
    }

    Real fVar = fVar1 - ((Real)2.0)*(fVar2 + fVar3) + fVar4;
    return fVar;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM BezierCurve2<float>;
template class WML_ITEM BezierCurve2<double>;
}
//----------------------------------------------------------------------------
