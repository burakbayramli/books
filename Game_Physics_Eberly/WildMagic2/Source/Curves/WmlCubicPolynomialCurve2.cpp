// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCubicPolynomialCurve2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
CubicPolynomialCurve2<Real>::CubicPolynomialCurve2
    (Polynomial1<Real>* pkXPoly, Polynomial1<Real>* pkYPoly)
    :
    PolynomialCurve2<Real>(pkXPoly,pkYPoly)
{
    assert( pkXPoly && pkYPoly );
    assert( pkXPoly->GetDegree() == 3 && pkYPoly->GetDegree() == 3 );

    m_iVertexQuantity = 0;
    m_akVertex = NULL;
}
//----------------------------------------------------------------------------
template <class Real>
CubicPolynomialCurve2<Real>::~CubicPolynomialCurve2 ()
{
    delete[] m_akVertex;
}
//----------------------------------------------------------------------------
template <class Real>
int CubicPolynomialCurve2<Real>::GetVertexQuantity () const
{
    return m_iVertexQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real>* CubicPolynomialCurve2<Real>::Vertices ()
{
    return m_akVertex;
}
//----------------------------------------------------------------------------
template <class Real>
void CubicPolynomialCurve2<Real>::Tessellate (int iLevel)
{
    // vertices V = (2^L+1)
    int iTwoPowL = (1 << iLevel);
    m_iVertexQuantity = iTwoPowL + 1;
    delete[] m_akVertex;
    m_akVertex = new Vector2<Real>[m_iVertexQuantity];

    // indices of endpoints, I[t]
    IntervalParameters kIP;
    kIP.m_iI0 = 0;
    kIP.m_iI1 = iTwoPowL;

    // vertices for subdivision
    Vector2<Real>* akX = m_akVertex;
    akX[kIP.m_iI0] = GetPosition(m_fTMin);
    akX[kIP.m_iI1] = GetPosition(m_fTMax);

    // recursive subdivision
    if ( iLevel > 0 )
    {
        kIP.m_akXuu[0] = GetSecondDerivative(m_fTMin);
        kIP.m_akXuu[1] = GetSecondDerivative(m_fTMax);

        Subdivide(--iLevel,0.25,akX,kIP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void CubicPolynomialCurve2<Real>::Subdivide (int iLevel, Real fDSqr,
    Vector2<Real>* akX, IntervalParameters& rkIP)
{
    // subdivision index
    int iIM = (rkIP.m_iI0 + rkIP.m_iI1) >> 1;

    // vertices
    Vector2<Real> kXuuM = ((Real)0.5)*(rkIP.m_akXuu[0] + rkIP.m_akXuu[1]);
    akX[iIM] = ((Real)0.5)*(akX[rkIP.m_iI0] + akX[rkIP.m_iI1] - fDSqr*kXuuM);

    // recurse on two children
    if ( iLevel > 0 )
    {
        iLevel--;
        fDSqr *= (Real)0.25;

        IntervalParameters kSubIP;

        // subinterval [t0,tM]
        kSubIP.m_iI0 = rkIP.m_iI0;
        kSubIP.m_iI1 = iIM;
        kSubIP.m_akXuu[0] = rkIP.m_akXuu[0];
        kSubIP.m_akXuu[1] = kXuuM;
        Subdivide(iLevel,fDSqr,akX,kSubIP);

        // subinterval [tM,t1]
        kSubIP.m_iI0 = iIM;
        kSubIP.m_iI1 = rkIP.m_iI1;
        kSubIP.m_akXuu[0] = kXuuM;
        kSubIP.m_akXuu[1] = rkIP.m_akXuu[1];
        Subdivide(iLevel,fDSqr,akX,kSubIP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM CubicPolynomialCurve2<float>;
template class WML_ITEM CubicPolynomialCurve2<double>;
}
//----------------------------------------------------------------------------
