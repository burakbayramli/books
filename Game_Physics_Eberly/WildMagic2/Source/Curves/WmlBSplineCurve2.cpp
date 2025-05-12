// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBSplineCurve2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
BSplineCurve2<Real>::BSplineCurve2 (int iNumCtrlPoints,
    Vector2<Real>* akCtrlPoint, int iDegree, bool bLoop, bool bOpen)
    :
    SingleCurve2<Real>(0.0,1.0),
    m_bLoop(bLoop)
{
    assert( iNumCtrlPoints >= 2 );
    assert( 1 <= iDegree && iDegree <= iNumCtrlPoints-1 );

    m_iNumCtrlPoints = iNumCtrlPoints;
    m_iReplicate = ( bLoop ? (bOpen ? 1 : iDegree) : 0 );
    CreateControl(akCtrlPoint);
    m_kBasis.Create(m_iNumCtrlPoints+m_iReplicate,iDegree,bOpen);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineCurve2<Real>::BSplineCurve2 (int iNumCtrlPoints,
    Vector2<Real>* akCtrlPoint, int iDegree, bool bLoop, Real* afKnot)
    :
    SingleCurve2<Real>(0.0,1.0),
    m_bLoop(bLoop)
{
    assert( iNumCtrlPoints >= 2 );
    assert( 1 <= iDegree && iDegree <= iNumCtrlPoints-1 );

    m_iNumCtrlPoints = iNumCtrlPoints;
    m_iReplicate = ( bLoop ? 1 : 0 );
    CreateControl(akCtrlPoint);
    m_kBasis.Create(m_iNumCtrlPoints+m_iReplicate,iDegree,afKnot);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineCurve2<Real>::~BSplineCurve2 ()
{
    delete[] m_akCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
void BSplineCurve2<Real>::CreateControl (Vector2<Real>* akCtrlPoint)
{
    int iNewNumCtrlPoints = m_iNumCtrlPoints + m_iReplicate;
    m_akCtrlPoint = new Vector2<Real>[iNewNumCtrlPoints];
    memcpy(m_akCtrlPoint,akCtrlPoint,m_iNumCtrlPoints*sizeof(Vector2<Real>));
    for (int i = 0; i < m_iReplicate; i++)
        m_akCtrlPoint[m_iNumCtrlPoints+i] = akCtrlPoint[i];
}
//----------------------------------------------------------------------------
template <class Real>
int BSplineCurve2<Real>::GetNumCtrlPoints () const
{
    return m_iNumCtrlPoints;
}
//----------------------------------------------------------------------------
template <class Real>
int BSplineCurve2<Real>::GetDegree () const
{
    return m_kBasis.GetDegree();
}
//----------------------------------------------------------------------------
template <class Real>
bool BSplineCurve2<Real>::IsOpen () const
{
    return m_kBasis.IsOpen();
}
//----------------------------------------------------------------------------
template <class Real>
bool BSplineCurve2<Real>::IsUniform () const
{
    return m_kBasis.IsUniform();
}
//----------------------------------------------------------------------------
template <class Real>
bool BSplineCurve2<Real>::IsLoop () const
{
    return m_bLoop;
}
//----------------------------------------------------------------------------
template <class Real>
void BSplineCurve2<Real>::SetControlPoint (int i, const Vector2<Real>& rkCtrl)
{
    if ( 0 <= i && i < m_iNumCtrlPoints )
    {
        // set the control point
        m_akCtrlPoint[i] = rkCtrl;

        // set the replicated control point
        if ( i < m_iReplicate )
            m_akCtrlPoint[m_iNumCtrlPoints+i] = rkCtrl;
    }
}
//----------------------------------------------------------------------------
template <class Real>
const Vector2<Real>& BSplineCurve2<Real>::GetControlPoint (int i) const
{
    if ( 0 <= i && i < m_iNumCtrlPoints )
        return m_akCtrlPoint[i];

    return ms_kInvalidCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
Real& BSplineCurve2<Real>::Knot (int i)
{
    return m_kBasis.Knot(i);
}
//----------------------------------------------------------------------------
template <class Real>
void BSplineCurve2<Real>::Get (Real fTime, Vector2<Real>* pkPos,
    Vector2<Real>* pkDer1, Vector2<Real>* pkDer2, Vector2<Real>* pkDer3) const
{
    int i, iMin, iMax;
    if ( pkDer3 )
        m_kBasis.Compute(fTime,3,iMin,iMax);
    else if ( pkDer2 )
        m_kBasis.Compute(fTime,2,iMin,iMax);
    else if ( pkDer1 )
        m_kBasis.Compute(fTime,1,iMin,iMax);
    else
        m_kBasis.Compute(fTime,0,iMin,iMax);

    if ( pkPos )
    {
        *pkPos = Vector2<Real>::ZERO;
        for (i = iMin; i <= iMax; i++)
            *pkPos += m_kBasis.GetD0(i)*m_akCtrlPoint[i];
    }

    if ( pkDer1 )
    {
        *pkDer1 = Vector2<Real>::ZERO;
        for (i = iMin; i <= iMax; i++)
            *pkDer1 += m_kBasis.GetD1(i)*m_akCtrlPoint[i];
    }

    if ( pkDer2 )
    {
        *pkDer2 = Vector2<Real>::ZERO;
        for (i = iMin; i <= iMax; i++)
            *pkDer2 += m_kBasis.GetD2(i)*m_akCtrlPoint[i];
    }

    if ( pkDer3 )
    {
        *pkDer3 = Vector2<Real>::ZERO;
        for (i = iMin; i <= iMax; i++)
            *pkDer3 += m_kBasis.GetD3(i)*m_akCtrlPoint[i];
    }
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BSplineCurve2<Real>::GetPosition (Real fTime) const
{
    Vector2<Real> kPos;
    Get(fTime,&kPos,NULL,NULL,NULL);
    return kPos;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BSplineCurve2<Real>::GetFirstDerivative (Real fTime) const
{
    Vector2<Real> kDer1;
    Get(fTime,NULL,&kDer1,NULL,NULL);
    return kDer1;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BSplineCurve2<Real>::GetSecondDerivative (Real fTime) const
{
    Vector2<Real> kDer2;
    Get(fTime,NULL,NULL,&kDer2,NULL);
    return kDer2;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> BSplineCurve2<Real>::GetThirdDerivative (Real fTime) const
{
    Vector2<Real> kDer3;
    Get(fTime,NULL,NULL,NULL,&kDer3);
    return kDer3;
}
//----------------------------------------------------------------------------
template <class Real>
Real BSplineCurve2<Real>::GetVariation (Real, Real, const Vector2<Real>*,
    const Vector2<Real>*) const
{
    // TO DO.
    return (Real)0.0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM BSplineCurve2<float>;
Vector2f BSplineCurve2f::ms_kInvalidCtrlPoint = Vector2f::ZERO;

template class WML_ITEM BSplineCurve2<double>;
Vector2d BSplineCurve2d::ms_kInvalidCtrlPoint = Vector2d::ZERO;
}
//----------------------------------------------------------------------------
