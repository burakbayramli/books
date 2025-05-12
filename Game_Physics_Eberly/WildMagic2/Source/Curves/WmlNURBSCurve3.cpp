// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlNURBSCurve3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
NURBSCurve3<Real>::NURBSCurve3 (int iNumCtrlPoints,
    Vector3<Real>* akCtrlPoint, Real* afCtrlWeight, int iDegree, bool bLoop,
    bool bOpen)
    :
    SingleCurve3<Real>(0.0,1.0),
    m_bLoop(bLoop)
{
    assert( iNumCtrlPoints >= 2 );
    assert( 1 <= iDegree && iDegree <= iNumCtrlPoints-1 );

    m_iNumCtrlPoints = iNumCtrlPoints;
    m_iReplicate = ( bLoop ? (bOpen ? 1 : iDegree) : 0 );
    CreateControl(akCtrlPoint,afCtrlWeight);
    m_kBasis.Create(m_iNumCtrlPoints+m_iReplicate,iDegree,bOpen);
}
//----------------------------------------------------------------------------
template <class Real>
NURBSCurve3<Real>::NURBSCurve3 (int iNumCtrlPoints,
    Vector3<Real>* akCtrlPoint, Real* afCtrlWeight, int iDegree, bool bLoop,
    Real* afKnot)
    :
    SingleCurve3<Real>(0.0,1.0),
    m_bLoop(bLoop)
{
    assert( iNumCtrlPoints >= 2 );
    assert( 1 <= iDegree && iDegree <= iNumCtrlPoints-1 );

    m_iNumCtrlPoints = iNumCtrlPoints;
    m_iReplicate = ( bLoop ? 1 : 0 );
    CreateControl(akCtrlPoint,afCtrlWeight);
    m_kBasis.Create(m_iNumCtrlPoints+m_iReplicate,iDegree,afKnot);
}
//----------------------------------------------------------------------------
template <class Real>
NURBSCurve3<Real>::~NURBSCurve3 ()
{
    delete[] m_akCtrlPoint;
    delete[] m_afCtrlWeight;
}
//----------------------------------------------------------------------------
template <class Real>
void NURBSCurve3<Real>::CreateControl (Vector3<Real>* akCtrlPoint,
    Real* afCtrlWeight)
{
    int iNewNumCtrlPoints = m_iNumCtrlPoints + m_iReplicate;
    m_akCtrlPoint = new Vector3<Real>[iNewNumCtrlPoints];
    m_afCtrlWeight = new Real[iNewNumCtrlPoints];
    memcpy(m_akCtrlPoint,akCtrlPoint,m_iNumCtrlPoints*sizeof(Vector3<Real>));
    memcpy(m_afCtrlWeight,afCtrlWeight,m_iNumCtrlPoints*sizeof(Real));
    for (int i = 0; i < m_iReplicate; i++)
    {
        m_akCtrlPoint[m_iNumCtrlPoints+i] = akCtrlPoint[i];
        m_afCtrlWeight[m_iNumCtrlPoints+i] = afCtrlWeight[i];
    }
}
//----------------------------------------------------------------------------
template <class Real>
int NURBSCurve3<Real>::GetNumCtrlPoints () const
{
    return m_iNumCtrlPoints;
}
//----------------------------------------------------------------------------
template <class Real>
int NURBSCurve3<Real>::GetDegree () const
{
    return m_kBasis.GetDegree();
}
//----------------------------------------------------------------------------
template <class Real>
bool NURBSCurve3<Real>::IsOpen () const
{
    return m_kBasis.IsOpen();
}
//----------------------------------------------------------------------------
template <class Real>
bool NURBSCurve3<Real>::IsUniform () const
{
    return m_kBasis.IsUniform();
}
//----------------------------------------------------------------------------
template <class Real>
bool NURBSCurve3<Real>::IsLoop () const
{
    return m_bLoop;
}
//----------------------------------------------------------------------------
template <class Real>
void NURBSCurve3<Real>::SetControlPoint (int i, const Vector3<Real>& rkCtrl)
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
const Vector3<Real>& NURBSCurve3<Real>::GetControlPoint (int i) const
{
    if ( 0 <= i && i < m_iNumCtrlPoints )
        return m_akCtrlPoint[i];

    return ms_kInvalidCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
void NURBSCurve3<Real>::SetControlWeight (int i, Real fWeight)
{
    if ( 0 <= i && i < m_iNumCtrlPoints )
    {
        // set the control weight
        m_afCtrlWeight[i] = fWeight;

        // set the replicated control weight
        if ( i < m_iReplicate )
            m_afCtrlWeight[m_iNumCtrlPoints+i] = fWeight;
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real NURBSCurve3<Real>::GetControlWeight (int i) const
{
    if ( 0 <= i && i < m_iNumCtrlPoints )
        return m_afCtrlWeight[i];

    return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------
template <class Real>
Real& NURBSCurve3<Real>::Knot (int i)
{
    return m_kBasis.Knot(i);
}
//----------------------------------------------------------------------------
template <class Real>
void NURBSCurve3<Real>::Get (Real fTime, Vector3<Real>* pkPos,
    Vector3<Real>* pkDer1, Vector3<Real>* pkDer2, Vector3<Real>* pkDer3) const
{
    int i, iMin, iMax;
    if ( pkDer3 )
    {
        m_kBasis.Compute(fTime,0,iMin,iMax);
        m_kBasis.Compute(fTime,1,iMin,iMax);
        m_kBasis.Compute(fTime,2,iMin,iMax);
        m_kBasis.Compute(fTime,3,iMin,iMax);
    }
    else if ( pkDer2 )
    {
        m_kBasis.Compute(fTime,0,iMin,iMax);
        m_kBasis.Compute(fTime,1,iMin,iMax);
        m_kBasis.Compute(fTime,2,iMin,iMax);
    }
    else if ( pkDer1 )
    {
        m_kBasis.Compute(fTime,0,iMin,iMax);
        m_kBasis.Compute(fTime,1,iMin,iMax);
    }
    else  // pkPos
    {
        m_kBasis.Compute(fTime,0,iMin,iMax);
    }

    Real fTmp;

    // compute position
    Vector3<Real> kX = Vector3<Real>::ZERO;
    Real fW = (Real)0.0;
    for (i = iMin; i <= iMax; i++)
    {
        fTmp = m_kBasis.GetD0(i)*m_afCtrlWeight[i];
        kX += fTmp*m_akCtrlPoint[i];
        fW += fTmp;
    }
    Real fInvW = ((Real)1.0)/fW;
    Vector3<Real> kP = fInvW*kX;
    if ( pkPos )
        *pkPos = kP;

    if ( !pkDer1 && !pkDer2 && !pkDer3 )
        return;

    // compute first derivative
    Vector3<Real> kXDer1 = Vector3<Real>::ZERO;
    Real fWDer1 = (Real)0.0;
    for (i = iMin; i <= iMax; i++)
    {
        fTmp = m_kBasis.GetD1(i)*m_afCtrlWeight[i];
        kXDer1 += fTmp*m_akCtrlPoint[i];
        fWDer1 += fTmp;
    }
    Vector3<Real> kPDer1 = fInvW*(kXDer1 - fWDer1*kP);
    if ( pkDer1 )
        *pkDer1 = kPDer1;

    if ( !pkDer2 && !pkDer3 )
        return;

    // compute second derivative
    Vector3<Real> kXDer2 = Vector3<Real>::ZERO;
    Real fWDer2 = (Real)0.0;
    for (i = iMin; i <= iMax; i++)
    {
        fTmp = m_kBasis.GetD2(i)*m_afCtrlWeight[i];
        kXDer2 += fTmp*m_akCtrlPoint[i];
        fWDer2 += fTmp;
    }
    Vector3<Real> kPDer2 = fInvW*(kXDer2-((Real)2.0)*fWDer1*kPDer1-fWDer2*kP);
    if ( pkDer2 )
        *pkDer2 = kPDer2;

    if ( !pkDer3 )
        return;

    // compute third derivative
    Vector3<Real> kXDer3 = Vector3<Real>::ZERO;
    Real fWDer3 = (Real)0.0;
    for (i = iMin; i <= iMax; i++)
    {
        fTmp = m_kBasis.GetD3(i)*m_afCtrlWeight[i];
        kXDer3 += fTmp*m_akCtrlPoint[i];
        fWDer3 += fTmp;
    }
    if ( pkDer3 )
    {
        *pkDer3 = fInvW*(kXDer3 - ((Real)3.0)*fWDer1*kPDer2 -
            ((Real)3.0)*fWDer2*kPDer1 - fWDer3*kP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NURBSCurve3<Real>::GetPosition (Real fTime) const
{
    Vector3<Real> kPos;
    Get(fTime,&kPos,NULL,NULL,NULL);
    return kPos;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NURBSCurve3<Real>::GetFirstDerivative (Real fTime) const
{
    Vector3<Real> kDer1;
    Get(fTime,NULL,&kDer1,NULL,NULL);
    return kDer1;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NURBSCurve3<Real>::GetSecondDerivative (Real fTime) const
{
    Vector3<Real> kDer2;
    Get(fTime,NULL,NULL,&kDer2,NULL);
    return kDer2;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> NURBSCurve3<Real>::GetThirdDerivative (Real fTime) const
{
    Vector3<Real> kDer3;
    Get(fTime,NULL,NULL,NULL,&kDer3);
    return kDer3;
}
//----------------------------------------------------------------------------
template <class Real>
Real NURBSCurve3<Real>::GetVariation (Real, Real, const Vector3<Real>*,
    const Vector3<Real>*) const
{
    return (Real)0.0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM NURBSCurve3<float>;
Vector3f NURBSCurve3f::ms_kInvalidCtrlPoint = Vector3f::ZERO;

template class WML_ITEM NURBSCurve3<double>;
Vector3d NURBSCurve3d::ms_kInvalidCtrlPoint = Vector3d::ZERO;
}
//----------------------------------------------------------------------------
