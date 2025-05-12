// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlBSplineRectangle.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
BSplineRectangle<Real>::BSplineRectangle (int iNumUCtrlPoints,
    int iNumVCtrlPoints, Vector3<Real>** aakCtrlPoint, int iUDegree,
    int iVDegree, bool bULoop,  bool bVLoop, bool bUOpen, bool bVOpen)
    :
    ParametricSurface<Real>((Real)0.0,(Real)1.0,(Real)0.0,(Real)1.0,true)
{
    assert( iNumUCtrlPoints >= 2 );
    assert( 1 <= iUDegree && iUDegree <= iNumUCtrlPoints-1 );
    assert( iNumVCtrlPoints >= 2 );
    assert( 1 <= iVDegree && iVDegree <= iNumVCtrlPoints-1 );

    m_abLoop[0] = bULoop;
    m_abLoop[1] = bVLoop;

    m_iNumUCtrlPoints = iNumUCtrlPoints;
    m_iNumVCtrlPoints = iNumVCtrlPoints;
    m_iUReplicate = ( bULoop ? ( bUOpen ? 1 : iUDegree ) : 0 );
    m_iVReplicate = ( bVLoop ? ( bVOpen ? 1 : iVDegree ) : 0 );
    CreateControl(aakCtrlPoint);

    m_akBasis[0].Create(m_iNumUCtrlPoints+m_iUReplicate,iUDegree,bUOpen);
    m_akBasis[1].Create(m_iNumVCtrlPoints+m_iVReplicate,iVDegree,bVOpen);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineRectangle<Real>::BSplineRectangle (int iNumUCtrlPoints,
    int iNumVCtrlPoints, Vector3<Real>** aakCtrlPoint, int iUDegree,
    int iVDegree, bool bULoop, bool bVLoop, bool bUOpen, Real* afVKnot)
    :
    ParametricSurface<Real>((Real)0.0,(Real)1.0,(Real)0.0,(Real)1.0,true)
{
    assert( iNumUCtrlPoints >= 2 );
    assert( 1 <= iUDegree && iUDegree <= iNumUCtrlPoints-1 );
    assert( iNumVCtrlPoints >= 2 );
    assert( 1 <= iVDegree && iVDegree <= iNumVCtrlPoints-1 );

    m_abLoop[0] = bULoop;
    m_abLoop[1] = bVLoop;

    m_iNumUCtrlPoints = iNumUCtrlPoints;
    m_iNumVCtrlPoints = iNumVCtrlPoints;
    m_iUReplicate = ( bULoop ? ( bUOpen ? 1 : iUDegree ) : 0 );
    m_iVReplicate = ( bVLoop ? 1 : 0 );
    CreateControl(aakCtrlPoint);

    m_akBasis[0].Create(m_iNumUCtrlPoints+m_iUReplicate,iUDegree,bUOpen);
    m_akBasis[1].Create(m_iNumVCtrlPoints+m_iVReplicate,iVDegree,afVKnot);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineRectangle<Real>::BSplineRectangle (int iNumUCtrlPoints,
    int iNumVCtrlPoints, Vector3<Real>** aakCtrlPoint, int iUDegree,
    int iVDegree, bool bULoop, bool bVLoop, Real* afUKnot, bool bVOpen)
    :
    ParametricSurface<Real>((Real)0.0,(Real)1.0,(Real)0.0,(Real)1.0,true)
{
    assert( iNumUCtrlPoints >= 2 );
    assert( 1 <= iUDegree && iUDegree <= iNumUCtrlPoints-1 );
    assert( iNumVCtrlPoints >= 2 );
    assert( 1 <= iVDegree && iVDegree <= iNumVCtrlPoints-1 );

    m_abLoop[0] = bULoop;
    m_abLoop[1] = bVLoop;

    m_iNumUCtrlPoints = iNumUCtrlPoints;
    m_iNumVCtrlPoints = iNumVCtrlPoints;
    m_iUReplicate = ( bULoop ? 1 : 0 );
    m_iVReplicate = ( bVLoop ? ( bVOpen ? 1 : iVDegree ) : 0 );
    CreateControl(aakCtrlPoint);

    m_akBasis[0].Create(m_iNumUCtrlPoints+m_iUReplicate,iUDegree,afUKnot);
    m_akBasis[1].Create(m_iNumVCtrlPoints+m_iVReplicate,iVDegree,bVOpen);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineRectangle<Real>::BSplineRectangle (int iNumUCtrlPoints,
    int iNumVCtrlPoints, Vector3<Real>** aakCtrlPoint, int iUDegree,
    int iVDegree, bool bULoop, bool bVLoop, Real* afUKnot, Real* afVKnot)
    :
    ParametricSurface<Real>((Real)0.0,(Real)1.0,(Real)0.0,(Real)1.0,true)
{
    assert( iNumUCtrlPoints >= 2 );
    assert( 1 <= iUDegree && iUDegree <= iNumUCtrlPoints-1 );
    assert( iNumVCtrlPoints >= 2 );
    assert( 1 <= iVDegree && iVDegree <= iNumVCtrlPoints-1 );

    m_abLoop[0] = bULoop;
    m_abLoop[1] = bVLoop;

    m_iNumUCtrlPoints = iNumUCtrlPoints;
    m_iNumVCtrlPoints = iNumVCtrlPoints;
    m_iUReplicate = ( bULoop ? 1 : 0 );
    m_iVReplicate = ( bVLoop ? 1 : 0 );
    CreateControl(aakCtrlPoint);

    m_akBasis[0].Create(m_iNumUCtrlPoints+m_iUReplicate,iUDegree,afUKnot);
    m_akBasis[1].Create(m_iNumVCtrlPoints+m_iVReplicate,iVDegree,afVKnot);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineRectangle<Real>::~BSplineRectangle ()
{
    delete[] m_aakCtrlPoint[0];
    delete[] m_aakCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
void BSplineRectangle<Real>::CreateControl (Vector3<Real>** aakCtrlPoint)
{
    int iNewNumUCtrlPoints = m_iNumUCtrlPoints + m_iUReplicate;
    int iNewNumVCtrlPoints = m_iNumVCtrlPoints + m_iVReplicate;
    m_aakCtrlPoint = new Vector3<Real>*[iNewNumUCtrlPoints];
    m_aakCtrlPoint[0] =
        new Vector3<Real>[iNewNumUCtrlPoints*iNewNumVCtrlPoints];
    for (int i = 1; i < iNewNumUCtrlPoints; i++)
        m_aakCtrlPoint[i] = &m_aakCtrlPoint[0][i*iNewNumVCtrlPoints];

    for (int iU = 0; iU < iNewNumUCtrlPoints; iU++)
    {
        int iUOld = iU % m_iNumUCtrlPoints;
        for (int iV = 0; iV < iNewNumVCtrlPoints; iV++)
        {
            int iVOld = iV % m_iNumVCtrlPoints;
            m_aakCtrlPoint[iU][iV] = aakCtrlPoint[iUOld][iVOld];
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
int BSplineRectangle<Real>::GetNumCtrlPoints (int iDim) const
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_akBasis[iDim].GetNumCtrlPoints();
}
//----------------------------------------------------------------------------
template <class Real>
int BSplineRectangle<Real>::GetDegree (int iDim) const
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_akBasis[iDim].GetDegree();
}
//----------------------------------------------------------------------------
template <class Real>
bool BSplineRectangle<Real>::IsOpen (int iDim) const
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_akBasis[iDim].IsOpen();
}
//----------------------------------------------------------------------------
template <class Real>
bool BSplineRectangle<Real>::IsUniform (int iDim) const
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_akBasis[iDim].IsUniform();
}
//----------------------------------------------------------------------------
template <class Real>
bool BSplineRectangle<Real>::IsLoop (int iDim) const
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_abLoop[iDim];
}
//----------------------------------------------------------------------------
template <class Real>
void BSplineRectangle<Real>::SetControlPoint (int iUIndex, int iVIndex,
    const Vector3<Real>& rkCtrl)
{
    if ( 0 <= iUIndex && iUIndex < m_iNumUCtrlPoints
    &&   0 <= iVIndex && iVIndex < m_iNumVCtrlPoints )
    {
        // set the control point
        m_aakCtrlPoint[iUIndex][iVIndex] = rkCtrl;

        // set the replicated control point
        bool bDoUReplicate = ( iUIndex < m_iUReplicate );
        bool bDoVReplicate = ( iVIndex < m_iVReplicate );
        int iUExt, iVExt;

        if ( bDoUReplicate )
        {
            iUExt = m_iNumUCtrlPoints + iUIndex;
            m_aakCtrlPoint[iUExt][iVIndex] = rkCtrl;
        }
        if ( bDoVReplicate )
        {
            iVExt = m_iNumVCtrlPoints + iVIndex;
            m_aakCtrlPoint[iUIndex][iVExt] = rkCtrl;
        }
        if ( bDoUReplicate && bDoVReplicate )
            m_aakCtrlPoint[iUExt][iVExt] = rkCtrl;
    }
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>& BSplineRectangle<Real>::GetControlPoint (int iUIndex,
    int iVIndex) const
{
    if ( 0 <= iUIndex && iUIndex < m_iNumUCtrlPoints
    &&   0 <= iVIndex && iVIndex < m_iNumVCtrlPoints )
    {
        return m_aakCtrlPoint[iUIndex][iVIndex];
    }

    return ms_kInvalidCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
Real& BSplineRectangle<Real>::Knot (int iDim, int i)
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_akBasis[iDim].Knot(i);
}
//----------------------------------------------------------------------------
template <class Real>
void BSplineRectangle<Real>::Get (Real fU, Real fV, Vector3<Real>* pkPos,
    Vector3<Real>* pkDerU, Vector3<Real>* pkDerV, Vector3<Real>* pkDerUU,
    Vector3<Real>* pkDerUV, Vector3<Real>* pkDerVV) const
{
    int iU, iUMin, iUMax;
    if ( pkDerUU )
        m_akBasis[0].Compute(fU,2,iUMin,iUMax);
    else if ( pkDerUV || pkDerU )
        m_akBasis[0].Compute(fU,1,iUMin,iUMax);
    else
        m_akBasis[0].Compute(fU,0,iUMin,iUMax);

    int iV, iVMin, iVMax;
    if ( pkDerVV )
        m_akBasis[1].Compute(fV,2,iVMin,iVMax);
    else if ( pkDerUV || pkDerV )
        m_akBasis[1].Compute(fV,1,iVMin,iVMax);
    else
        m_akBasis[1].Compute(fV,0,iVMin,iVMax);

    Real fTmp;

    if ( pkPos )
    {
        *pkPos = Vector3<Real>::ZERO;
        for (iU = iUMin; iU <= iUMax; iU++)
        {
            for (iV = iVMin; iV <= iVMax; iV++)
            {
                fTmp = m_akBasis[0].GetD0(iU)*m_akBasis[1].GetD0(iV);
                *pkPos += fTmp*m_aakCtrlPoint[iU][iV];
            }
        }
    }

    if ( pkDerU )
    {
        *pkDerU = Vector3<Real>::ZERO;
        for (iU = iUMin; iU <= iUMax; iU++)
        {
            for (iV = iVMin; iV <= iVMax; iV++)
            {
                fTmp = m_akBasis[0].GetD1(iU)*m_akBasis[1].GetD0(iV);
                *pkDerU += fTmp*m_aakCtrlPoint[iU][iV];
            }
        }
    }

    if ( pkDerV )
    {
        *pkDerV = Vector3<Real>::ZERO;
        for (iU = iUMin; iU <= iUMax; iU++)
        {
            for (iV = iVMin; iV <= iVMax; iV++)
            {
                fTmp = m_akBasis[0].GetD0(iU)*m_akBasis[1].GetD1(iV);
                *pkDerV += fTmp*m_aakCtrlPoint[iU][iV];
            }
        }
    }

    if ( pkDerUU )
    {
        *pkDerUU = Vector3<Real>::ZERO;
        for (iU = iUMin; iU <= iUMax; iU++)
        {
            for (iV = iVMin; iV <= iVMax; iV++)
            {
                fTmp = m_akBasis[0].GetD2(iU)*m_akBasis[1].GetD0(iV);
                *pkDerUU += fTmp*m_aakCtrlPoint[iU][iV];
            }
        }
    }

    if ( pkDerUV )
    {
        *pkDerUV = Vector3<Real>::ZERO;
        for (iU = iUMin; iU <= iUMax; iU++)
        {
            for (iV = iVMin; iV <= iVMax; iV++)
            {
                fTmp = m_akBasis[0].GetD1(iU)*m_akBasis[1].GetD1(iV);
                *pkDerUV += fTmp*m_aakCtrlPoint[iU][iV];
            }
        }
    }

    if ( pkDerVV )
    {
        *pkDerVV = Vector3<Real>::ZERO;
        for (iU = iUMin; iU <= iUMax; iU++)
        {
            for (iV = iVMin; iV <= iVMax; iV++)
            {
                fTmp = m_akBasis[0].GetD0(iU)*m_akBasis[1].GetD2(iV);
                *pkDerVV += fTmp*m_aakCtrlPoint[iU][iV];
            }
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineRectangle<Real>::GetPosition (Real fU, Real fV) const
{
    Vector3<Real> kPos;
    Get(fU,fV,&kPos,NULL,NULL,NULL,NULL,NULL);
    return kPos;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineRectangle<Real>::GetDerivativeU (Real fU, Real fV) const
{
    Vector3<Real> kDerU;
    Get(fU,fV,NULL,&kDerU,NULL,NULL,NULL,NULL);
    return kDerU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineRectangle<Real>::GetDerivativeV (Real fU, Real fV) const
{
    Vector3<Real> kDerV;
    Get(fU,fV,NULL,NULL,&kDerV,NULL,NULL,NULL);
    return kDerV;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineRectangle<Real>::GetDerivativeUU (Real fU, Real fV) const
{
    Vector3<Real> kDerUU;
    Get(fU,fV,NULL,NULL,NULL,&kDerUU,NULL,NULL);
    return kDerUU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineRectangle<Real>::GetDerivativeUV (Real fU, Real fV) const
{
    Vector3<Real> kDerUV;
    Get(fU,fV,NULL,NULL,NULL,NULL,&kDerUV,NULL);
    return kDerUV;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineRectangle<Real>::GetDerivativeVV (Real fU, Real fV) const
{
    Vector3<Real> kDerVV;
    Get(fU,fV,NULL,NULL,NULL,NULL,NULL,&kDerVV);
    return kDerVV;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM BSplineRectangle<float>;
Vector3<float> BSplineRectanglef::ms_kInvalidCtrlPoint;

template class WML_ITEM BSplineRectangle<double>;
Vector3<double> BSplineRectangled::ms_kInvalidCtrlPoint;
}
//----------------------------------------------------------------------------
