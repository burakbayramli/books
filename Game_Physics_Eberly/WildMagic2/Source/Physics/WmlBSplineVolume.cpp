// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlBSplineVolume.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
BSplineVolume<Real>::BSplineVolume (int iNumUCtrlPoints, int iNumVCtrlPoints,
    int iNumWCtrlPoints, int iUDegree, int iVDegree, int iWDegree)
{
    assert( iNumUCtrlPoints >= 2 );
    assert( 1 <= iUDegree && iUDegree <= iNumUCtrlPoints-1 );
    assert( iNumVCtrlPoints >= 2 );
    assert( 1 <= iVDegree && iVDegree <= iNumVCtrlPoints-1 );
    assert( iNumWCtrlPoints >= 2 );
    assert( 1 <= iWDegree && iWDegree <= iNumWCtrlPoints-1 );

    m_aaakCtrlPoint = new Vector3<Real>**[iNumUCtrlPoints];
    for (int i0 = 0; i0 < iNumUCtrlPoints; i0++)
    {
        m_aaakCtrlPoint[i0] = new Vector3<Real>*[iNumVCtrlPoints];
        for (int i1 = 0; i1 < iNumVCtrlPoints; i1++)
        {
            m_aaakCtrlPoint[i0][i1] = new Vector3<Real>[iNumWCtrlPoints];
            for (int i2 = 0; i2 < iNumWCtrlPoints; i2++)
                m_aaakCtrlPoint[i0][i1][i2] = Vector3<Real>::ZERO;
        }
    }

    m_akBasis[0].Create(iNumUCtrlPoints,iUDegree,true);
    m_akBasis[1].Create(iNumVCtrlPoints,iVDegree,true);
    m_akBasis[2].Create(iNumWCtrlPoints,iWDegree,true);
}
//----------------------------------------------------------------------------
template <class Real>
BSplineVolume<Real>::~BSplineVolume ()
{
    for (int i0 = 0; i0 <  m_akBasis[0].GetNumCtrlPoints(); i0++)
    {
        for (int i1 = 0; i1 < m_akBasis[1].GetNumCtrlPoints(); i1++)
            delete[] m_aaakCtrlPoint[i0][i1];
        delete[] m_aaakCtrlPoint[i0];
    }
    delete[] m_aaakCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
int BSplineVolume<Real>::GetNumCtrlPoints (int iDim) const
{
    assert( 0 <= iDim && iDim <= 2 );
    return m_akBasis[iDim].GetNumCtrlPoints();
}
//----------------------------------------------------------------------------
template <class Real>
int BSplineVolume<Real>::GetDegree (int iDim) const
{
    assert( 0 <= iDim && iDim <= 1 );
    return m_akBasis[iDim].GetDegree();
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real>& BSplineVolume<Real>::ControlPoint (int iUIndex, int iVIndex,
    int iWIndex)
{
    if ( 0 <= iUIndex && iUIndex < m_akBasis[0].GetNumCtrlPoints()
    &&   0 <= iVIndex && iVIndex < m_akBasis[1].GetNumCtrlPoints()
    &&   0 <= iWIndex && iWIndex < m_akBasis[2].GetNumCtrlPoints() )
    {
        return m_aaakCtrlPoint[iUIndex][iVIndex][iWIndex];
    }

    return ms_kInvalidCtrlPoint;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineVolume<Real>::GetPosition (Real fU, Real fV, Real fW)
    const
{
    int iUMin, iUMax, iVMin, iVMax, iWMin, iWMax;
    m_akBasis[0].Compute(fU,0,iUMin,iUMax);
    m_akBasis[1].Compute(fV,0,iVMin,iVMax);
    m_akBasis[2].Compute(fW,0,iWMin,iWMax);

    Vector3<Real> kPos = Vector3<Real>::ZERO;
    for (int iU = iUMin; iU <= iUMax; iU++)
    {
        Real fTmp0 = m_akBasis[0].GetD0(iU);
        for (int iV = iVMin; iV <= iVMax; iV++)
        {
            Real fTmp1 = m_akBasis[1].GetD0(iV);
            for (int iW = iWMin; iW <= iWMax; iW++)
            {
                Real fTmp2 = m_akBasis[2].GetD0(iW);
                Real fProd = fTmp0*fTmp1*fTmp2;
                kPos += fProd*m_aaakCtrlPoint[iU][iV][iW];
            }
        }
    }

    return kPos;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineVolume<Real>::GetDerivativeU (Real fU, Real fV, Real fW)
    const
{
    int iUMin, iUMax, iVMin, iVMax, iWMin, iWMax;
    m_akBasis[0].Compute(fU,1,iUMin,iUMax);
    m_akBasis[1].Compute(fV,0,iVMin,iVMax);
    m_akBasis[2].Compute(fW,0,iWMin,iWMax);

    Vector3<Real> kDerU = Vector3<Real>::ZERO;
    for (int iU = iUMin; iU <= iUMax; iU++)
    {
        Real fTmp0 = m_akBasis[0].GetD1(iU);
        for (int iV = iVMin; iV <= iVMax; iV++)
        {
            Real fTmp1 = m_akBasis[1].GetD0(iV);
            for (int iW = iWMin; iW <= iWMax; iW++)
            {
                Real fTmp2 = m_akBasis[2].GetD0(iW);
                Real fProd = fTmp0*fTmp1*fTmp2;
                kDerU += fProd*m_aaakCtrlPoint[iU][iV][iW];
            }
        }
    }

    return kDerU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineVolume<Real>::GetDerivativeV (Real fU, Real fV, Real fW)
    const
{
    int iUMin, iUMax, iVMin, iVMax, iWMin, iWMax;
    m_akBasis[0].Compute(fU,0,iUMin,iUMax);
    m_akBasis[1].Compute(fV,1,iVMin,iVMax);
    m_akBasis[2].Compute(fW,0,iWMin,iWMax);

    Vector3<Real> kDerU = Vector3<Real>::ZERO;
    for (int iU = iUMin; iU <= iUMax; iU++)
    {
        Real fTmp0 = m_akBasis[0].GetD0(iU);
        for (int iV = iVMin; iV <= iVMax; iV++)
        {
            Real fTmp1 = m_akBasis[1].GetD1(iV);
            for (int iW = iWMin; iW <= iWMax; iW++)
            {
                Real fTmp2 = m_akBasis[2].GetD0(iW);
                Real fProd = fTmp0*fTmp1*fTmp2;
                kDerU += fProd*m_aaakCtrlPoint[iU][iV][iW];
            }
        }
    }

    return kDerU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineVolume<Real>::GetDerivativeW (Real fU, Real fV, Real fW)
    const
{
    int iUMin, iUMax, iVMin, iVMax, iWMin, iWMax;
    m_akBasis[0].Compute(fU,0,iUMin,iUMax);
    m_akBasis[1].Compute(fV,0,iVMin,iVMax);
    m_akBasis[2].Compute(fW,1,iWMin,iWMax);

    Vector3<Real> kDerU = Vector3<Real>::ZERO;
    for (int iU = iUMin; iU <= iUMax; iU++)
    {
        Real fTmp0 = m_akBasis[0].GetD0(iU);
        for (int iV = iVMin; iV <= iVMax; iV++)
        {
            Real fTmp1 = m_akBasis[1].GetD0(iV);
            for (int iW = iWMin; iW <= iWMax; iW++)
            {
                Real fTmp2 = m_akBasis[2].GetD1(iW);
                Real fProd = fTmp0*fTmp1*fTmp2;
                kDerU += fProd*m_aaakCtrlPoint[iU][iV][iW];
            }
        }
    }

    return kDerU;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineVolume<Real>::GetPosition (Real afP[3]) const
{
    return GetPosition(afP[0],afP[1],afP[2]);
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> BSplineVolume<Real>::GetDerivative (int i, Real afP[3]) const
{
    switch ( i )
    {
    case 0:  return GetDerivativeU(afP[0],afP[1],afP[2]);
    case 1:  return GetDerivativeV(afP[0],afP[1],afP[2]);
    case 2:  return GetDerivativeW(afP[0],afP[1],afP[2]);
    }

    // should not get here
    assert( false );
    return Vector3<Real>::ZERO;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class BSplineVolume<float>;
Vector3<float> BSplineVolumef::ms_kInvalidCtrlPoint;

template class BSplineVolume<double>;
Vector3<double> BSplineVolumed::ms_kInvalidCtrlPoint;
}
//----------------------------------------------------------------------------
