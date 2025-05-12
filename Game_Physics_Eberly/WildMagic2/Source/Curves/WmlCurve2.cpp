// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCurve2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Curve2<Real>::Curve2 (Real fTMin, Real fTMax)
{
    m_fTMin = fTMin;
    m_fTMax = fTMax;
}
//----------------------------------------------------------------------------
template <class Real>
Curve2<Real>::~Curve2 ()
{
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve2<Real>::GetMinTime () const
{
    return m_fTMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve2<Real>::GetMaxTime () const
{
    return m_fTMax;
}
//----------------------------------------------------------------------------
template <class Real>
void Curve2<Real>::SetTimeInterval (Real fTMin, Real fTMax)
{
    assert( fTMin < fTMax );
    m_fTMin = fTMin;
    m_fTMax = fTMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve2<Real>::GetSpeed (Real fTime) const
{
    Vector2<Real> kVelocity = GetFirstDerivative(fTime);
    Real fSpeed = kVelocity.Length();
    return fSpeed;
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve2<Real>::GetTotalLength () const
{
    return GetLength(m_fTMin,m_fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Curve2<Real>::GetTangent (Real fTime) const
{
    Vector2<Real> kVelocity = GetFirstDerivative(fTime);
    kVelocity.Normalize();
    return kVelocity;
}
//----------------------------------------------------------------------------
template <class Real>
Vector2<Real> Curve2<Real>::GetNormal (Real fTime) const
{
    Vector2<Real> kTangent = GetFirstDerivative(fTime);
    kTangent.Normalize();
    Vector2<Real> kNormal = kTangent.Perp();
    return kNormal;
}
//----------------------------------------------------------------------------
template <class Real>
void Curve2<Real>::GetFrame (Real fTime, Vector2<Real>& rkPosition,
    Vector2<Real>& rkTangent, Vector2<Real>& rkNormal) const
{
    rkPosition = GetPosition(fTime);
    rkTangent = GetFirstDerivative(fTime);
    rkTangent.Normalize();
    rkNormal = rkTangent.Perp();
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve2<Real>::GetCurvature (Real fTime) const
{
    Vector2<Real> kDer1 = GetFirstDerivative(fTime);
    Vector2<Real> kDer2 = GetSecondDerivative(fTime);
    Real fSpeedSqr = kDer1.SquaredLength();

    if ( fSpeedSqr >= Math<Real>::EPSILON )
    {
        Real fNumer = kDer1.Kross(kDer2);
        Real fDenom = Math<Real>::Pow(fSpeedSqr,1.5);
        return fNumer/fDenom;
    }
    else
    {
        // curvature is indeterminate, just return 0
        return (Real)0.0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Curve2<Real>::SubdivideByTime (int iNumPoints,
    Vector2<Real>*& rakPoint) const
{
    assert( iNumPoints >= 2 );
    rakPoint = new Vector2<Real>[iNumPoints];

    Real fDelta = (m_fTMax - m_fTMin)/(iNumPoints-1);

    for (int i = 0; i < iNumPoints; i++)
    {
        Real fTime = m_fTMin + fDelta*i;
        rakPoint[i] = GetPosition(fTime);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Curve2<Real>::SubdivideByLength (int iNumPoints,
    Vector2<Real>*& rakPoint) const
{
    assert( iNumPoints >= 2 );
    rakPoint = new Vector2<Real>[iNumPoints];

    Real fDelta = GetTotalLength()/(iNumPoints-1);

    for (int i = 0; i < iNumPoints; i++)
    {
        Real fLength = fDelta*i;
        Real fTime = GetTime(fLength);
        rakPoint[i] = GetPosition(fTime);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Curve2<Real>::SubdivideByVariation (Real fT0, const Vector2<Real>& rkP0,
    Real fT1, const Vector2<Real>& rkP1, Real fMinVariation,
    int iLevel, int& riNumPoints, PointList*& rpkList) const
{
    if ( iLevel > 0 && GetVariation(fT0,fT1,&rkP0,&rkP1) > fMinVariation )
    {
        // too much variation, subdivide interval
        iLevel--;
        Real fTMid = ((Real)0.5)*(fT0+fT1);
        Vector2<Real> kPMid = GetPosition(fTMid);

        SubdivideByVariation(fT0,rkP0,fTMid,kPMid,fMinVariation,iLevel,
            riNumPoints,rpkList);

        SubdivideByVariation(fTMid,kPMid,fT1,rkP1,fMinVariation,iLevel,
            riNumPoints,rpkList);
    }
    else
    {
        // add right end point, left end point was added by neighbor
        rpkList = new PointList(rkP1,rpkList);
        riNumPoints++;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Curve2<Real>::SubdivideByVariation (Real fMinVariation, int iMaxLevel,
    int& riNumPoints, Vector2<Real>*& rakPoint) const
{
    // compute end points of curve
    Vector2<Real> kPMin = GetPosition(m_fTMin);
    Vector2<Real> kPMax = GetPosition(m_fTMax);

    // add left end point to list
    PointList* pkList = new PointList(kPMin,0);
    riNumPoints = 1;

    // binary subdivision, leaf nodes add right end point of subinterval
    SubdivideByVariation(m_fTMin,kPMin,m_fTMax,kPMax,fMinVariation,
        iMaxLevel,riNumPoints,pkList->m_kNext);

    // repackage points in an array
    assert( riNumPoints >= 2 );
    rakPoint = new Vector2<Real>[riNumPoints];
    for (int i = 0; i < riNumPoints; i++)
    {
        assert( pkList );
        rakPoint[i] = pkList->m_kPoint;

        PointList* pkSave = pkList;
        pkList = pkList->m_kNext;
        delete pkSave;
    }
    assert( pkList == 0 );
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM Curve2<float>;
template class WML_ITEM Curve2<double>;
}
//----------------------------------------------------------------------------
