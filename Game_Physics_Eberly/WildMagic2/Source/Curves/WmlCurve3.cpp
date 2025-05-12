// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCurve3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Curve3<Real>::Curve3 (Real fTMin, Real fTMax)
{
    m_fTMin = fTMin;
    m_fTMax = fTMax;
}
//----------------------------------------------------------------------------
template <class Real>
Curve3<Real>::~Curve3 ()
{
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve3<Real>::GetMinTime () const
{
    return m_fTMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve3<Real>::GetMaxTime () const
{
    return m_fTMax;
}
//----------------------------------------------------------------------------
template <class Real>
void Curve3<Real>::SetTimeInterval (Real fTMin, Real fTMax)
{
    assert( fTMin < fTMax );
    m_fTMin = fTMin;
    m_fTMax = fTMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve3<Real>::GetSpeed (Real fTime) const
{
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    Real fSpeed = kVelocity.Length();
    return fSpeed;
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve3<Real>::GetTotalLength () const
{
    return GetLength(m_fTMin,m_fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Curve3<Real>::GetTangent (Real fTime) const
{
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    kVelocity.Normalize();
    return kVelocity;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Curve3<Real>::GetNormal (Real fTime) const
{
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    Vector3<Real> kAcceleration = GetSecondDerivative(fTime);
    Real fVDotV = kVelocity.Dot(kVelocity);
    Real fVDotA = kVelocity.Dot(kAcceleration);
    Vector3<Real> kNormal = fVDotV*kAcceleration - fVDotA*kVelocity;
    kNormal.Normalize();
    return kNormal;
}
//----------------------------------------------------------------------------
template <class Real>
Vector3<Real> Curve3<Real>::GetBinormal (Real fTime) const
{
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    Vector3<Real> kAcceleration = GetSecondDerivative(fTime);
    Real fVDotV = kVelocity.Dot(kVelocity);
    Real fVDotA = kVelocity.Dot(kAcceleration);
    Vector3<Real> kNormal = fVDotV*kAcceleration - fVDotA*kVelocity;
    kNormal.Normalize();
    kVelocity.Normalize();
    Vector3<Real> kBinormal = kVelocity.Cross(kNormal);
    return kBinormal;
}
//----------------------------------------------------------------------------
template <class Real>
void Curve3<Real>::GetFrame (Real fTime, Vector3<Real>& rkPosition,
    Vector3<Real>& rkTangent, Vector3<Real>& rkNormal,
    Vector3<Real>& rkBinormal) const
{
    rkPosition = GetPosition(fTime);
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    Vector3<Real> kAcceleration = GetSecondDerivative(fTime);
    Real fVDotV = kVelocity.Dot(kVelocity);
    Real fVDotA = kVelocity.Dot(kAcceleration);
    rkNormal = fVDotV*kAcceleration - fVDotA*kVelocity;
    rkNormal.Normalize();
    rkTangent = kVelocity;
    rkTangent.Normalize();
    rkBinormal = rkTangent.Cross(rkNormal);
}
//----------------------------------------------------------------------------
template <class Real>
Real Curve3<Real>::GetCurvature (Real fTime) const
{
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    Real fSpeedSqr = kVelocity.SquaredLength();

    if ( fSpeedSqr >= Math<Real>::EPSILON )
    {
        Vector3<Real> kAcceleration = GetSecondDerivative(fTime);
        Vector3<Real> kCross = kVelocity.Cross(kAcceleration);
        Real fNumer = kCross.Length();
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
Real Curve3<Real>::GetTorsion (Real fTime) const
{
    Vector3<Real> kVelocity = GetFirstDerivative(fTime);
    Vector3<Real> kAcceleration = GetSecondDerivative(fTime);
    Vector3<Real> kCross = kVelocity.Cross(kAcceleration);
    Real fDenom = kCross.SquaredLength();

    if ( fDenom >= Math<Real>::EPSILON )
    {
        Vector3<Real> kJerk = GetThirdDerivative(fTime);
        Real fNumer = kCross.Dot(kJerk);
        return fNumer/fDenom;
    }
    else
    {
        // torsion is indeterminate, just return 0
        return (Real)0.0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Curve3<Real>::SubdivideByTime (int iNumPoints,
    Vector3<Real>*& rakPoint) const
{
    assert( iNumPoints >= 2 );
    rakPoint = new Vector3<Real>[iNumPoints];

    Real fDelta = (m_fTMax - m_fTMin)/(iNumPoints-1);

    for (int i = 0; i < iNumPoints; i++)
    {
        Real fTime = m_fTMin + fDelta*i;
        rakPoint[i] = GetPosition(fTime);
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Curve3<Real>::SubdivideByLength (int iNumPoints,
    Vector3<Real>*& rakPoint) const
{
    assert( iNumPoints >= 2 );
    rakPoint = new Vector3<Real>[iNumPoints];

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
void Curve3<Real>::SubdivideByVariation (Real fT0, const Vector3<Real>& rkP0,
    Real fT1, const Vector3<Real>& rkP1, Real fMinVariation, int iLevel,
    int& riNumPoints, PointList*& rpkList) const
{
    if ( iLevel > 0 && GetVariation(fT0,fT1,&rkP0,&rkP1) > fMinVariation )
    {
        // too much variation, subdivide interval
        iLevel--;
        Real fTMid = ((Real)0.5)*(fT0+fT1);
        Vector3<Real> kPMid = GetPosition(fTMid);

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
void Curve3<Real>::SubdivideByVariation (Real fMinVariation, int iMaxLevel,
    int& riNumPoints, Vector3<Real>*& rakPoint) const
{
    // compute end points of curve
    Vector3<Real> kPMin = GetPosition(m_fTMin);
    Vector3<Real> kPMax = GetPosition(m_fTMax);

    // add left end point to list
    PointList* pkList = new PointList(kPMin,0);
    riNumPoints = 1;

    // binary subdivision, leaf nodes add right end point of subinterval
    SubdivideByVariation(m_fTMin,kPMin,m_fTMax,kPMax,fMinVariation,
        iMaxLevel,riNumPoints,pkList->m_kNext);

    // repackage points in an array
    assert( riNumPoints >= 2 );
    rakPoint = new Vector3<Real>[riNumPoints];
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
template class WML_ITEM Curve3<float>;
template class WML_ITEM Curve3<double>;
}
//----------------------------------------------------------------------------
