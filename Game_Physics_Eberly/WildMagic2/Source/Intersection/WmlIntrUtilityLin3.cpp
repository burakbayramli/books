// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrUtilityLin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::LineProjection (const Vector3<Real>& rkD,
    const Vector3<Real> akV[2], Real& rfMin, Real& rfMax)
{
    Real afDot[2] = { rkD.Dot(akV[0]), rkD.Dot(akV[1]) };

    rfMin = afDot[0];
    rfMax = rfMin;

    if ( afDot[1] < rfMin )
        rfMin = afDot[1];
    else if ( afDot[1] > rfMax )
        rfMax = afDot[1];
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::GetLineConfiguration (const Vector3<Real>& rkAxis, 
    const Vector3<Real> akU[2], ContactConfig<Real>& rkConfig)
{
    Real afDot[2] = { rkAxis.Dot(akU[0]), rkAxis.Dot(akU[1]) };

    if ( Math<Real>::FAbs(afDot[1]-afDot[0]) < Math<Real>::EPSILON )
        rkConfig.m_kMap = m2;
    else
        rkConfig.m_kMap = m11;

    if ( afDot[0] < afDot[1] )
    {
        rkConfig.m_fMin = afDot[0];
        rkConfig.m_fMax = afDot[1];
        rkConfig.m_aiIndex[0] = 0;
        rkConfig.m_aiIndex[1] = 1;
    }
    else
    {
        rkConfig.m_fMin = afDot[1];
        rkConfig.m_fMax = afDot[0];
        rkConfig.m_aiIndex[0] = 1;
        rkConfig.m_aiIndex[1] = 0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::FindContactSetColinearLines (const Vector3<Real> akU[2],
    const Vector3<Real> akV[2], int& riQuantity, Vector3<Real>* akP)
{
    // The potential intersection is initialized to the line segment U
    // and clipped against the endpoints of V

    riQuantity = 2;
    memcpy(akP,akU,2*sizeof(Vector3<Real>));

    // point 0
    Vector3<Real> kV = akV[1] - akV[0];
    Real fC = kV.Dot(akV[0]);
    ClipConvexPolygonAgainstPlane(kV,fC,riQuantity,akP);

    // point 1
    kV = -kV;
    fC = kV.Dot(akV[1]);
    ClipConvexPolygonAgainstPlane(kV,fC,riQuantity,akP);
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::FindContactSetLineThroughPlane (const Vector3<Real> akV[2], 
    const Vector3<Real>& rkU, const Vector3<Real>& rkNormU,
    int& riQuantity, Vector3<Real>* akP)
{
    riQuantity = 1;

    Real fU = rkNormU.Dot(rkU);
    Real fV0 = rkNormU.Dot(akV[0]);
    Real fV1 = rkNormU.Dot(akV[1]);

    // Now that there it has been reduced to a 1-dimensional problem via
    // projection, it becomes easy to find the ratio along V that V 
    // intersects with U.
    Real fRatio = (fU - fV0)/(fV1 - fV0);
    akP[0] = akV[0] + fRatio*(akV[1] - akV[0]);
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::FindContactSetLinLin (const Vector3<Real> akU[2], 
    const Vector3<Real> akV[2], int& riQuantity, Vector3<Real>* akP)
{
    Vector3<Real> kDirU = akU[1] - akU[0];
    Vector3<Real> kDirV = akV[1] - akV[0];
    Vector3<Real> kNorm = kDirU.Cross(kDirV);

    // the comparison is sin(kDirU,kDirV) < epsilon
    Real fUSqrLen = kDirU.SquaredLength();
    Real fVSqrLen = kDirV.SquaredLength();
    Real fNSqrLen = kNorm.SquaredLength();
    if ( fNSqrLen < Math<Real>::EPSILON*fUSqrLen*fVSqrLen )
    {
        FindContactSetColinearLines<Real>(akU,akV,riQuantity,akP);
    }
    else
    {
        FindContactSetLineThroughPlane<Real>(akV,akU[0],
            kNorm.Cross(akU[1]-akU[0]),riQuantity,akP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void LineProjection<float> (const Vector3<float>&,
    const Vector3<float>[2], float&, float&);
template WML_ITEM void GetLineConfiguration<float> (
    const Vector3<float>&, const Vector3<float>[2], ContactConfig<float>&);
template WML_ITEM void FindContactSetColinearLines<float> (
    const Vector3<float>[2], const Vector3<float>[2], int&,
    Vector3<float>*);
template WML_ITEM void FindContactSetLineThroughPlane<float> (
    const Vector3<float>[2], const Vector3<float>&,
    const Vector3<float>&, int&, Vector3<float>*);
template WML_ITEM void FindContactSetLinLin<float> (
    const Vector3<float>[2], const Vector3<float>[2], int&,
    Vector3<float>*);

template WML_ITEM void LineProjection<double> (const Vector3<double>&,
    const Vector3<double>[2], double&, double&);
template WML_ITEM void GetLineConfiguration<double> (
    const Vector3<double>&, const Vector3<double>[2], ContactConfig<double>&);
template WML_ITEM void FindContactSetColinearLines<double> (
    const Vector3<double>[2], const Vector3<double>[2], int&,
    Vector3<double>*);
template WML_ITEM void FindContactSetLineThroughPlane<double> (
    const Vector3<double>[2], const Vector3<double>&,
    const Vector3<double>&, int&, Vector3<double>*);
template WML_ITEM void FindContactSetLinLin<double> (
    const Vector3<double>[2], const Vector3<double>[2], int&,
    Vector3<double>*);
}
//----------------------------------------------------------------------------
