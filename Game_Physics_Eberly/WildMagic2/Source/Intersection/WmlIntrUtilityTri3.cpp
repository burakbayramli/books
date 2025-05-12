// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrUtilityTri3.h"
#include "WmlIntrUtilityLin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::TriProjection (const Vector3<Real>& rkD, const Vector3<Real> akV[3],
    Real& rfMin, Real& rfMax)
{
    Real afDot[3] = { rkD.Dot(akV[0]), rkD.Dot(akV[1]), rkD.Dot(akV[2]) };

    rfMin = afDot[0];
    rfMax = rfMin;

    if ( afDot[1] < rfMin )
        rfMin = afDot[1];
    else if ( afDot[1] > rfMax )
        rfMax = afDot[1];

    if ( afDot[2] < rfMin )
        rfMin = afDot[2];
    else if ( afDot[2] > rfMax )
        rfMax = afDot[2];
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::GetTriConfiguration (const Vector3<Real>& rkAxis, 
    const Vector3<Real> akU[3], ContactConfig<Real>& rkConfig)
{
    // find projections of vertices onto potential separating axis
    Real fD0 = rkAxis.Dot(akU[0]);
    Real fD1 = rkAxis.Dot(akU[1]);
    Real fD2 = rkAxis.Dot(akU[2]);

    // explicit sort of vertices to construct a ContactConfig
    if ( fD0 <= fD1 )
    {
        if ( fD1 <= fD2 ) // D0 <= D1 <= D2
        {
            if ( fD0 != fD1 )
            {
                if ( fD1 != fD2 )
                    rkConfig.m_kMap = m111;
                else
                    rkConfig.m_kMap = m12;
            }
            else // ( D0 == D1 )
            {
                if ( fD1 != fD2 )
                    rkConfig.m_kMap = m21;
                else
                    rkConfig.m_kMap = m3;
            }
            rkConfig.m_aiIndex[0] = 0;
            rkConfig.m_aiIndex[1] = 1;
            rkConfig.m_aiIndex[2] = 2;
            rkConfig.m_fMin = fD0;
            rkConfig.m_fMax = fD2;
        }
        else if ( fD0 <= fD2 ) // D0 <= D2 < D1
        {
            if ( fD0 != fD2 )
            {
                rkConfig.m_kMap = m111;
                rkConfig.m_aiIndex[0] = 0;
                rkConfig.m_aiIndex[1] = 2;
                rkConfig.m_aiIndex[2] = 1;
            }
            else
            {
                rkConfig.m_kMap = m21;
                rkConfig.m_aiIndex[0] = 2;
                rkConfig.m_aiIndex[1] = 0;
                rkConfig.m_aiIndex[2] = 1;
            }
            rkConfig.m_fMin = fD0;
            rkConfig.m_fMax = fD1;
        }
        else // D2 < D0 <= D1
        {
            if ( fD0 != fD1 )
                rkConfig.m_kMap = m111;
            else
                rkConfig.m_kMap = m12;

            rkConfig.m_aiIndex[0] = 2;
            rkConfig.m_aiIndex[1] = 0;
            rkConfig.m_aiIndex[2] = 1;
            rkConfig.m_fMin = fD2;
            rkConfig.m_fMax = fD1;
        }
    }
    else if ( fD2 <= fD1 ) // D2 <= D1 < D0
    {
        if ( fD2 != fD1 )
        {
            rkConfig.m_kMap = m111;
            rkConfig.m_aiIndex[0] = 2;
            rkConfig.m_aiIndex[1] = 1;
            rkConfig.m_aiIndex[2] = 0;
        }
        else
        {
            rkConfig.m_kMap = m21;
            rkConfig.m_aiIndex[0] = 1;
            rkConfig.m_aiIndex[1] = 2;
            rkConfig.m_aiIndex[2] = 0;

        }
        rkConfig.m_fMin = fD2;
        rkConfig.m_fMax = fD0;
    }
    else if ( fD2 <= fD0 ) // D1 < D2 <= D0
    {
        if ( fD2 != fD0 ) 
            rkConfig.m_kMap = m111;
        else
            rkConfig.m_kMap = m12;

        rkConfig.m_aiIndex[0] = 1;
        rkConfig.m_aiIndex[1] = 2;
        rkConfig.m_aiIndex[2] = 0;
        rkConfig.m_fMin = fD1;
        rkConfig.m_fMax = fD0;
    }
    else // D1 < D0 < D2
    {
        rkConfig.m_kMap = m111;
        rkConfig.m_aiIndex[0] = 1;
        rkConfig.m_aiIndex[1] = 0;
        rkConfig.m_aiIndex[2] = 2;
        rkConfig.m_fMin = fD1;
        rkConfig.m_fMax = fD2;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::FindContactSetColinearLineTri (const Vector3<Real> akU[2],
    const Vector3<Real> akV[3], int& riQuantity, Vector3<Real>* akP)
{
    // The potential intersection is initialized to the line segment and then
    // clipped against the three sides of the tri

    riQuantity = 2;
    memcpy(akP,akU,2*sizeof(Vector3<Real>));

    Vector3<Real> akSide[3] =
    {
        akV[1] - akV[0],
        akV[2] - akV[1],
        akV[0] - akV[2]
    };

    Vector3<Real> kN = akSide[0].Cross(akSide[1]);
    for (int i = 0; i < 3; i++)
    {
        // normal pointing inside the triangle
        Vector3<Real> kSideN = kN.Cross(akSide[i]);
        Real fConstant = kSideN.Dot(akV[i]);
        ClipConvexPolygonAgainstPlane(kSideN,fConstant,riQuantity,akP);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void TriProjection<float> (const Vector3<float>&,
    const Vector3<float>[3], float&, float&);
template WML_ITEM void GetTriConfiguration<float> (
    const Vector3<float>&, const Vector3<float>[3], ContactConfig<float>&);
template WML_ITEM void FindContactSetColinearLineTri<float> (
    const Vector3<float>[2], const Vector3<float>[3], int&,
    Vector3<float>*);

template WML_ITEM void TriProjection<double> (const Vector3<double>&,
    const Vector3<double>[3], double&, double&);
template WML_ITEM void GetTriConfiguration<double> (
    const Vector3<double>&, const Vector3<double>[3], ContactConfig<double>&);
template WML_ITEM void FindContactSetColinearLineTri<double> (
    const Vector3<double>[2], const Vector3<double>[3], int&,
    Vector3<double>*);
}
//----------------------------------------------------------------------------
