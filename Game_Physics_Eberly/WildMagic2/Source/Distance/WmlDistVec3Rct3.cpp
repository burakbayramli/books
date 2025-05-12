// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Rct3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Rectangle3<Real>& rkRct, Real* pfSParam, Real* pfTParam)
{
    Vector3<Real> kDiff = rkRct.Origin() - rkPoint;
    Real fA00 = rkRct.Edge0().SquaredLength();
    Real fA11 = rkRct.Edge1().SquaredLength();
    Real fB0 = kDiff.Dot(rkRct.Edge0());
    Real fB1 = kDiff.Dot(rkRct.Edge1());
    Real fS = -fB0, fT = -fB1;

    Real fSqrDist = kDiff.SquaredLength();

    if ( fS < (Real)0.0 )
    {
        fS = (Real)0.0;
    }
    else if ( fS <= fA00 )
    {
        fS /= fA00;
        fSqrDist += fB0*fS;
    }
    else
    {
        fS = (Real)1.0;
        fSqrDist += fA00 + ((Real)2.0)*fB0;
    }

    if ( fT < (Real)0.0 )
    {
        fT = (Real)0.0;
    }
    else if ( fT <= fA11 )
    {
        fT /= fA11;
        fSqrDist += fB1*fT;
    }
    else
    {
        fT = (Real)1.0;
        fSqrDist += fA11 + ((Real)2.0)*fB1;
    }

    if ( pfSParam )
        *pfSParam = fS;

    if ( pfTParam )
        *pfTParam = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint,
    const Rectangle3<Real>& rkRct, Real* pfSParam, Real* pfTParam)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkRct,pfSParam,pfTParam));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Rectangle3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Rectangle3<float>&, float*, float*);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Rectangle3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Rectangle3<double>&, double*, double*);
}
//----------------------------------------------------------------------------
