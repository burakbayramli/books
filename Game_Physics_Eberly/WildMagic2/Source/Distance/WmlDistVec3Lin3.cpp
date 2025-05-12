// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Line3<Real>& rkLine, Real* pfParam)
{
    Vector3<Real> kDiff = rkPoint - rkLine.Origin();
    Real fSqrLen = rkLine.Direction().SquaredLength();
    Real fT = kDiff.Dot(rkLine.Direction())/fSqrLen;
    kDiff -= fT*rkLine.Direction();

    if ( pfParam )
        *pfParam = fT;

    return kDiff.SquaredLength();
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint, const Ray3<Real>& rkRay,
    Real* pfParam)
{
    Vector3<Real> kDiff = rkPoint - rkRay.Origin();
    Real fT = kDiff.Dot(rkRay.Direction());

    if ( fT <= (Real)0.0 )
    {
        fT = (Real)0.0;
    }
    else
    {
        fT /= rkRay.Direction().SquaredLength();
        kDiff -= fT*rkRay.Direction();
    }

    if ( pfParam )
        *pfParam = fT;

    return kDiff.SquaredLength();
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Segment3<Real>& rkSegment, Real* pfParam)
{
    Vector3<Real> kDiff = rkPoint - rkSegment.Origin();
    Real fT = kDiff.Dot(rkSegment.Direction());

    if ( fT <= (Real)0.0 )
    {
        fT = (Real)0.0;
    }
    else
    {
        Real fSqrLen= rkSegment.Direction().SquaredLength();
        if ( fT >= fSqrLen )
        {
            fT = (Real)1.0;
            kDiff -= rkSegment.Direction();
        }
        else
        {
            fT /= fSqrLen;
            kDiff -= fT*rkSegment.Direction();
        }
    }

    if ( pfParam )
        *pfParam = fT;

    return kDiff.SquaredLength();
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint, const Line3<Real>& rkLine,
    Real* pfParam)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkLine,pfParam));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint, const Ray3<Real>& rkRay,
    Real* pfParam)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkRay,pfParam));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint,
    const Segment3<Real>& rkSegment, Real* pfParam)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkSegment,pfParam));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Line3<float>&, float*);
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Ray3<float>&, float*);
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Segment3<float>&, float*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Line3<float>&, float*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Ray3<float>&, float*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Segment3<float>&, float*);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Line3<double>&, double*);
template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Ray3<double>&, double*);
template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Segment3<double>&, double*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Line3<double>&, double*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Ray3<double>&, double*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Segment3<double>&, double*);
}
//----------------------------------------------------------------------------
