// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Box3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint, const Box3<Real>& rkBox,
    Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    // compute coordinates of point in box coordinate system
    Vector3<Real> kDiff = rkPoint - rkBox.Center();
    Vector3<Real> kClosest(kDiff.Dot(rkBox.Axis(0)),kDiff.Dot(rkBox.Axis(1)),
        kDiff.Dot(rkBox.Axis(2)));

    // project test point onto box
    Real fSqrDistance = (Real)0.0;
    Real fDelta;

    if ( kClosest.X() < -rkBox.Extent(0) )
    {
        fDelta = kClosest.X() + rkBox.Extent(0);
        fSqrDistance += fDelta*fDelta;
        kClosest.X() = -rkBox.Extent(0);
    }
    else if ( kClosest.X() > rkBox.Extent(0) )
    {
        fDelta = kClosest.X() - rkBox.Extent(0);
        fSqrDistance += fDelta*fDelta;
        kClosest.X() = rkBox.Extent(0);
    }

    if ( kClosest.Y() < -rkBox.Extent(1) )
    {
        fDelta = kClosest.Y() + rkBox.Extent(1);
        fSqrDistance += fDelta*fDelta;
        kClosest.Y() = -rkBox.Extent(1);
    }
    else if ( kClosest.Y() > rkBox.Extent(1) )
    {
        fDelta = kClosest.Y() - rkBox.Extent(1);
        fSqrDistance += fDelta*fDelta;
        kClosest.Y() = rkBox.Extent(1);
    }

    if ( kClosest.Z() < -rkBox.Extent(2) )
    {
        fDelta = kClosest.Z() + rkBox.Extent(2);
        fSqrDistance += fDelta*fDelta;
        kClosest.Z() = -rkBox.Extent(2);
    }
    else if ( kClosest.Z() > rkBox.Extent(2) )
    {
        fDelta = kClosest.Z() - rkBox.Extent(2);
        fSqrDistance += fDelta*fDelta;
        kClosest.Z() = rkBox.Extent(2);
    }

    if ( pfBParam0 )
        *pfBParam0 = kClosest.X();

    if ( pfBParam1 )
        *pfBParam1 = kClosest.Y();

    if ( pfBParam2 )
        *pfBParam2 = kClosest.Z();

    return fSqrDistance;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint, const Box3<Real>& rkBox,
    Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkBox,pfBParam0,pfBParam1,
        pfBParam2));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint, Real fXMin, Real fXMax,
    Real fYMin, Real fYMax, Real fZMin, Real fZMax,
    Vector3<Real>* pkClosest)
{
    Real fSqrDistance = (Real)0.0;
    Real fDelta;

    if ( pkClosest )
    {
        if ( rkPoint.X() < fXMin )
        {
            fDelta = rkPoint.X() - fXMin;
            fSqrDistance += fDelta*fDelta;
            pkClosest->X() = fXMin;
        }
        else if ( rkPoint.X() > fXMax )
        {
            fDelta = rkPoint.X() - fXMax;
            fSqrDistance += fDelta*fDelta;
            pkClosest->X() = fXMax;
        }
        else
        {
            pkClosest->X() = rkPoint.X();
        }

        if ( rkPoint.Y() < fYMin )
        {
            fDelta = rkPoint.Y() - fYMin;
            fSqrDistance += fDelta*fDelta;
            pkClosest->Y() = fYMin;
        }
        else if ( rkPoint.Y() > fYMax )
        {
            fDelta = rkPoint.Y() - fYMax;
            fSqrDistance += fDelta*fDelta;
            pkClosest->Y() = fYMax;
        }
        else
        {
            pkClosest->Y() = rkPoint.Y();
        }

        if ( rkPoint.Z() < fZMin )
        {
            fDelta = rkPoint.Z() - fZMin;
            fSqrDistance += fDelta*fDelta;
            pkClosest->Z() = fZMin;
        }
        else if ( rkPoint.Z() > fZMax )
        {
            fDelta = rkPoint.Z() - fZMax;
            fSqrDistance += fDelta*fDelta;
            pkClosest->Z() = fZMax;
        }
        else
        {
            pkClosest->Z() = rkPoint.Z();
        }
    }
    else
    {
        if ( rkPoint.X() < fXMin )
        {
            fDelta = rkPoint.X() - fXMin;
            fSqrDistance += fDelta*fDelta;
        }
        else if ( rkPoint.X() > fXMax )
        {
            fDelta = rkPoint.X() - fXMax;
            fSqrDistance += fDelta*fDelta;
        }

        if ( rkPoint.Y() < fYMin )
        {
            fDelta = rkPoint.Y() - fYMin;
            fSqrDistance += fDelta*fDelta;
        }
        else if ( rkPoint.Y() > fYMax )
        {
            fDelta = rkPoint.Y() - fYMax;
            fSqrDistance += fDelta*fDelta;
        }

        if ( rkPoint.Z() < fZMin )
        {
            fDelta = rkPoint.Z() - fZMin;
            fSqrDistance += fDelta*fDelta;
        }
        else if ( rkPoint.Z() > fZMax )
        {
            fDelta = rkPoint.Z() - fZMax;
            fSqrDistance += fDelta*fDelta;
        }
    }

    return fSqrDistance;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint, Real fXMin, Real fXMax,
    Real fYMin, Real fYMax, Real fZMin, Real fZMax,
    Vector3<Real>* pkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,fXMin,fXMax,fYMin,fYMax,
        fZMin,fZMax,pkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Box3<float>&, float*, float*, float*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Box3<float>&, float*, float*, float*);
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    float, float, float, float, float, float, Vector3<float>*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    float, float, float, float, float, float, Vector3<float>*);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Box3<double>&, double*, double*, double*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Box3<double>&, double*, double*, double*);
template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    double, double, double, double, double, double, Vector3<double>*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    double, double, double, double, double, double, Vector3<double>*);
}
//----------------------------------------------------------------------------
