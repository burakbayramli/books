// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContSphere3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Sphere3<Real> Wml::ContSphereOfAABB (int iQuantity,
    const Vector3<Real>* akPoint)
{
    Vector3<Real> kMin = akPoint[0], kMax = kMin;
    for (int i = 1; i < iQuantity; i++)
    {
        if ( akPoint[i].X() < kMin.X() )
            kMin.X() = akPoint[i].X();
        else if ( akPoint[i].X() > kMax.X() )
            kMax.X() = akPoint[i].X();

        if ( akPoint[i].Y() < kMin.Y() )
            kMin.Y() = akPoint[i].Y();
        else if ( akPoint[i].Y() > kMax.Y() )
            kMax.Y() = akPoint[i].Y();

        if ( akPoint[i].Z() < kMin.Z() )
            kMin.Z() = akPoint[i].Z();
        else if ( akPoint[i].Z() > kMax.Z() )
            kMax.Z() = akPoint[i].Z();
    }

    Sphere3<Real> kSphere;
    kSphere.Center() = ((Real)0.5)*(kMax + kMin);
    Vector3<Real> kHalfDiagonal = ((Real)0.5)*(kMax - kMin);
    kSphere.Radius() = kHalfDiagonal.Length();

    return kSphere;
}
//----------------------------------------------------------------------------
template <class Real>
Sphere3<Real> Wml::ContSphereAverage (int iQuantity,
    const Vector3<Real>* akPoint)
{
    Vector3<Real> kCenter = akPoint[0];
    int i;
    for (i = 1; i < iQuantity; i++)
        kCenter += akPoint[i];
    Real fInvQuantity = ((Real)1.0)/iQuantity;
    kCenter *= fInvQuantity;

    Real fMaxRadiusSqr = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        Vector3<Real> kDiff = akPoint[i] - kCenter;
        Real fRadiusSqr = kDiff.SquaredLength();
        if ( fRadiusSqr > fMaxRadiusSqr )
            fMaxRadiusSqr = fRadiusSqr;
    }

    Sphere3<Real> kSphere;
    kSphere.Center() = kCenter;
    kSphere.Radius() = Math<Real>::Sqrt(fMaxRadiusSqr);

    return kSphere;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::ContSphereOfAABB (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Sphere3<Real>& rkSphere)
{
    Vector3<Real> kMin, kMax;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kMin = akPoint[i];
            kMax = kMin;
            break;
        }
    }
    if ( i == iQuantity )
        return false;

    for (i++; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            if ( akPoint[i].X() < kMin.X() )
                kMin.X() = akPoint[i].X();
            else if ( akPoint[i].X() > kMax.X() )
                kMax.X() = akPoint[i].X();

            if ( akPoint[i].Y() < kMin.Y() )
                kMin.Y() = akPoint[i].Y();
            else if ( akPoint[i].Y() > kMax.Y() )
                kMax.Y() = akPoint[i].Y();

            if ( akPoint[i].Z() < kMin.Z() )
                kMin.Z() = akPoint[i].Z();
            else if ( akPoint[i].Z() > kMax.Z() )
                kMax.Z() = akPoint[i].Z();
        }
    }

    rkSphere.Center() = ((Real)0.5)*(kMax + kMin);
    Vector3<Real> kHalfDiagonal = ((Real)0.5)*(kMax - kMin);
    rkSphere.Radius() = kHalfDiagonal.Length();

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::ContSphereAverage (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Sphere3<Real>& rkSphere)
{
    Vector3<Real> kCenter = Vector3<Real>::ZERO;
    int i, iValidQuantity = 0;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kCenter += akPoint[i];
            iValidQuantity++;
        }
    }
    if ( iValidQuantity == 0 )
        return false;

    Real fInvQuantity = ((Real)1.0)/iValidQuantity;
    kCenter *= fInvQuantity;

    Real fMaxRadiusSqr = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            Vector3<Real> kDiff = akPoint[i] - kCenter;
            Real fRadiusSqr = kDiff.SquaredLength();
            if ( fRadiusSqr > fMaxRadiusSqr )
                fMaxRadiusSqr = fRadiusSqr;
        }
    }

    rkSphere.Center() = kCenter;
    rkSphere.Radius() = Math<Real>::Sqrt(fMaxRadiusSqr);

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::InSphere (const Vector3<Real>& rkPoint,
    const Sphere3<Real>& rkSphere, Real fEpsilon)
{
    Real fRSqr = rkSphere.Radius()*rkSphere.Radius();
    Vector3<Real> kDiff = rkPoint - rkSphere.Center();
    Real fSqrDist = kDiff.SquaredLength();
    return fSqrDist <= fRSqr + fEpsilon;
}
//----------------------------------------------------------------------------
template <class Real>
Sphere3<Real> Wml::MergeSpheres (const Sphere3<Real>& rkSphere0,
    const Sphere3<Real>& rkSphere1)
{
    Vector3<Real> kCDiff = rkSphere1.Center() - rkSphere0.Center();
    Real fLSqr = kCDiff.SquaredLength();
    Real fRDiff = rkSphere1.Radius() - rkSphere0.Radius();
    Real fRDiffSqr = fRDiff*fRDiff;

    if ( fRDiffSqr >= fLSqr )
        return ( fRDiff >= (Real)0.0 ? rkSphere1 : rkSphere0 );

    Real fLength = Math<Real>::Sqrt(fLSqr);
    Sphere3<Real> kSphere;

    if ( fLength > Math<Real>::EPSILON )
    {
        Real fCoeff = (fLength + fRDiff)/(((Real)2.0)*fLength);
        kSphere.Center() = rkSphere0.Center() + fCoeff*kCDiff;
    }
    else
    {
        kSphere.Center() = rkSphere0.Center();
    }

    kSphere.Radius() = ((Real)0.5)*(fLength + rkSphere0.Radius() +
        rkSphere1.Radius());

    return kSphere;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Sphere3<float> ContSphereOfAABB<float> (int,
    const Vector3<float>*);
template WML_ITEM Sphere3<float> ContSphereAverage<float> (int,
    const Vector3<float>*);
template WML_ITEM bool ContSphereOfAABB<float> (int,
    const Vector3<float>*, const bool*, Sphere3<float>&);
template WML_ITEM bool ContSphereAverage<float> (int,
    const Vector3<float>*, const bool*, Sphere3<float>&);
template WML_ITEM bool InSphere<float> (const Vector3<float>&,
    const Sphere3<float>&, float);
template WML_ITEM Sphere3<float> MergeSpheres<float>
    (const Sphere3<float>&, const Sphere3<float>&);

template WML_ITEM Sphere3<double> ContSphereOfAABB<double> (int,
    const Vector3<double>*);
template WML_ITEM Sphere3<double> ContSphereAverage<double> (int,
    const Vector3<double>*);
template WML_ITEM bool ContSphereOfAABB<double> (int,
    const Vector3<double>*, const bool*, Sphere3<double>&);
template WML_ITEM bool ContSphereAverage<double> (int,
    const Vector3<double>*, const bool*, Sphere3<double>&);
template WML_ITEM bool InSphere<double> (const Vector3<double>&,
    const Sphere3<double>&, double);
template WML_ITEM Sphere3<double> MergeSpheres<double>
    (const Sphere3<double>&, const Sphere3<double>&);
}
//----------------------------------------------------------------------------
