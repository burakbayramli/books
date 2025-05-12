// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprGaussPointsFit3.h"
#include "WmlContLozenge3.h"
#include "WmlDistVec3Lin3.h"
#include "WmlDistVec3Rct3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Lozenge3<Real> Wml::ContLozenge (int iQuantity, const Vector3<Real>* akPoint)
{
    Lozenge3<Real> kLozenge;

    // Fit with Gaussian.  Axis(0) corresponds to the smallest eigenvalue.
    Vector3<Real> kCenter, akAxis[3];
    Real afExtent[3];
    GaussPointsFit(iQuantity,akPoint,kCenter,akAxis,afExtent);

    Vector3<Real> kDiff = akPoint[0] - kCenter;
    Real fWMin = akAxis[0].Dot(kDiff), fWMax = fWMin, fW;
    int i;
    for (i = 1; i < iQuantity; i++)
    {
        kDiff = akPoint[i] - kCenter;
        fW = akAxis[0].Dot(kDiff);
        if ( fW < fWMin )
            fWMin = fW;
        else if ( fW > fWMax )
            fWMax = fW;
    }

    Real fRadius = 0.5f*(fWMax - fWMin);
    Real fRSqr = fRadius*fRadius;
    kCenter += (0.5f*(fWMax + fWMin))*akAxis[0];

    Real fAMin = Math<Real>::MAX_REAL, fAMax = -fAMin;
    Real fBMin = Math<Real>::MAX_REAL, fBMax = -fBMin;
    Real fDiscr, fRadical, fU, fV, fTest;
    for (i = 0; i < iQuantity; i++)
    {
        kDiff = akPoint[i] - kCenter;
        fU = akAxis[2].Dot(kDiff);
        fV = akAxis[1].Dot(kDiff);
        fW = akAxis[0].Dot(kDiff);
        fDiscr = fRSqr - fW*fW;
        fRadical = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));

        fTest = fU + fRadical;
        if ( fTest < fAMin )
            fAMin = fTest;

        fTest = fU - fRadical;
        if ( fTest > fAMax )
            fAMax = fTest;

        fTest = fV + fRadical;
        if ( fTest < fBMin )
            fBMin = fTest;

        fTest = fV - fRadical;
        if ( fTest > fBMax )
            fBMax = fTest;
    }

    // enclosing region might be a capsule or a sphere
    if ( fAMin >= fAMax )
    {
        fTest = ((Real)0.5)*(fAMin + fAMax);
        fAMin = fTest;
        fAMax = fTest;
    }
    if ( fBMin >= fBMax )
    {
        fTest = ((Real)0.5)*(fBMin + fBMax);
        fBMin = fTest;
        fBMax = fTest;
    }

    // Make correction for points inside mitered corner but outside quarter
    // sphere.
    for (i = 0; i < iQuantity; i++)
    {
        kDiff = akPoint[i] - kCenter;
        fU = akAxis[2].Dot(kDiff);
        fV = akAxis[1].Dot(kDiff);

        Real* pfAExtreme = NULL;
        Real* pfBExtreme = NULL;

        if ( fU > fAMax )
        {
            if ( fV > fBMax )
            {
                pfAExtreme = &fAMax;
                pfBExtreme = &fBMax;
            }
            else if ( fV < fBMin )
            {
                pfAExtreme = &fAMax;
                pfBExtreme = &fBMin;
            }
        }
        else if ( fU < fAMin )
        {
            if ( fV > fBMax )
            {
                pfAExtreme = &fAMin;
                pfBExtreme = &fBMax;
            }
            else if ( fV < fBMin )
            {
                pfAExtreme = &fAMin;
                pfBExtreme = &fBMin;
            }
        }

        if ( pfAExtreme )
        {
            Real fDeltaU = fU - *pfAExtreme;
            Real fDeltaV = fV - *pfBExtreme;
            Real fDeltaSumSqr = fDeltaU*fDeltaU + fDeltaV*fDeltaV;
            fW = akAxis[0].Dot(kDiff);
            Real fWSqr = fW*fW;
            fTest = fDeltaSumSqr + fWSqr;
            if ( fTest > fRSqr )
            {
                fDiscr = (fRSqr - fWSqr)/fDeltaSumSqr;
                Real fT = -Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
                *pfAExtreme = fU + fT*fDeltaU;
                *pfBExtreme = fV + fT*fDeltaV;
            }
        }
    }

    if ( fAMin < fAMax )
    {
        if ( fBMin < fBMax )
        {
            kLozenge.Origin() = kCenter + fAMin*akAxis[2] + fBMin*akAxis[1];
            kLozenge.Edge0() = (fAMax - fAMin)*akAxis[2];
            kLozenge.Edge1() = (fBMax - fBMin)*akAxis[1];
        }
        else
        {
            // enclosing lozenge is really a capsule
            kLozenge.Origin() = kCenter + fAMin*akAxis[2] +
                (((Real)0.5)*(fBMin+fBMax))*akAxis[1];
            kLozenge.Edge0() = (fAMax - fAMin)*akAxis[2];
            kLozenge.Edge1() = Vector3<Real>::ZERO;
        }
    }
    else
    {
        if ( fBMin < fBMax )
        {
            // enclosing lozenge is really a capsule
            kLozenge.Origin() = kCenter +
                (((Real)0.5)*(fAMin+fAMax))*akAxis[2] + fBMin*akAxis[1];
            kLozenge.Edge0() = Vector3<Real>::ZERO;
            kLozenge.Edge1() = (fBMax - fBMin)*akAxis[1];
        }
        else
        {
            // enclosing lozenge is really a sphere
            kLozenge.Origin() = kCenter +
                (((Real)0.5)*(fAMin+fAMax))*akAxis[2] +
                (((Real)0.5)*(fBMin+fBMax))*akAxis[1];
            kLozenge.Edge0() = Vector3<Real>::ZERO;
            kLozenge.Edge1() = Vector3<Real>::ZERO;
        }
    }

    kLozenge.Radius() = fRadius;

    return kLozenge;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::ContLozenge (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Lozenge3<Real>& rkLozenge)
{
    // Fit with Gaussian.  Axis(0) corresponds to the smallest eigenvalue.
    Vector3<Real> kCenter, akAxis[3];
    Real afExtent[3];
    if ( !GaussPointsFit(iQuantity,akPoint,abValid,kCenter,akAxis,afExtent) )
        return false;

    Vector3<Real> kDiff;
    Real fWMin, fWMax, fW;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kDiff = akPoint[i] - kCenter;
            fWMin = akAxis[0].Dot(kDiff);
            fWMax = fWMin;
            break;
        }
    }

    for (i++; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kDiff = akPoint[i] - kCenter;
            fW = akAxis[0].Dot(kDiff);
            if ( fW < fWMin )
                fWMin = fW;
            else if ( fW > fWMax )
                fWMax = fW;
        }
    }

    Real fRadius = 0.5f*(fWMax - fWMin);
    Real fRSqr = fRadius*fRadius;
    kCenter += (0.5f*(fWMax + fWMin))*akAxis[0];

    Real fAMin = Math<Real>::MAX_REAL, fAMax = -fAMin;
    Real fBMin = Math<Real>::MAX_REAL, fBMax = -fBMin;
    Real fDiscr, fRadical, fU, fV, fTest;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kDiff = akPoint[i] - kCenter;
            fU = akAxis[2].Dot(kDiff);
            fV = akAxis[1].Dot(kDiff);
            fW = akAxis[0].Dot(kDiff);
            fDiscr = fRSqr - fW*fW;
            fRadical = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));

            fTest = fU + fRadical;
            if ( fTest < fAMin )
                fAMin = fTest;

            fTest = fU - fRadical;
            if ( fTest > fAMax )
                fAMax = fTest;

            fTest = fV + fRadical;
            if ( fTest < fBMin )
                fBMin = fTest;

            fTest = fV - fRadical;
            if ( fTest > fBMax )
                fBMax = fTest;
        }
    }

    // enclosing region might be a capsule or a sphere
    if ( fAMin >= fAMax )
    {
        fTest = ((Real)0.5)*(fAMin + fAMax);
        fAMin = fTest;
        fAMax = fTest;
    }
    if ( fBMin >= fBMax )
    {
        fTest = ((Real)0.5)*(fBMin + fBMax);
        fBMin = fTest;
        fBMax = fTest;
    }

    // Make correction for points inside mitered corner but outside quarter
    // sphere.
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kDiff = akPoint[i] - kCenter;
            fU = akAxis[2].Dot(kDiff);
            fV = akAxis[1].Dot(kDiff);

            Real* pfAExtreme = NULL;
            Real* pfBExtreme = NULL;

            if ( fU > fAMax )
            {
                if ( fV > fBMax )
                {
                    pfAExtreme = &fAMax;
                    pfBExtreme = &fBMax;
                }
                else if ( fV < fBMin )
                {
                    pfAExtreme = &fAMax;
                    pfBExtreme = &fBMin;
                }
            }
            else if ( fU < fAMin )
            {
                if ( fV > fBMax )
                {
                    pfAExtreme = &fAMin;
                    pfBExtreme = &fBMax;
                }
                else if ( fV < fBMin )
                {
                    pfAExtreme = &fAMin;
                    pfBExtreme = &fBMin;
                }
            }

            if ( pfAExtreme )
            {
                Real fDeltaU = fU - *pfAExtreme;
                Real fDeltaV = fV - *pfBExtreme;
                Real fDeltaSumSqr = fDeltaU*fDeltaU + fDeltaV*fDeltaV;
                fW = akAxis[0].Dot(kDiff);
                Real fWSqr = fW*fW;
                fTest = fDeltaSumSqr + fWSqr;
                if ( fTest > fRSqr )
                {
                    fDiscr = (fRSqr - fWSqr)/fDeltaSumSqr;
                    Real fT = -Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
                    *pfAExtreme = fU + fT*fDeltaU;
                    *pfBExtreme = fV + fT*fDeltaV;
                }
            }
        }
    }

    if ( fAMin < fAMax )
    {
        if ( fBMin < fBMax )
        {
            rkLozenge.Origin() = kCenter + fAMin*akAxis[2] + fBMin*akAxis[1];
            rkLozenge.Edge0() = (fAMax - fAMin)*akAxis[2];
            rkLozenge.Edge1() = (fBMax - fBMin)*akAxis[1];
        }
        else
        {
            // enclosing lozenge is really a capsule
            rkLozenge.Origin() = kCenter + fAMin*akAxis[2] +
                (((Real)0.5)*(fBMin+fBMax))*akAxis[1];
            rkLozenge.Edge0() = (fAMax - fAMin)*akAxis[2];
            rkLozenge.Edge1() = Vector3<Real>::ZERO;
        }
    }
    else
    {
        if ( fBMin < fBMax )
        {
            // enclosing lozenge is really a capsule
            rkLozenge.Origin() = kCenter +
                (((Real)0.5)*(fAMin+fAMax))*akAxis[2] + fBMin*akAxis[1];
            rkLozenge.Edge0() = Vector3<Real>::ZERO;
            rkLozenge.Edge1() = (fBMax - fBMin)*akAxis[1];
        }
        else
        {
            // enclosing lozenge is really a sphere
            rkLozenge.Origin() = kCenter +
                (((Real)0.5)*(fAMin+fAMax))*akAxis[2] +
                (((Real)0.5)*(fBMin+fBMax))*akAxis[1];
            rkLozenge.Edge0() = Vector3<Real>::ZERO;
            rkLozenge.Edge1() = Vector3<Real>::ZERO;
        }
    }

    rkLozenge.Radius() = fRadius;

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::InLozenge (const Vector3<Real>& rkPoint,
    const Lozenge3<Real>& rkLozenge, Real fEpsilon)
{
    Real fRSqr = rkLozenge.Radius()*rkLozenge.Radius();
    Real fSqrDist = SqrDistance(rkPoint,rkLozenge.Rectangle());
    return fSqrDist <= fRSqr + fEpsilon;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Lozenge3<float> ContLozenge<float> (int,
    const Vector3<float>*);
template WML_ITEM bool ContLozenge<float> (int, const Vector3<float>*,
    const bool*, Lozenge3<float>&);
template WML_ITEM bool InLozenge<float> (const Vector3<float>&,
    const Lozenge3<float>&, float);

template WML_ITEM Lozenge3<double> ContLozenge<double> (int,
    const Vector3<double>*);
template WML_ITEM bool ContLozenge<double> (int, const Vector3<double>*,
    const bool*, Lozenge3<double>&);
template WML_ITEM bool InLozenge<double> (const Vector3<double>&,
    const Lozenge3<double>&, double);
}
//----------------------------------------------------------------------------
