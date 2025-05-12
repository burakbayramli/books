// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Tri3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Triangle3<Real>& rkTri, Real* pfSParam, Real* pfTParam)
{
    Vector3<Real> kDiff = rkTri.Origin() - rkPoint;
    Real fA00 = rkTri.Edge0().SquaredLength();
    Real fA01 = rkTri.Edge0().Dot(rkTri.Edge1());
    Real fA11 = rkTri.Edge1().SquaredLength();
    Real fB0 = kDiff.Dot(rkTri.Edge0());
    Real fB1 = kDiff.Dot(rkTri.Edge1());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fS = fA01*fB1-fA11*fB0;
    Real fT = fA01*fB0-fA00*fB1;
    Real fSqrDist;

    if ( fS + fT <= fDet )
    {
        if ( fS < (Real)0.0 )
        {
            if ( fT < (Real)0.0 )  // region 4
            {
                if ( fB0 < (Real)0.0 )
                {
                    fT = (Real)0.0;
                    if ( -fB0 >= fA00 )
                    {
                        fS = (Real)1.0;
                        fSqrDist = fA00+((Real)2.0)*fB0+fC;
                    }
                    else
                    {
                        fS = -fB0/fA00;
                        fSqrDist = fB0*fS+fC;
                    }
                }
                else
                {
                    fS = (Real)0.0;
                    if ( fB1 >= (Real)0.0 )
                    {
                        fT = (Real)0.0;
                        fSqrDist = fC;
                    }
                    else if ( -fB1 >= fA11 )
                    {
                        fT = (Real)1.0;
                        fSqrDist = fA11+((Real)2.0)*fB1+fC;
                    }
                    else
                    {
                        fT = -fB1/fA11;
                        fSqrDist = fB1*fT+fC;
                    }
                }
            }
            else  // region 3
            {
                fS = (Real)0.0;
                if ( fB1 >= (Real)0.0 )
                {
                    fT = (Real)0.0;
                    fSqrDist = fC;
                }
                else if ( -fB1 >= fA11 )
                {
                    fT = (Real)1.0;
                    fSqrDist = fA11+((Real)2.0)*fB1+fC;
                }
                else
                {
                    fT = -fB1/fA11;
                    fSqrDist = fB1*fT+fC;
                }
            }
        }
        else if ( fT < (Real)0.0 )  // region 5
        {
            fT = (Real)0.0;
            if ( fB0 >= (Real)0.0 )
            {
                fS = (Real)0.0;
                fSqrDist = fC;
            }
            else if ( -fB0 >= fA00 )
            {
                fS = (Real)1.0;
                fSqrDist = fA00+((Real)2.0)*fB0+fC;
            }
            else
            {
                fS = -fB0/fA00;
                fSqrDist = fB0*fS+fC;
            }
        }
        else  // region 0
        {
            // minimum at interior point
            Real fInvDet = ((Real)1.0)/fDet;
            fS *= fInvDet;
            fT *= fInvDet;
            fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
        }
    }
    else
    {
        Real fTmp0, fTmp1, fNumer, fDenom;

        if ( fS < (Real)0.0 )  // region 2
        {
            fTmp0 = fA01 + fB0;
            fTmp1 = fA11 + fB1;
            if ( fTmp1 > fTmp0 )
            {
                fNumer = fTmp1 - fTmp0;
                fDenom = fA00-2.0f*fA01+fA11;
                if ( fNumer >= fDenom )
                {
                    fS = (Real)1.0;
                    fT = (Real)0.0;
                    fSqrDist = fA00+((Real)2.0)*fB0+fC;
                }
                else
                {
                    fS = fNumer/fDenom;
                    fT = (Real)1.0 - fS;
                    fSqrDist = fS*(fA00*fS+fA01*fT+2.0f*fB0) +
                        fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
                }
            }
            else
            {
                fS = (Real)0.0;
                if ( fTmp1 <= (Real)0.0 )
                {
                    fT = (Real)1.0;
                    fSqrDist = fA11+((Real)2.0)*fB1+fC;
                }
                else if ( fB1 >= (Real)0.0 )
                {
                    fT = (Real)0.0;
                    fSqrDist = fC;
                }
                else
                {
                    fT = -fB1/fA11;
                    fSqrDist = fB1*fT+fC;
                }
            }
        }
        else if ( fT < (Real)0.0 )  // region 6
        {
            fTmp0 = fA01 + fB1;
            fTmp1 = fA00 + fB0;
            if ( fTmp1 > fTmp0 )
            {
                fNumer = fTmp1 - fTmp0;
                fDenom = fA00-((Real)2.0)*fA01+fA11;
                if ( fNumer >= fDenom )
                {
                    fT = (Real)1.0;
                    fS = (Real)0.0;
                    fSqrDist = fA11+((Real)2.0)*fB1+fC;
                }
                else
                {
                    fT = fNumer/fDenom;
                    fS = (Real)1.0 - fT;
                    fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                        fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
                }
            }
            else
            {
                fT = (Real)0.0;
                if ( fTmp1 <= (Real)0.0 )
                {
                    fS = (Real)1.0;
                    fSqrDist = fA00+((Real)2.0)*fB0+fC;
                }
                else if ( fB0 >= (Real)0.0 )
                {
                    fS = (Real)0.0;
                    fSqrDist = fC;
                }
                else
                {
                    fS = -fB0/fA00;
                    fSqrDist = fB0*fS+fC;
                }
            }
        }
        else  // region 1
        {
            fNumer = fA11 + fB1 - fA01 - fB0;
            if ( fNumer <= (Real)0.0 )
            {
                fS = (Real)0.0;
                fT = (Real)1.0;
                fSqrDist = fA11+((Real)2.0)*fB1+fC;
            }
            else
            {
                fDenom = fA00-2.0f*fA01+fA11;
                if ( fNumer >= fDenom )
                {
                    fS = (Real)1.0;
                    fT = (Real)0.0;
                    fSqrDist = fA00+((Real)2.0)*fB0+fC;
                }
                else
                {
                    fS = fNumer/fDenom;
                    fT = (Real)1.0 - fS;
                    fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                        fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
                }
            }
        }
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
    const Triangle3<Real>& rkTri, Real* pfSParam, Real* pfTParam)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkTri,pfSParam,pfTParam));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Triangle3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Triangle3<float>&, float*, float*);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Triangle3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Triangle3<double>&, double*, double*);
}
//----------------------------------------------------------------------------
