// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistLin3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Line3<Real>& rkLine0,
    const Line3<Real>& rkLine1, Real* pfLinP0, Real* pfLinP1)
{
    Vector3<Real> kDiff = rkLine0.Origin() - rkLine1.Origin();
    Real fA00 = rkLine0.Direction().SquaredLength();
    Real fA01 = -rkLine0.Direction().Dot(rkLine1.Direction());
    Real fA11 = rkLine1.Direction().SquaredLength();
    Real fB0 = kDiff.Dot(rkLine0.Direction());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fB1, fS, fT, fSqrDist;

    if ( fDet >= Math<Real>::EPSILON )
    {
        // lines are not parallel
        fB1 = -kDiff.Dot(rkLine1.Direction());
        Real fInvDet = ((Real)1.0)/fDet;
        fS = (fA01*fB1-fA11*fB0)*fInvDet;
        fT = (fA01*fB0-fA00*fB1)*fInvDet;
        fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
            fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
    }
    else
    {
        // lines are parallel, select any closest pair of points
        fS = -fB0/fA00;
        fT = (Real)0.0;
        fSqrDist = fB0*fS+fC;
    }

    if ( pfLinP0 )
        *pfLinP0 = fS;

    if ( pfLinP1 )
        *pfLinP1 = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Line3<Real>& rkLine, const Ray3<Real>& rkRay,
    Real* pfLinP, Real* pfRayP)
{
    Vector3<Real> kDiff = rkLine.Origin() - rkRay.Origin();
    Real fA00 = rkLine.Direction().SquaredLength();
    Real fA01 = -rkLine.Direction().Dot(rkRay.Direction());
    Real fA11 = rkRay.Direction().SquaredLength();
    Real fB0 = kDiff.Dot(rkLine.Direction());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fB1, fS, fT, fSqrDist;

    if ( fDet >= Math<Real>::EPSILON )
    {
        fB1 = -kDiff.Dot(rkRay.Direction());
        fT = fA01*fB0-fA00*fB1;

        if ( fT >= (Real)0.0 )
        {
            // two interior points are closest, one on line and one on ray
            Real fInvDet = ((Real)1.0)/fDet;
            fS = (fA01*fB1-fA11*fB0)*fInvDet;
            fT *= fInvDet;
            fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
        }
        else
        {
            // end point of ray and interior point of line are closest
            fS = -fB0/fA00;
            fT = (Real)0.0;
            fSqrDist = fB0*fS+fC;
        }
    }
    else
    {
        // lines are parallel, closest pair with one point at ray origin
        fS = -fB0/fA00;
        fT = (Real)0.0;
        fSqrDist = fB0*fS+fC;
    }

    if ( pfLinP )
        *pfLinP = fS;

    if ( pfRayP )
        *pfRayP = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Line3<Real>& rkLine, const Segment3<Real>& rkSeg,
    Real* pfLinP, Real* pfSegP)
{
    Vector3<Real> kDiff = rkLine.Origin() - rkSeg.Origin();
    Real fA00 = rkLine.Direction().SquaredLength();
    Real fA01 = -rkLine.Direction().Dot(rkSeg.Direction());
    Real fA11 = rkSeg.Direction().SquaredLength();
    Real fB0 = kDiff.Dot(rkLine.Direction());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fB1, fS, fT, fSqrDist;

    if ( fDet >= Math<Real>::EPSILON )
    {
        fB1 = -kDiff.Dot(rkSeg.Direction());
        fT = fA01*fB0-fA00*fB1;

        if ( fT >= (Real)0.0 )
        {
            if ( fT <= fDet )
            {
                // two interior points are closest, one on line and one on
                // segment
                Real fInvDet = ((Real)1.0)/fDet;
                fS = (fA01*fB1-fA11*fB0)*fInvDet;
                fT *= fInvDet;
                fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                    fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
            }
            else
            {
                // end point e1 of segment and interior point of line are
                // closest
                Real fTmp = fA01+fB0;
                fS = -fTmp/fA00;
                fT = (Real)1.0;
                fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
            }
        }
        else
        {
            // end point e0 of segment and interior point of line are closest
            fS = -fB0/fA00;
            fT = (Real)0.0;
            fSqrDist = fB0*fS+fC;
        }
    }
    else
    {
        // lines are parallel, closest pair with one point at segment origin
        fS = -fB0/fA00;
        fT = (Real)0.0;
        fSqrDist = fB0*fS+fC;
    }

    if ( pfLinP )
        *pfLinP = fS;

    if ( pfSegP )
        *pfSegP = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Ray3<Real>& rkRay0, const Ray3<Real>& rkRay1,
    Real* pfRayP0, Real* pfRayP1)
{
    Vector3<Real> kDiff = rkRay0.Origin() - rkRay1.Origin();
    Real fA00 = rkRay0.Direction().SquaredLength();
    Real fA01 = -rkRay0.Direction().Dot(rkRay1.Direction());
    Real fA11 = rkRay1.Direction().SquaredLength();
    Real fB0 = kDiff.Dot(rkRay0.Direction());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fB1, fS, fT, fSqrDist;

    if ( fDet >= Math<Real>::EPSILON )
    {
        // rays are not parallel
        fB1 = -kDiff.Dot(rkRay1.Direction());
        fS = fA01*fB1-fA11*fB0;
        fT = fA01*fB0-fA00*fB1;

        if ( fS >= (Real)0.0 )
        {
            if ( fT >= (Real)0.0 )  // region 0 (interior)
            {
                // minimum at two interior points of rays
                Real fInvDet = ((Real)1.0)/fDet;
                fS *= fInvDet;
                fT *= fInvDet;
                fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                    fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
            }
            else  // region 3 (side)
            {
                fT = (Real)0.0;
                if ( fB0 >= (Real)0.0 )
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
        else
        {
            if ( fT >= (Real)0.0 )  // region 1 (side)
            {
                fS = (Real)0.0;
                if ( fB1 >= (Real)0.0 )
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
            else  // region 2 (corner)
            {
                if ( fB0 < (Real)0.0 )
                {
                    fS = -fB0/fA00;
                    fT = (Real)0.0;
                    fSqrDist = fB0*fS+fC;
                }
                else
                {
                    fS = (Real)0.0;
                    if ( fB1 >= (Real)0.0 )
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
        }
    }
    else
    {
        // rays are parallel
        if ( fA01 > (Real)0.0 )
        {
            // opposite direction vectors
            fT = (Real)0.0;
            if ( fB0 >= (Real)0.0 )
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
        else
        {
            // same direction vectors
            if ( fB0 >= (Real)0.0 )
            {
                fB1 = -kDiff.Dot(rkRay1.Direction());
                fS = (Real)0.0;
                fT = -fB1/fA11;
                fSqrDist = fB1*fT+fC;
            }
            else
            {
                fS = -fB0/fA00;
                fT = (Real)0.0;
                fSqrDist = fB0*fS+fC;
            }
        }
    }

    if ( pfRayP0 )
        *pfRayP0 = fS;

    if ( pfRayP1 )
        *pfRayP1 = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Ray3<Real>& rkRay, const Segment3<Real>& rkSeg,
    Real* pfRayP, Real* pfSegP)
{
    Vector3<Real> kDiff = rkRay.Origin() - rkSeg.Origin();
    Real fA00 = rkRay.Direction().SquaredLength();
    Real fA01 = -rkRay.Direction().Dot(rkSeg.Direction());
    Real fA11 = rkSeg.Direction().SquaredLength();
    Real fB0 = kDiff.Dot(rkRay.Direction());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fB1, fS, fT, fSqrDist, fTmp;

    if ( fDet >= Math<Real>::EPSILON )
    {
        // ray and segment are not parallel
        fB1 = -kDiff.Dot(rkSeg.Direction());
        fS = fA01*fB1-fA11*fB0;
        fT = fA01*fB0-fA00*fB1;

        if ( fS >= (Real)0.0 )
        {
            if ( fT >= (Real)0.0 )
            {
                if ( fT <= fDet )  // region 0
                {
                    // minimum at interior points of ray and segment
                    Real fInvDet = ((Real)1.0)/fDet;
                    fS *= fInvDet;
                    fT *= fInvDet;
                    fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                        fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
                }
                else  // region 1
                {
                    fT = (Real)1.0;
                    if ( fB0 >= -fA01 )
                    {
                        fS = (Real)0.0;
                        fSqrDist = fA11+((Real)2.0)*fB1+fC;
                    }
                    else
                    {
                        fTmp = fA01 + fB0;
                        fS = -fTmp/fA00;
                        fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
                    }
                }
            }
            else  // region 5
            {
                fT = (Real)0.0;
                if ( fB0 >= (Real)0.0 )
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
        else
        {
            if ( fT <= (Real)0.0 )  // region 4
            {
                if ( fB0 < (Real)0.0 )
                {
                    fS = -fB0/fA00;
                    fT = (Real)0.0;
                    fSqrDist = fB0*fS+fC;
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
            else if ( fT <= fDet )  // region 3
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
            else  // region 2
            {
                fTmp = fA01+fB0;
                if ( fTmp < (Real)0.0 )
                {
                    fS = -fTmp/fA00;
                    fT = (Real)1.0;
                    fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
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
        }
    }
    else
    {
        // ray and segment are parallel
        if ( fA01 > (Real)0.0 )
        {
            // opposite direction vectors
            fT = (Real)0.0;
            if ( fB0 >= (Real)0.0 )
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
        else
        {
            // same direction vectors
            fB1 = -kDiff.Dot(rkSeg.Direction());
            fT = (Real)1.0;
            fTmp = fA01+fB0;
            if ( fTmp >= (Real)0.0 )
            {
                fS = (Real)0.0;
                fSqrDist = fA11+((Real)2.0)*fB1+fC;
            }
            else
            {
                fS = -fTmp/fA00;
                fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
            }
        }
    }

    if ( pfRayP )
        *pfRayP = fS;

    if ( pfSegP )
        *pfSegP = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Segment3<Real>& rkSeg0, const Segment3<Real>& rkSeg1,
    Real* pfSegP0, Real* pfSegP1)
{
    Vector3<Real> kDiff = rkSeg0.Origin() - rkSeg1.Origin();
    Real fA00 = rkSeg0.Direction().SquaredLength();
    Real fA01 = -rkSeg0.Direction().Dot(rkSeg1.Direction());
    Real fA11 = rkSeg1.Direction().SquaredLength();
    Real fB0 = kDiff.Dot(rkSeg0.Direction());
    Real fC = kDiff.SquaredLength();
    Real fDet = Math<Real>::FAbs(fA00*fA11-fA01*fA01);
    Real fB1, fS, fT, fSqrDist, fTmp;

    if ( fDet >= Math<Real>::EPSILON )
    {
        // line segments are not parallel
        fB1 = -kDiff.Dot(rkSeg1.Direction());
        fS = fA01*fB1-fA11*fB0;
        fT = fA01*fB0-fA00*fB1;
        
        if ( fS >= (Real)0.0 )
        {
            if ( fS <= fDet )
            {
                if ( fT >= (Real)0.0 )
                {
                    if ( fT <= fDet )  // region 0 (interior)
                    {
                        // minimum at two interior points of 3D lines
                        Real fInvDet = ((Real)1.0)/fDet;
                        fS *= fInvDet;
                        fT *= fInvDet;
                        fSqrDist = fS*(fA00*fS+fA01*fT+((Real)2.0)*fB0) +
                            fT*(fA01*fS+fA11*fT+((Real)2.0)*fB1)+fC;
                    }
                    else  // region 3 (side)
                    {
                        fT = (Real)1.0;
                        fTmp = fA01+fB0;
                        if ( fTmp >= (Real)0.0 )
                        {
                            fS = (Real)0.0;
                            fSqrDist = fA11+((Real)2.0)*fB1+fC;
                        }
                        else if ( -fTmp >= fA00 )
                        {
                            fS = (Real)1.0;
                            fSqrDist = fA00+fA11+fC+((Real)2.0)*(fB1+fTmp);
                        }
                        else
                        {
                            fS = -fTmp/fA00;
                            fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
                        }
                    }
                }
                else  // region 7 (side)
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
            }
            else
            {
                if ( fT >= (Real)0.0 )
                {
                    if ( fT <= fDet )  // region 1 (side)
                    {
                        fS = (Real)1.0;
                        fTmp = fA01+fB1;
                        if ( fTmp >= (Real)0.0 )
                        {
                            fT = (Real)0.0;
                            fSqrDist = fA00+((Real)2.0)*fB0+fC;
                        }
                        else if ( -fTmp >= fA11 )
                        {
                            fT = (Real)1.0;
                            fSqrDist = fA00+fA11+fC+((Real)2.0)*(fB0+fTmp);
                        }
                        else
                        {
                            fT = -fTmp/fA11;
                            fSqrDist = fTmp*fT+fA00+((Real)2.0)*fB0+fC;
                        }
                    }
                    else  // region 2 (corner)
                    {
                        fTmp = fA01+fB0;
                        if ( -fTmp <= fA00 )
                        {
                            fT = (Real)1.0;
                            if ( fTmp >= (Real)0.0 )
                            {
                                fS = (Real)0.0;
                                fSqrDist = fA11+((Real)2.0)*fB1+fC;
                            }
                            else
                            {
                                 fS = -fTmp/fA00;
                                 fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
                            }
                        }
                        else
                        {
                            fS = (Real)1.0;
                            fTmp = fA01+fB1;
                            if ( fTmp >= (Real)0.0 )
                            {
                                fT = (Real)0.0;
                                fSqrDist = fA00+((Real)2.0)*fB0+fC;
                            }
                            else if ( -fTmp >= fA11 )
                            {
                                fT = (Real)1.0;
                                fSqrDist = fA00+fA11+fC+
                                    ((Real)2.0)*(fB0+fTmp);
                            }
                            else
                            {
                                fT = -fTmp/fA11;
                                fSqrDist = fTmp*fT+fA00+((Real)2.0)*fB0+fC;
                            }
                        }
                    }
                }
                else  // region 8 (corner)
                {
                    if ( -fB0 < fA00 )
                    {
                        fT = (Real)0.0;
                        if ( fB0 >= (Real)0.0 )
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
                    else
                    {
                        fS = (Real)1.0;
                        fTmp = fA01+fB1;
                        if ( fTmp >= (Real)0.0 )
                        {
                            fT = (Real)0.0;
                            fSqrDist = fA00+((Real)2.0)*fB0+fC;
                        }
                        else if ( -fTmp >= fA11 )
                        {
                            fT = (Real)1.0;
                            fSqrDist = fA00+fA11+fC+((Real)2.0)*(fB0+fTmp);
                        }
                        else
                        {
                            fT = -fTmp/fA11;
                            fSqrDist = fTmp*fT+fA00+((Real)2.0)*fB0+fC;
                        }
                    }
                }
            }
        }
        else 
        {
            if ( fT >= (Real)0.0 )
            {
                if ( fT <= fDet )  // region 5 (side)
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
                else  // region 4 (corner)
                {
                    fTmp = fA01+fB0;
                    if ( fTmp < (Real)0.0 )
                    {
                        fT = (Real)1.0;
                        if ( -fTmp >= fA00 )
                        {
                            fS = (Real)1.0;
                            fSqrDist = fA00+fA11+fC+((Real)2.0)*(fB1+fTmp);
                        }
                        else
                        {
                            fS = -fTmp/fA00;
                            fSqrDist = fTmp*fS+fA11+((Real)2.0)*fB1+fC;
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
            }
            else   // region 6 (corner)
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
        }
    }
    else
    {
        // line segments are parallel
        if ( fA01 > (Real)0.0 )
        {
            // direction vectors form an obtuse angle
            if ( fB0 >= (Real)0.0 )
            {
                fS = (Real)0.0;
                fT = (Real)0.0;
                fSqrDist = fC;
            }
            else if ( -fB0 <= fA00 )
            {
                fS = -fB0/fA00;
                fT = (Real)0.0;
                fSqrDist = fB0*fS+fC;
            }
            else
            {
                fB1 = -kDiff.Dot(rkSeg1.Direction());
                fS = (Real)1.0;
                fTmp = fA00+fB0;
                if ( -fTmp >= fA01 )
                {
                    fT = (Real)1.0;
                    fSqrDist = fA00+fA11+fC+((Real)2.0)*(fA01+fB0+fB1);
                }
                else
                {
                    fT = -fTmp/fA01;
                    fSqrDist = fA00+((Real)2.0)*fB0+fC+fT*(fA11*fT+
                        ((Real)2.0)*(fA01+fB1));
                }
            }
        }
        else
        {
            // direction vectors form an acute angle
            if ( -fB0 >= fA00 )
            {
                fS = (Real)1.0;
                fT = (Real)0.0;
                fSqrDist = fA00+((Real)2.0)*fB0+fC;
            }
            else if ( fB0 <= (Real)0.0 )
            {
                fS = -fB0/fA00;
                fT = (Real)0.0;
                fSqrDist = fB0*fS+fC;
            }
            else
            {
                fB1 = -kDiff.Dot(rkSeg1.Direction());
                fS = (Real)0.0;
                if ( fB0 >= -fA01 )
                {
                    fT = (Real)1.0;
                    fSqrDist = fA11+((Real)2.0)*fB1+fC;
                }
                else
                {
                    fT = -fB0/fA01;
                    fSqrDist = fC+fT*(((Real)2.0)*fB1+fA11*fT);
                }
            }
        }
    }

    if ( pfSegP0 )
        *pfSegP0 = fS;

    if ( pfSegP1 )
        *pfSegP1 = fT;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Line3<Real>& rkLine0, const Line3<Real>& rkLine1,
    Real* pfLinP0, Real* pfLinP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkLine0,rkLine1,pfLinP0,pfLinP1));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Line3<Real>& rkLine, const Ray3<Real>& rkRay,
    Real* pfLinP, Real* pfRayP)
{
    return Math<Real>::Sqrt(SqrDistance(rkLine,rkRay,pfLinP,pfRayP));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Line3<Real>& rkLine, const Segment3<Real>& rkSeg,
    Real* pfLinP, Real* pfSegP)
{
    return Math<Real>::Sqrt(SqrDistance(rkLine,rkSeg,pfLinP,pfSegP));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Ray3<Real>& rkRay0, const Ray3<Real>& rkRay1,
    Real* pfRayP0, Real* pfRayP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkRay0,rkRay1,pfRayP0,pfRayP1));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Ray3<Real>& rkRay, const Segment3<Real>& rkSeg,
    Real* pfRayP, Real* pfSegP)
{
    return Math<Real>::Sqrt(SqrDistance(rkRay,rkSeg,pfRayP,pfSegP));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Segment3<Real>& rkSeg0,
    const Segment3<Real>& rkSeg1, Real* pfSegP0, Real* pfSegP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkSeg0,rkSeg1,pfSegP0,pfSegP1));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Line3<float>&,
    const Line3<float>&, float*, float*);
template WML_ITEM float SqrDistance<float> (const Line3<float>&,
    const Ray3<float>&, float*, float*);
template WML_ITEM float SqrDistance<float> (const Line3<float>&,
    const Segment3<float>&, float*, float*);
template WML_ITEM float SqrDistance<float> (const Ray3<float>&,
    const Ray3<float>& rkRay1, float*, float*);
template WML_ITEM float SqrDistance<float> (const Ray3<float>&,
    const Segment3<float>&, float*, float*);
template WML_ITEM float SqrDistance<float> (const Segment3<float>&,
    const Segment3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Line3<float>&,
    const Line3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Line3<float>&,
    const Ray3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Line3<float>&,
    const Segment3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Ray3<float>&,
    const Ray3<float>& rkRay1, float*, float*);
template WML_ITEM float Distance<float> (const Ray3<float>&,
    const Segment3<float>&, float*, float*);
template WML_ITEM float Distance<float> (const Segment3<float>&,
    const Segment3<float>&, float*, float*);

template WML_ITEM double SqrDistance<double> (const Line3<double>&,
    const Line3<double>&, double*, double*);
template WML_ITEM double SqrDistance<double> (const Line3<double>&,
    const Ray3<double>&, double*, double*);
template WML_ITEM double SqrDistance<double> (const Line3<double>&,
    const Segment3<double>&, double*, double*);
template WML_ITEM double SqrDistance<double> (const Ray3<double>&,
    const Ray3<double>& rkRay1, double*, double*);
template WML_ITEM double SqrDistance<double> (const Ray3<double>&,
    const Segment3<double>&, double*, double*);
template WML_ITEM double SqrDistance<double> (const Segment3<double>&,
    const Segment3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Line3<double>&,
    const Line3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Line3<double>&,
    const Ray3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Line3<double>&,
    const Segment3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Ray3<double>&,
    const Ray3<double>& rkRay1, double*, double*);
template WML_ITEM double Distance<double> (const Ray3<double>&,
    const Segment3<double>&, double*, double*);
template WML_ITEM double Distance<double> (const Segment3<double>&,
    const Segment3<double>&, double*, double*);
}
//----------------------------------------------------------------------------
