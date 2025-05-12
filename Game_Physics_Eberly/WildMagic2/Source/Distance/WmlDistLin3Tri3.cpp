// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistLin3Tri3.h"
#include "WmlDistLin3Lin3.h"
#include "WmlDistVec3Tri3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Line3<Real>& rkLine,
    const Triangle3<Real>& rkTri, Real* pfLinP, Real* pfTriP0, Real* pfTriP1)
{
    Vector3<Real> kDiff = rkTri.Origin() - rkLine.Origin();
    Real fA00 = rkLine.Direction().SquaredLength();
    Real fA01 = -rkLine.Direction().Dot(rkTri.Edge0());
    Real fA02 = -rkLine.Direction().Dot(rkTri.Edge1());
    Real fA11 = rkTri.Edge0().SquaredLength();
    Real fA12 = rkTri.Edge0().Dot(rkTri.Edge1());
    Real fA22 = rkTri.Edge1().Dot(rkTri.Edge1());
    Real fB0  = -kDiff.Dot(rkLine.Direction());
    Real fB1  = kDiff.Dot(rkTri.Edge0());
    Real fB2  = kDiff.Dot(rkTri.Edge1());

    Segment3<Real> kTriSeg;
    Real fSqrDist, fSqrDist0, fR, fS, fT, fR0, fS0, fT0;

    // Set up for a relative error test on the angle between ray direction
    // and triangle normal to determine parallel/nonparallel status.
    Vector3<Real> kN = rkTri.Edge0().Cross(rkTri.Edge1());
    Real fNSqrLen = kN.SquaredLength();
    Real fDot = rkLine.Direction().Dot(kN);
    bool bNotParallel = (fDot*fDot >= Math<Real>::EPSILON*fA00*fNSqrLen);

    if ( bNotParallel )
    {
        Real fCof00 = fA11*fA22-fA12*fA12;
        Real fCof01 = fA02*fA12-fA01*fA22;
        Real fCof02 = fA01*fA12-fA02*fA11;
        Real fCof11 = fA00*fA22-fA02*fA02;
        Real fCof12 = fA02*fA01-fA00*fA12;
        Real fCof22 = fA00*fA11-fA01*fA01;
        Real fInvDet = ((Real)1.0)/(fA00*fCof00+fA01*fCof01+fA02*fCof02);
        Real fRhs0 = -fB0*fInvDet;
        Real fRhs1 = -fB1*fInvDet;
        Real fRhs2 = -fB2*fInvDet;

        fR = fCof00*fRhs0+fCof01*fRhs1+fCof02*fRhs2;
        fS = fCof01*fRhs0+fCof11*fRhs1+fCof12*fRhs2;
        fT = fCof02*fRhs0+fCof12*fRhs1+fCof22*fRhs2;

        if ( fS+fT <= (Real)1.0 )
        {
            if ( fS < (Real)0.0 )
            {
                if ( fT < (Real)0.0 )  // region 4
                {
                    // min on face s=0 or t=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance<Real>(rkLine,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist0 = SqrDistance<Real>(rkLine,kTriSeg,&fR0,&fS0);
                    fT0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 3
                {
                    // min on face s=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance<Real>(rkLine,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                }
            }
            else if ( fT < (Real)0.0 )  // region 5
            {
                // min on face t=0
                kTriSeg.Origin() = rkTri.Origin();
                kTriSeg.Direction() = rkTri.Edge0();
                fSqrDist = SqrDistance<Real>(rkLine,kTriSeg,&fR,&fS);
                fT = (Real)0.0;
            }
            else  // region 0
            {
                // line intersects triangle
                fSqrDist = (Real)0.0;
            }
        }
        else
        {
            if ( fS < (Real)0.0 )  // region 2
            {
                // min on face s=0 or s+t=1
                kTriSeg.Origin() = rkTri.Origin();
                kTriSeg.Direction() = rkTri.Edge1();
                fSqrDist = SqrDistance<Real>(rkLine,kTriSeg,&fR,&fT);
                fS = (Real)0.0;
                kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                fSqrDist0 = SqrDistance<Real>(rkLine,kTriSeg,&fR0,&fT0);
                fS0 = (Real)1.0-fT0;
                if ( fSqrDist0 < fSqrDist )
                {
                    fSqrDist = fSqrDist0;
                    fR = fR0;
                    fS = fS0;
                    fT = fT0;
                }
            }
            else if ( fT < (Real)0.0 )  // region 6
            {
                // min on face t=0 or s+t=1
                kTriSeg.Origin() = rkTri.Origin();
                kTriSeg.Direction() = rkTri.Edge0();
                fSqrDist = SqrDistance<Real>(rkLine,kTriSeg,&fR,&fS);
                fT = (Real)0.0;
                kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                fSqrDist0 = SqrDistance<Real>(rkLine,kTriSeg,&fR0,&fT0);
                fS0 = (Real)1.0-fT0;
                if ( fSqrDist0 < fSqrDist )
                {
                    fSqrDist = fSqrDist0;
                    fR = fR0;
                    fS = fS0;
                    fT = fT0;
                }
            }
            else  // region 1
            {
                // min on face s+t=1
                kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                fSqrDist = SqrDistance<Real>(rkLine,kTriSeg,&fR,&fT);
                fS = (Real)1.0-fT;
            }
        }
    }
    else
    {
        // line and triangle are parallel
        kTriSeg.Origin() = rkTri.Origin();
        kTriSeg.Direction() = rkTri.Edge0();
        fSqrDist = SqrDistance(rkLine,kTriSeg,&fR,&fS);
        fT = (Real)0.0;

        kTriSeg.Direction() = rkTri.Edge1();
        fSqrDist0 = SqrDistance(rkLine,kTriSeg,&fR0,&fT0);
        fS0 = (Real)0.0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }

        kTriSeg.Origin() = rkTri.Origin() + rkTri.Edge0();
        kTriSeg.Direction() = rkTri.Edge1() - rkTri.Edge0();
        fSqrDist0 = SqrDistance(rkLine,kTriSeg,&fR0,&fT0);
        fS0 = (Real)1.0-fT0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }
    }

    if ( pfLinP )
        *pfLinP = fR;

    if ( pfTriP0 )
        *pfTriP0 = fS;

    if ( pfTriP1 )
        *pfTriP1 = fT;

    return fSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Ray3<Real>& rkRay, const Triangle3<Real>& rkTri,
    Real* pfRayP, Real* pfTriP0, Real* pfTriP1)
{
    Vector3<Real> kDiff = rkTri.Origin() - rkRay.Origin();
    Real fA00 = rkRay.Direction().SquaredLength();
    Real fA01 = -rkRay.Direction().Dot(rkTri.Edge0());
    Real fA02 = -rkRay.Direction().Dot(rkTri.Edge1());
    Real fA11 = rkTri.Edge0().SquaredLength();
    Real fA12 = rkTri.Edge0().Dot(rkTri.Edge1());
    Real fA22 = rkTri.Edge1().Dot(rkTri.Edge1());
    Real fB0  = -kDiff.Dot(rkRay.Direction());
    Real fB1  = kDiff.Dot(rkTri.Edge0());
    Real fB2  = kDiff.Dot(rkTri.Edge1());

    Segment3<Real> kTriSeg;
    Real fSqrDist, fSqrDist0, fR, fS, fT, fR0, fS0, fT0;

    // Set up for a relative error test on the angle between ray direction
    // and triangle normal to determine parallel/nonparallel status.
    Vector3<Real> kN = rkTri.Edge0().Cross(rkTri.Edge1());
    Real fNSqrLen = kN.SquaredLength();
    Real fDot = rkRay.Direction().Dot(kN);
    bool bNotParallel = (fDot*fDot >= Math<Real>::EPSILON*fA00*fNSqrLen);

    if ( bNotParallel )
    {
        Real fCof00 = fA11*fA22-fA12*fA12;
        Real fCof01 = fA02*fA12-fA01*fA22;
        Real fCof02 = fA01*fA12-fA02*fA11;
        Real fCof11 = fA00*fA22-fA02*fA02;
        Real fCof12 = fA02*fA01-fA00*fA12;
        Real fCof22 = fA00*fA11-fA01*fA01;
        Real fInvDet = ((Real)1.0)/(fA00*fCof00+fA01*fCof01+fA02*fCof02);
        Real fRhs0 = -fB0*fInvDet;
        Real fRhs1 = -fB1*fInvDet;
        Real fRhs2 = -fB2*fInvDet;

        fR = fCof00*fRhs0+fCof01*fRhs1+fCof02*fRhs2;
        fS = fCof01*fRhs0+fCof11*fRhs1+fCof12*fRhs2;
        fT = fCof02*fRhs0+fCof12*fRhs1+fCof22*fRhs2;

        if ( fR <= (Real)0.0 )
        {
            if ( fS+fT <= (Real)1.0 )
            {
                if ( fS < (Real)0.0 )
                {
                    if ( fT < (Real)0.0 )  // region 4m
                    {
                        // min on face s=0 or t=0 or r=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge0();
                        fSqrDist0 = SqrDistance(rkRay,kTriSeg,&fR0,&fS0);
                        fT0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                        fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,
                            &fT0);
                        fR0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                    else  // region 3m
                    {
                        // min on face s=0 or r=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,
                            &fT0);
                        fR0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                }
                else if ( fT < (Real)0.0 )  // region 5m
                {
                    // min on face t=0 or r=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 0m
                {
                    // min on face r=0
                    fSqrDist = SqrDistance(rkRay.Origin(),rkTri,&fS,&fT);
                    fR = (Real)0.0;
                }
            }
            else
            {
                if ( fS < (Real)0.0 )  // region 2m
                {
                    // min on face s=0 or s+t=1 or r=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkRay,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                    fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 6m
                {
                    // min on face t=0 or s+t=1 or r=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkRay,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                    fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 1m
                {
                    // min on face s+t=1 or r=0
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fT);
                    fS = (Real)1.0-fT;
                    fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
            }
        }
        else  // fR > 0
        {
            if ( fS+fT <= (Real)1.0 )
            {
                if ( fS < (Real)0.0 )
                {
                    if ( fT < (Real)0.0 )  // region 4p
                    {
                        // min on face s=0 or t=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance<Real>(rkRay,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge0();
                        fSqrDist0 = SqrDistance<Real>(rkRay,kTriSeg,&fR0,
                            &fS0);
                        fT0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                    else  // region 3p
                    {
                        // min on face s=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance<Real>(rkRay,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 5p
                {
                    // min on face t=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance<Real>(rkRay,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                }
                else  // region 0p
                {
                    // ray intersects triangle
                    fSqrDist = (Real)0.0;
                }
            }
            else
            {
                if ( fS < (Real)0.0 )  // region 2p
                {
                    // min on face s=0 or s+t=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance<Real>(rkRay,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance<Real>(rkRay,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 6p
                {
                    // min on face t=0 or s+t=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance<Real>(rkRay,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance<Real>(rkRay,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 1p
                {
                    // min on face s+t=1
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist = SqrDistance<Real>(rkRay,kTriSeg,&fR,&fT);
                    fS = (Real)1.0-fT;
                }
            }
        }
    }
    else
    {
        // ray and triangle are parallel
        kTriSeg.Origin() = rkTri.Origin();
        kTriSeg.Direction() = rkTri.Edge0();
        fSqrDist = SqrDistance(rkRay,kTriSeg,&fR,&fS);
        fT = (Real)0.0;

        kTriSeg.Direction() = rkTri.Edge1();
        fSqrDist0 = SqrDistance(rkRay,kTriSeg,&fR0,&fT0);
        fS0 = (Real)0.0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }

        kTriSeg.Origin() = rkTri.Origin() + rkTri.Edge0();
        kTriSeg.Direction() = rkTri.Edge1() - rkTri.Edge0();
        fSqrDist0 = SqrDistance(rkRay,kTriSeg,&fR0,&fT0);
        fS0 = (Real)1.0-fT0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }

        fSqrDist0 = SqrDistance(rkRay.Origin(),rkTri,&fS0,&fT0);
        fR0 = (Real)0.0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }
    }

    if ( pfRayP )
        *pfRayP = fR;

    if ( pfTriP0 )
        *pfTriP0 = fS;

    if ( pfTriP1 )
        *pfTriP1 = fT;

    return fSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Segment3<Real>& rkSeg, const Triangle3<Real>& rkTri,
    Real* pfSegP, Real* pfTriP0, Real* pfTriP1)
{
    Vector3<Real> kDiff = rkTri.Origin() - rkSeg.Origin();
    Real fA00 = rkSeg.Direction().SquaredLength();
    Real fA01 = -rkSeg.Direction().Dot(rkTri.Edge0());
    Real fA02 = -rkSeg.Direction().Dot(rkTri.Edge1());
    Real fA11 = rkTri.Edge0().SquaredLength();
    Real fA12 = rkTri.Edge0().Dot(rkTri.Edge1());
    Real fA22 = rkTri.Edge1().Dot(rkTri.Edge1());
    Real fB0  = -kDiff.Dot(rkSeg.Direction());
    Real fB1  = kDiff.Dot(rkTri.Edge0());
    Real fB2  = kDiff.Dot(rkTri.Edge1());

    Segment3<Real> kTriSeg;
    Vector3<Real> kPt;
    Real fSqrDist, fSqrDist0, fR, fS, fT, fR0, fS0, fT0;

    // Set up for a relative error test on the angle between ray direction
    // and triangle normal to determine parallel/nonparallel status.
    Vector3<Real> kN = rkTri.Edge0().Cross(rkTri.Edge1());
    Real fNSqrLen = kN.SquaredLength();
    Real fDot = rkSeg.Direction().Dot(kN);
    bool bNotParallel = (fDot*fDot >= Math<Real>::EPSILON*fA00*fNSqrLen);

    if ( bNotParallel )
    {
        Real fCof00 = fA11*fA22-fA12*fA12;
        Real fCof01 = fA02*fA12-fA01*fA22;
        Real fCof02 = fA01*fA12-fA02*fA11;
        Real fCof11 = fA00*fA22-fA02*fA02;
        Real fCof12 = fA02*fA01-fA00*fA12;
        Real fCof22 = fA00*fA11-fA01*fA01;
        Real fInvDet = ((Real)1.0)/(fA00*fCof00+fA01*fCof01+fA02*fCof02);
        Real fRhs0 = -fB0*fInvDet;
        Real fRhs1 = -fB1*fInvDet;
        Real fRhs2 = -fB2*fInvDet;

        fR = fCof00*fRhs0+fCof01*fRhs1+fCof02*fRhs2;
        fS = fCof01*fRhs0+fCof11*fRhs1+fCof12*fRhs2;
        fT = fCof02*fRhs0+fCof12*fRhs1+fCof22*fRhs2;

        if ( fR < (Real)0.0 )
        {
            if ( fS+fT <= (Real)1.0 )
            {
                if ( fS < (Real)0.0 )
                {
                    if ( fT < (Real)0.0 )  // region 4m
                    {
                        // min on face s=0 or t=0 or r=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge0();
                        fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fS0);
                        fT0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                        fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,
                            &fT0);
                        fR0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                    else  // region 3m
                    {
                        // min on face s=0 or r=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,
                            &fT0);
                        fR0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                }
                else if ( fT < (Real)0.0 )  // region 5m
                {
                    // min on face t=0 or r=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 0m
                {
                    // min on face r=0
                    fSqrDist = SqrDistance(rkSeg.Origin(),rkTri,&fS,&fT);
                    fR = (Real)0.0;
                }
            }
            else
            {
                if ( fS < (Real)0.0 )  // region 2m
                {
                    // min on face s=0 or s+t=1 or r=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                    fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 6m
                {
                    // min on face t=0 or s+t=1 or r=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                    fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 1m
                {
                    // min on face s+t=1 or r=0
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                    fS = (Real)1.0-fT;
                    fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,&fT0);
                    fR0 = (Real)0.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
            }
        }
        else if ( fR <= (Real)1.0 )
        {
            if ( fS+fT <= (Real)1.0 )
            {
                if ( fS < (Real)0.0 )
                {
                    if ( fT < (Real)0.0 )  // region 4
                    {
                        // min on face s=0 or t=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge0();
                        fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fS0);
                        fT0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                    else  // region 3
                    {
                        // min on face s=0
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 5
                {
                    // min on face t=0
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                }
                else  // region 0
                {
                    // global minimum is interior, done
                    fSqrDist = (Real)0.0;
                }
            }
            else
            {
                if ( fS < (Real)0.0 )  // region 2
                {
                    // min on face s=0 or s+t=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 6
                {
                    // min on face t=0 or s+t=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 1
                {
                    // min on face s+t=1
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                    fS = (Real)1.0-fT;
                }
            }
        }
        else  // fR > 1
        {
            if ( fS+fT <= (Real)1.0 )
            {
                if ( fS < (Real)0.0 )
                {
                    if ( fT < (Real)0.0 )  // region 4p
                    {
                        // min on face s=0 or t=0 or r=1
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge0();
                        fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fS0);
                        fT0 = (Real)0.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                        kPt = rkSeg.Origin()+rkSeg.Direction();
                        fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
                        fR0 = (Real)1.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                    else  // region 3p
                    {
                        // min on face s=0 or r=1
                        kTriSeg.Origin() = rkTri.Origin();
                        kTriSeg.Direction() = rkTri.Edge1();
                        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                        fS = (Real)0.0;
                        kPt = rkSeg.Origin()+rkSeg.Direction();
                        fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
                        fR0 = (Real)1.0;
                        if ( fSqrDist0 < fSqrDist )
                        {
                            fSqrDist = fSqrDist0;
                            fR = fR0;
                            fS = fS0;
                            fT = fT0;
                        }
                    }
                }
                else if ( fT < (Real)0.0 )  // region 5p
                {
                    // min on face t=0 or r=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    kPt = rkSeg.Origin()+rkSeg.Direction();
                    fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
                    fR0 = (Real)1.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 0p
                {
                    // min face on r=1
                    kPt = rkSeg.Origin()+rkSeg.Direction();
                    fSqrDist = SqrDistance(kPt,rkTri,&fS,&fT);
                    fR = (Real)1.0;
                }
            }
            else
            {
                if ( fS < (Real)0.0 )  // region 2p
                {
                    // min on face s=0 or s+t=1 or r=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge1();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                    fS = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                    kPt = rkSeg.Origin()+rkSeg.Direction();
                    fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
                    fR0 = (Real)1.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else if ( fT < (Real)0.0 )  // region 6p
                {
                    // min on face t=0 or s+t=1 or r=1
                    kTriSeg.Origin() = rkTri.Origin();
                    kTriSeg.Direction() = rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
                    fT = (Real)0.0;
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
                    fS0 = (Real)1.0-fT0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                    kPt = rkSeg.Origin()+rkSeg.Direction();
                    fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
                    fR0 = (Real)1.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
                else  // region 1p
                {
                    // min on face s+t=1 or r=1
                    kTriSeg.Origin() = rkTri.Origin()+rkTri.Edge0();
                    kTriSeg.Direction() = rkTri.Edge1()-rkTri.Edge0();
                    fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fT);
                    fS = (Real)1.0-fT;
                    kPt = rkSeg.Origin()+rkSeg.Direction();
                    fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
                    fR0 = (Real)1.0;
                    if ( fSqrDist0 < fSqrDist )
                    {
                        fSqrDist = fSqrDist0;
                        fR = fR0;
                        fS = fS0;
                        fT = fT0;
                    }
                }
            }
        }
    }
    else
    {
        // segment and triangle are parallel
        kTriSeg.Origin() = rkTri.Origin();
        kTriSeg.Direction() = rkTri.Edge0();
        fSqrDist = SqrDistance(rkSeg,kTriSeg,&fR,&fS);
        fT = (Real)0.0;

        kTriSeg.Direction() = rkTri.Edge1();
        fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
        fS0 = (Real)0.0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }

        kTriSeg.Origin() = rkTri.Origin() + rkTri.Edge0();
        kTriSeg.Direction() = rkTri.Edge1() - rkTri.Edge0();
        fSqrDist0 = SqrDistance(rkSeg,kTriSeg,&fR0,&fT0);
        fS0 = (Real)1.0-fT0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }

        fSqrDist0 = SqrDistance(rkSeg.Origin(),rkTri,&fS0,&fT0);
        fR0 = (Real)0.0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }

        kPt = rkSeg.Origin()+rkSeg.Direction();
        fSqrDist0 = SqrDistance(kPt,rkTri,&fS0,&fT0);
        fR0 = (Real)1.0;
        if ( fSqrDist0 < fSqrDist )
        {
            fSqrDist = fSqrDist0;
            fR = fR0;
            fS = fS0;
            fT = fT0;
        }
    }

    if ( pfSegP )
        *pfSegP = fR;

    if ( pfTriP0 )
        *pfTriP0 = fS;

    if ( pfTriP1 )
        *pfTriP1 = fT;

    return fSqrDist;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Line3<Real>& rkLine, const Triangle3<Real>& rkTri,
    Real* pfLinP, Real* pfTriP0, Real* pfTriP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkLine,rkTri,pfLinP,pfTriP0,pfTriP1));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Ray3<Real>& rkRay, const Triangle3<Real>& rkTri,
    Real* pfRayP, Real* pfTriP0, Real* pfTriP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkRay,rkTri,pfRayP,pfTriP0,pfTriP1));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Segment3<Real>& rkSeg, const Triangle3<Real>& rkTri,
    Real* pfSegP, Real* pfTriP0, Real* pfTriP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkSeg,rkTri,pfSegP,pfTriP0,pfTriP1));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Line3<float>&,
    const Triangle3<float>&, float*, float*, float*);
template WML_ITEM float SqrDistance<float> (const Ray3<float>&,
    const Triangle3<float>&, float*, float*, float*);
template WML_ITEM float SqrDistance<float> (const Segment3<float>&,
    const Triangle3<float>&, float*, float*, float*);
template WML_ITEM float Distance<float> (const Line3<float>&,
    const Triangle3<float>&, float*, float*, float*);
template WML_ITEM float Distance<float> (const Ray3<float>&,
    const Triangle3<float>&, float*, float*, float*);
template WML_ITEM float Distance<float> (const Segment3<float>&,
    const Triangle3<float>&, float*, float*, float*);

template WML_ITEM double SqrDistance<double> (const Line3<double>&,
    const Triangle3<double>&, double*, double*, double*);
template WML_ITEM double SqrDistance<double> (const Ray3<double>&,
    const Triangle3<double>&, double*, double*, double*);
template WML_ITEM double SqrDistance<double> (const Segment3<double>&,
    const Triangle3<double>&, double*, double*, double*);
template WML_ITEM double Distance<double> (const Line3<double>&,
    const Triangle3<double>&, double*, double*, double*);
template WML_ITEM double Distance<double> (const Ray3<double>&,
    const Triangle3<double>&, double*, double*, double*);
template WML_ITEM double Distance<double> (const Segment3<double>&,
    const Triangle3<double>&, double*, double*, double*);
}
//----------------------------------------------------------------------------
