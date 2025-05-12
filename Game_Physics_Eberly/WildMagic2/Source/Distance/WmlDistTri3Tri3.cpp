// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistTri3Tri3.h"
#include "WmlDistLin3Tri3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Triangle3<Real>& rkTri0,
    const Triangle3<Real>& rkTri1, Real* pfTri0P0, Real* pfTri0P1,
    Real* pfTri1P0, Real* pfTri1P1)
{
    Real fS, fT, fU, fV, fS0, fT0, fU0, fV0, fSqrDist, fSqrDist0;
    Segment3<Real> kSeg;

    // compare edges of tri0 against all of tri1
    kSeg.Origin() = rkTri0.Origin();
    kSeg.Direction() = rkTri0.Edge0();
    fSqrDist = SqrDistance(kSeg,rkTri1,&fS,&fU,&fV);
    fT = (Real)0.0;

    kSeg.Direction() = rkTri0.Edge1();
    fSqrDist0 = SqrDistance(kSeg,rkTri1,&fT0,&fU0,&fV0);
    fS0 = (Real)0.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Origin() = kSeg.Origin() + rkTri0.Edge0();
    kSeg.Direction() = kSeg.Direction() - rkTri0.Edge0();
    fSqrDist0 = SqrDistance(kSeg,rkTri1,&fT0,&fU0,&fV0);
    fS0 = (Real)1.0-fT0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    // compare edges of tri1 against all of tri0
    kSeg.Origin() = rkTri1.Origin();
    kSeg.Direction() = rkTri1.Edge0();
    fSqrDist0 = SqrDistance(kSeg,rkTri0,&fU0,&fS0,&fT0);
    fV0 = (Real)0.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Direction() = rkTri1.Edge1();
    fSqrDist0 = SqrDistance(kSeg,rkTri0,&fV0,&fS0,&fT0);
    fU0 = (Real)0.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Origin() = kSeg.Origin() + rkTri1.Edge0();
    kSeg.Direction() = kSeg.Direction() - rkTri1.Edge0();
    fSqrDist0 = SqrDistance(kSeg,rkTri0,&fV0,&fS0,&fT0);
    fU0 = (Real)1.0-fV0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    if ( pfTri0P0 )
        *pfTri0P0 = fS;

    if ( pfTri0P1 )
        *pfTri0P1 = fT;

    if ( pfTri1P0 )
        *pfTri1P0 = fU;

    if ( pfTri1P1 )
        *pfTri1P1 = fV;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Triangle3<Real>& rkTri0,
    const Triangle3<Real>& rkTri1, Real* pfTri0P0, Real* pfTri0P1,
    Real* pfTri1P0, Real* pfTri1P1)
{
    return Math<Real>::Sqrt(SqrDistance(rkTri0,rkTri1,pfTri0P0,pfTri0P1,
        pfTri1P0,pfTri1P1));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Triangle3<float>&,
    const Triangle3<float>&, float*, float*, float*, float*);
template WML_ITEM float Distance<float> (const Triangle3<float>&,
    const Triangle3<float>&, float*, float*, float*, float*);

template WML_ITEM double SqrDistance<double> (const Triangle3<double>&,
    const Triangle3<double>&, double*, double*, double*, double*);
template WML_ITEM double Distance<double> (const Triangle3<double>&,
    const Triangle3<double>&, double*, double*, double*, double*);
}
//----------------------------------------------------------------------------
