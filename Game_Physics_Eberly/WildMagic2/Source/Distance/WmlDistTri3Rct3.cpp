// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistTri3Rct3.h"
#include "WmlDistLin3Tri3.h"
#include "WmlDistLin3Rct3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Triangle3<Real>& rkTri,
    const Rectangle3<Real>& rkRct, Real* pfTriP0, Real* pfTriP1,
    Real* pfRctP0, Real* pfRctP1)
{
    Real fS, fT, fS0, fT0;  // triangle parameters
    Real fU, fV, fU0, fV0;  // parallelogram parameters
    Real fSqrDist, fSqrDist0;
    Segment3<Real> kSeg;

    // compare edges of tri against all of pgm
    kSeg.Origin() = rkTri.Origin();
    kSeg.Direction() = rkTri.Edge0();
    fSqrDist = SqrDistance(kSeg,rkRct,&fS,&fU,&fV);
    fT = (Real)0.0;

    kSeg.Direction() = rkTri.Edge1();
    fSqrDist0 = SqrDistance(kSeg,rkRct,&fT0,&fU0,&fV0);
    fS0 = (Real)0.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Origin() = kSeg.Origin() + rkTri.Edge0();
    kSeg.Direction() = kSeg.Direction() - rkTri.Edge0();
    fSqrDist0 = SqrDistance(kSeg,rkRct,&fT0,&fU0,&fV0);
    fS0 = (Real)1.0-fT0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    // compare edges of pgm against all of tri
    kSeg.Origin() = rkRct.Origin();
    kSeg.Direction() = rkRct.Edge0();
    fSqrDist0 = SqrDistance(kSeg,rkTri,&fU0,&fS0,&fT0);
    fV0 = (Real)0.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Direction() = rkRct.Edge1();
    fSqrDist0 = SqrDistance(kSeg,rkTri,&fV0,&fS0,&fT0);
    fU0 = (Real)0.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Origin() = rkRct.Origin() + rkRct.Edge1();
    kSeg.Direction() = rkRct.Edge0();
    fSqrDist0 = SqrDistance(kSeg,rkTri,&fU0,&fS0,&fT0);
    fV0 = (Real)1.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    kSeg.Origin() = rkRct.Origin() + rkRct.Edge0();
    kSeg.Direction() = rkRct.Edge1();
    fSqrDist0 = SqrDistance(kSeg,rkTri,&fV0,&fS0,&fT0);
    fU0 = (Real)1.0;
    if ( fSqrDist0 < fSqrDist )
    {
        fSqrDist = fSqrDist0;
        fS = fS0;
        fT = fT0;
        fU = fU0;
        fV = fV0;
    }

    if ( pfTriP0 )
        *pfTriP0 = fS;

    if ( pfTriP1 )
        *pfTriP1 = fT;

    if ( pfRctP0 )
        *pfRctP0 = fU;

    if ( pfRctP1 )
        *pfRctP1 = fV;

    return Math<Real>::FAbs(fSqrDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Triangle3<Real>& rkTri,
    const Rectangle3<Real>& rkRct, Real* pfTriP0, Real* pfTriP1,
    Real* pfRctP0, Real* pfRctP1)
{
    return Math<Real>::Sqrt(SqrDistance(rkTri,rkRct,pfTriP0,pfTriP1,pfRctP0,
        pfRctP1));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Triangle3<float>&,
    const Rectangle3<float>&, float*, float*, float*, float*);
template WML_ITEM float Distance<float> (const Triangle3<float>&,
    const Rectangle3<float>&, float*, float*, float*, float*);

template WML_ITEM double SqrDistance<double> (const Triangle3<double>&,
    const Rectangle3<double>&, double*, double*, double*, double*);
template WML_ITEM double Distance<double> (const Triangle3<double>&,
    const Rectangle3<double>&, double*, double*, double*, double*);
}
//----------------------------------------------------------------------------
