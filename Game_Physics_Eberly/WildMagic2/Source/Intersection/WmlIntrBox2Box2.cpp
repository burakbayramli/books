// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrBox2Box2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Box2<Real>& rkBox0,
    const Box2<Real>& rkBox1)
{
    // convenience variables
    const Vector2<Real>* akA = rkBox0.Axes();
    const Vector2<Real>* akB = rkBox1.Axes();
    const Real* afEA = rkBox0.Extents();
    const Real* afEB = rkBox1.Extents();

    // compute difference of box centers, D = C1-C0
    Vector2<Real> kD = rkBox1.Center() - rkBox0.Center();

    Real aafAbsAdB[2][2], fAbsAdD, fRSum;
    
    // axis C0+t*A0
    aafAbsAdB[0][0] = Math<Real>::FAbs(akA[0].Dot(akB[0]));
    aafAbsAdB[0][1] = Math<Real>::FAbs(akA[0].Dot(akB[1]));
    fAbsAdD = Math<Real>::FAbs(akA[0].Dot(kD));
    fRSum = afEA[0] + afEB[0]*aafAbsAdB[0][0] + afEB[1]*aafAbsAdB[0][1];
    if ( fAbsAdD > fRSum )
        return false;

    // axis C0+t*A1
    aafAbsAdB[1][0] = Math<Real>::FAbs(akA[1].Dot(akB[0]));
    aafAbsAdB[1][1] = Math<Real>::FAbs(akA[1].Dot(akB[1]));
    fAbsAdD = Math<Real>::FAbs(akA[1].Dot(kD));
    fRSum = afEA[1] + afEB[0]*aafAbsAdB[1][0] + afEB[1]*aafAbsAdB[1][1];
    if ( fAbsAdD > fRSum )
        return false;

    // axis C0+t*B0
    fAbsAdD = Math<Real>::FAbs(akB[0].Dot(kD));
    fRSum = afEB[0] + afEA[0]*aafAbsAdB[0][0] + afEA[1]*aafAbsAdB[1][0];
    if ( fAbsAdD > fRSum )
        return false;

    // axis C0+t*B1
    fAbsAdD = Math<Real>::FAbs(akB[1].Dot(kD));
    fRSum = afEB[1] + afEA[0]*aafAbsAdB[0][1] + afEA[1]*aafAbsAdB[1][1];
    if ( fAbsAdD > fRSum )
        return false;

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Box2<float>&,
    const Box2<float>&);

template WML_ITEM bool TestIntersection<double> (const Box2<double>&,
    const Box2<double>&);
}
//----------------------------------------------------------------------------
