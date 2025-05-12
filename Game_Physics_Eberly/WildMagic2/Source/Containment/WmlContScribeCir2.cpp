// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContScribeCir2.h"
#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::Circumscribe (const Vector2<Real>& rkV0, const Vector2<Real>& rkV1,
    const Vector2<Real>& rkV2, Circle2<Real>& rkCircle)
{
    Vector2<Real> kE10 = rkV1 - rkV0;
    Vector2<Real> kE20 = rkV2 - rkV0;

    Real aafA[2][2] =
    {
        {kE10.X(), kE10.Y()},
        {kE20.X(), kE20.Y()}
    };

    Real afB[2] =
    {
        ((Real)0.5)*kE10.SquaredLength(),
        ((Real)0.5)*kE20.SquaredLength()
    };

    Vector2<Real> kSol;
    if ( LinearSystem<Real>::Solve2(aafA,afB,(Real*)&kSol) )
    {
        rkCircle.Center() = rkV0 + kSol;
        rkCircle.Radius() = kSol.Length();
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Inscribe (const Vector2<Real>& rkV0, const Vector2<Real>& rkV1,
    const Vector2<Real>& rkV2, Circle2<Real>& rkCircle)
{
    // edges
    Vector2<Real> kE0 = rkV1 - rkV0;
    Vector2<Real> kE1 = rkV2 - rkV1;
    Vector2<Real> kE2 = rkV0 - rkV2;

    // normals
    Vector2<Real> kN0 = kE0.Perp();
    Vector2<Real> kN1 = kE1.Perp();
    Vector2<Real> kN2 = kE2.Perp();

    Real fA0 = kN1.Dot(kE0);
    if ( Math<Real>::FAbs(fA0) < Math<Real>::EPSILON )
        return false;

    Real fA1 = kN2.Dot(kE1);
    if ( Math<Real>::FAbs(fA1) < Math<Real>::EPSILON )
        return false;

    Real fA2 = kN0.Dot(kE2);
    if ( Math<Real>::FAbs(fA2) < Math<Real>::EPSILON )
        return false;

    Real fInvA0 = ((Real)1.0)/fA0;
    Real fInvA1 = ((Real)1.0)/fA1;
    Real fInvA2 = ((Real)1.0)/fA2;

    rkCircle.Radius() = ((Real)1.0)/(fInvA0 + fInvA1 + fInvA2);
    rkCircle.Center() = rkCircle.Radius()*(fInvA0*rkV0 + fInvA1*rkV1 +
        fInvA2*rkV2);

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool Circumscribe<float> (const Vector2<float>&,
    const Vector2<float>&, const Vector2<float>&, Circle2<float>&);
template WML_ITEM bool Inscribe<float> (const Vector2<float>&,
    const Vector2<float>&, const Vector2<float>&, Circle2<float>&);

template WML_ITEM bool Circumscribe<double> (const Vector2<double>&,
    const Vector2<double>&, const Vector2<double>&, Circle2<double>&);
template WML_ITEM bool Inscribe<double> (const Vector2<double>&,
    const Vector2<double>&, const Vector2<double>&, Circle2<double>&);
}
//----------------------------------------------------------------------------
