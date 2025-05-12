// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Pln3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint,
    const Vector3<Real>& rkNormal, const Vector3<Real>& rkOrigin,
    Vector3<Real>* pkClosest)
{
    Vector3<Real> kDiff = rkPoint - rkOrigin;
    Real fSignedDist = rkNormal.Dot(kDiff);

    if ( pkClosest )
        *pkClosest = rkPoint - fSignedDist*rkNormal;

    return Math<Real>::FAbs(fSignedDist);
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint,
    const Vector3<Real>& rkNormal, Real fConstant, Vector3<Real>* pkClosest)
{
    Real fSignedDist = rkNormal.Dot(rkPoint) - fConstant;

    if ( pkClosest )
        *pkClosest = rkPoint - fSignedDist*rkNormal;

    return Math<Real>::FAbs(fSignedDist);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Vector3<float>&, const Vector3<float>&, Vector3<float>*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Vector3<float>&, float, Vector3<float>*);

template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Vector3<double>&, const Vector3<double>&, Vector3<double>*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Vector3<double>&, double, Vector3<double>*);
}
//----------------------------------------------------------------------------
