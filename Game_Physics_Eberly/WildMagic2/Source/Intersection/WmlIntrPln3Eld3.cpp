// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrPln3Eld3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Plane3<Real>& rkPlane,
    const Ellipsoid3<Real>& rkEllipsoid, bool bUnitNormal)
{
    Vector3<Real> kNormal = rkPlane.GetNormal();
    Real fConstant = rkPlane.GetConstant();
    if ( !bUnitNormal )
    {
        Real fLength = kNormal.Normalize();
        fConstant /= fLength;
    }

    Real fDiscr = kNormal.Dot(rkEllipsoid.InverseA()*kNormal);
    Real fRoot = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
    Real fSDist = kNormal.Dot(rkEllipsoid.Center()) - fConstant;
    return Math<Real>::FAbs(fSDist) <= fRoot;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Culled (const Plane3<Real>& rkPlane,
    const Ellipsoid3<Real>& rkEllipsoid, bool bUnitNormal)
{
    Vector3<Real> kNormal = rkPlane.GetNormal();
    Real fConstant = rkPlane.GetConstant();
    if ( !bUnitNormal )
    {
        Real fLength = kNormal.Normalize();
        fConstant /= fLength;
    }

    Real fDiscr = kNormal.Dot(rkEllipsoid.InverseA()*kNormal);
    Real fRoot = Math<Real>::Sqrt(Math<Real>::FAbs(fDiscr));
    Real fSDist = kNormal.Dot(rkEllipsoid.Center()) - fConstant;
    return fSDist <= -fRoot;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Plane3<float>&,
    const Ellipsoid3<float>&, bool);
template WML_ITEM bool Culled<float> (const Plane3<float>&,
    const Ellipsoid3<float>&, bool);

template WML_ITEM bool TestIntersection<double> (const Plane3<double>&,
    const Ellipsoid3<double>&, bool);
template WML_ITEM bool Culled<double> (const Plane3<double>&,
    const Ellipsoid3<double>&, bool);
}
//----------------------------------------------------------------------------
