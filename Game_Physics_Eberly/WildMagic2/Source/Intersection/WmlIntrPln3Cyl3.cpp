// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrPln3Cyl3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Plane3<Real>& rkPlane,
    const Cylinder3<Real>& rkCylinder, bool bUnitNormal)
{
    Vector3<Real> kNormal = rkPlane.GetNormal();
    Real fConstant = rkPlane.GetConstant();
    if ( !bUnitNormal )
    {
        Real fLength = kNormal.Normalize();
        fConstant /= fLength;
    }

    // Compute extremes of signed distance Dot(N,X)-d for points on the
    // cylinder.  These are
    //   min = (Dot(N,C)-d) - r*sqrt(1-Dot(N,W)^2) - (h/2)*|Dot(N,W)|
    //   max = (Dot(N,C)-d) + r*sqrt(1-Dot(N,W)^2) + (h/2)*|Dot(N,W)|
    Real fSDist = kNormal.Dot(rkCylinder.Center()) - fConstant;
    Real fAbsNdW = Math<Real>::FAbs(kNormal.Dot(rkCylinder.Direction()));
    Real fRoot = Math<Real>::Sqrt(Math<Real>::FAbs((Real)1.0
        - fAbsNdW*fAbsNdW));
    Real fTerm = rkCylinder.Radius()*fRoot +
        ((Real)0.5)*rkCylinder.Height()*fAbsNdW;

    // intersection occurs if and only if 0 is in the interval [min,max]
    return Math<Real>::FAbs(fSDist) <= fTerm;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Culled (const Plane3<Real>& rkPlane,
    const Cylinder3<Real>& rkCylinder, bool bUnitNormal)
{
    Vector3<Real> kNormal = rkPlane.GetNormal();
    Real fConstant = rkPlane.GetConstant();
    if ( !bUnitNormal )
    {
        Real fLength = kNormal.Normalize();
        fConstant /= fLength;
    }

    // Compute extremes of signed distance Dot(N,X)-d for points on the
    // cylinder.  These are
    //   min = (Dot(N,C)-d) - r*sqrt(1-Dot(N,W)^2) - (h/2)*|Dot(N,W)|
    //   max = (Dot(N,C)-d) + r*sqrt(1-Dot(N,W)^2) + (h/2)*|Dot(N,W)|
    Real fSDist = kNormal.Dot(rkCylinder.Center()) - fConstant;
    Real fAbsNdW = Math<Real>::FAbs(kNormal.Dot(rkCylinder.Direction()));
    Real fRoot = Math<Real>::Sqrt(Math<Real>::FAbs((Real)1.0
        - fAbsNdW*fAbsNdW));
    Real fTerm = rkCylinder.Radius()*fRoot +
        ((Real)0.5)*rkCylinder.Height()*fAbsNdW;

    // culling occurs if and only if max <= 0
    return fSDist + fTerm <= (Real)0.0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Plane3<float>&,
    const Cylinder3<float>&, bool);
template WML_ITEM bool Culled<float> (const Plane3<float>&,
    const Cylinder3<float>&, bool);

template WML_ITEM bool TestIntersection<double> (const Plane3<double>&,
    const Cylinder3<double>&, bool);
template WML_ITEM bool Culled<double> (const Plane3<double>&,
    const Cylinder3<double>&, bool);
}
//----------------------------------------------------------------------------
