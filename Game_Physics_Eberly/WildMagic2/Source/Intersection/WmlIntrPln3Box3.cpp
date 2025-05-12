// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrPln3Box3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Plane3<Real>& rkPlane,
    const Box3<Real>& rkBox)
{
    Real fTmp[3] =
    {
        rkBox.Extent(0)*(rkPlane.GetNormal().Dot(rkBox.Axis(0))),
        rkBox.Extent(1)*(rkPlane.GetNormal().Dot(rkBox.Axis(1))),
        rkBox.Extent(2)*(rkPlane.GetNormal().Dot(rkBox.Axis(2)))
    };

    Real fRadius = Math<Real>::FAbs(fTmp[0]) + Math<Real>::FAbs(fTmp[1]) +
        Math<Real>::FAbs(fTmp[2]);

    Real fPseudoDistance = rkPlane.DistanceTo(rkBox.Center());
    return Math<Real>::FAbs(fPseudoDistance) <= fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Culled (const Plane3<Real>& rkPlane, const Box3<Real>& rkBox)
{
    Real fTmp[3] =
    {
        rkBox.Extent(0)*(rkPlane.GetNormal().Dot(rkBox.Axis(0))),
        rkBox.Extent(1)*(rkPlane.GetNormal().Dot(rkBox.Axis(1))),
        rkBox.Extent(2)*(rkPlane.GetNormal().Dot(rkBox.Axis(2)))
    };

    Real fRadius = Math<Real>::FAbs(fTmp[0]) + Math<Real>::FAbs(fTmp[1]) +
        Math<Real>::FAbs(fTmp[2]);

    Real fPseudoDistance = rkPlane.DistanceTo(rkBox.Center());
    return fPseudoDistance <= -fRadius;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Plane3<float>&,
    const Box3<float>&);
template WML_ITEM bool Culled <float>(const Plane3<float>&,
    const Box3<float>&);

template WML_ITEM bool TestIntersection<double> (const Plane3<double>&,
    const Box3<double>&);
template WML_ITEM bool Culled <double>(const Plane3<double>&,
    const Box3<double>&);
}
//----------------------------------------------------------------------------
