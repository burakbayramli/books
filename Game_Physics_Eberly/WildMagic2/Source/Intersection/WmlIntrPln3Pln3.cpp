// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrPln3Pln3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Plane3<Real>& rkPlane0,
    const Plane3<Real>& rkPlane1)
{
    Vector3<Real> kCross = rkPlane0.GetNormal().Cross(rkPlane1.GetNormal());
    Real fSqrLength = kCross.SquaredLength();
    return fSqrLength > Math<Real>::EPSILON;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Plane3<Real>& rkPlane0,
    const Plane3<Real>& rkPlane1, Line3<Real>& rkLine)
{
    // If Cross(N0,N1) is zero, then either planes are parallel and separated
    // or the same plane.  In both cases, 'false' is returned.  Otherwise,
    // the intersection line is
    //
    //   L(t) = t*Cross(N0,N1) + c0*N0 + c1*N1
    //
    // for some coefficients c0 and c1 and for t any real number (the line
    // parameter).  Taking dot products with the normals,
    //
    //   d0 = Dot(N0,L) = c0*Dot(N0,N0) + c1*Dot(N0,N1)
    //   d1 = Dot(N1,L) = c0*Dot(N0,N1) + c1*Dot(N1,N1)
    //
    // which are two equations in two unknowns.  The solution is
    //
    //   c0 = (Dot(N1,N1)*d0 - Dot(N0,N1)*d1)/det
    //   c1 = (Dot(N0,N0)*d1 - Dot(N0,N1)*d0)/det
    //
    // where det = Dot(N0,N0)*Dot(N1,N1)-Dot(N0,N1)^2.

    Real fN00 = rkPlane0.GetNormal().SquaredLength();
    Real fN01 = rkPlane0.GetNormal().Dot(rkPlane1.GetNormal());
    Real fN11 = rkPlane1.GetNormal().SquaredLength();
    Real fDet = fN00*fN11 - fN01*fN01;

    if ( Math<Real>::FAbs(fDet) < Math<Real>::EPSILON )
        return false;

    Real fInvDet = ((Real)1.0)/fDet;
    Real fC0 = (fN11*rkPlane0.GetConstant() -
        fN01*rkPlane1.GetConstant())*fInvDet;
    Real fC1 = (fN00*rkPlane1.GetConstant() -
        fN01*rkPlane0.GetConstant())*fInvDet;

    rkLine.Direction() = rkPlane0.GetNormal().Cross(rkPlane1.GetNormal());
    rkLine.Origin() = fC0*rkPlane0.GetNormal() + fC1*rkPlane1.GetNormal();
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Plane3<float>&,
    const Plane3<float>&);
template WML_ITEM bool FindIntersection<float> (const Plane3<float>&,
    const Plane3<float>&, Line3<float>&);

template WML_ITEM bool TestIntersection<double> (const Plane3<double>&,
    const Plane3<double>&);
template WML_ITEM bool FindIntersection<double> (const Plane3<double>&,
    const Plane3<double>&, Line3<double>&);
}
//----------------------------------------------------------------------------
