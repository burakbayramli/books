// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrHsp3Sph3.h"
#include "WmlIntrUtility3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Sphere3<Real>& rkSphere)
{
    Real fDistance = rkHalfSpace.GetNormal().Dot(rkSphere.Center());
    return fDistance <= rkHalfSpace.GetConstant() + rkSphere.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax)
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkSphVelocity - rkHspVelocity;
    Real fDistance = rkHalfSpace.GetNormal().Dot(rkSphere.Center());

    return AxisTest(kVel,rkHalfSpace.GetNormal(),-Math<Real>::MAX_REAL,
        rkHalfSpace.GetConstant(),fDistance-rkSphere.Radius(),
        fDistance+rkSphere.Radius(),rfTFirst,fTLast,fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Sphere3<Real>& rkSphere,
    const Vector3<Real>& rkSphVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real>& rkP)
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkSphVelocity - rkHspVelocity;
    Real fDistance = rkHalfSpace.GetNormal().Dot(rkSphere.Center());

    if ( !AxisTest(kVel,rkHalfSpace.GetNormal(),-Math<Real>::MAX_REAL,
        rkHalfSpace.GetConstant(),fDistance-rkSphere.Radius(),
        fDistance+rkSphere.Radius(),rfTFirst,fTLast,fTMax) )
    {
        // never intersecting
        return false;
    }

    if ( rfTFirst == (Real)0.0 )
    {
        // intersecting now
        return false;
    }

    riQuantity = 1;
    rkP = rkSphere.Center() + rfTFirst*rkSphVelocity -
        rkSphere.Radius()*rkHalfSpace.GetNormal();

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const HalfSpace3<float>&,
    const Sphere3<float>&);
template WML_ITEM bool TestIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Sphere3<float>&, const Vector3<float>&,
    float&, float);
template WML_ITEM bool FindIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Sphere3<float>&, const Vector3<float>&,
    float&, float, int&, Vector3<float>&);

template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Sphere3<double>&);
template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Sphere3<double>&, const Vector3<double>&,
    double&, double);
template WML_ITEM bool FindIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Sphere3<double>&, const Vector3<double>&,
    double&, double, int&, Vector3<double>&);
}
//----------------------------------------------------------------------------
