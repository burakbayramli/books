// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContScribeCir3Sph3.h"
#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::Circumscribe (const Vector3<Real>& rkV0, const Vector3<Real>& rkV1,
    const Vector3<Real>& rkV2, Circle3<Real>& rkCircle)
{
    Vector3<Real> kE02 = rkV0 - rkV2;
    Vector3<Real> kE12 = rkV1 - rkV2;
    Real fE02E02 = kE02.Dot(kE02);
    Real fE02E12 = kE02.Dot(kE12);
    Real fE12E12 = kE12.Dot(kE12);
    Real fDet = fE02E02*fE12E12 - fE02E12*fE02E12;
    if ( Math<Real>::FAbs(fDet) < Math<Real>::EPSILON )
        return false;

    Real fHalfInvDet = ((Real)0.5)/fDet;
    Real fU0 = fHalfInvDet*fE12E12*(fE02E02 - fE02E12);
    Real fU1 = fHalfInvDet*fE02E02*(fE12E12 - fE02E12);
    Vector3<Real> kTmp = fU0*kE02 + fU1*kE12;

    rkCircle.Center() = rkV2 + kTmp;
    rkCircle.Radius() = kTmp.Length();

    rkCircle.N() = kE02.UnitCross(kE12);
    Vector3<Real>& rkN = rkCircle.N();
    Vector3<Real>& rkU = rkCircle.U();
    if ( Math<Real>::FAbs(rkN.X()) >= Math<Real>::FAbs(rkN.Y())
    &&   Math<Real>::FAbs(rkN.X()) >= Math<Real>::FAbs(rkN.Z()) )
    {
        rkU.X() = -rkN.Y();
        rkU.Y() = rkN.X();
        rkU.Z() = (Real)0.0;
    }
    else
    {
        rkU.X() = (Real)0.0;
        rkU.Y() = rkN.Z();
        rkU.Z() = -rkN.Y();
    }

    rkU.Normalize();
    rkCircle.V() = rkCircle.N().Cross(rkCircle.U());

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Circumscribe (const Vector3<Real>& rkV0, const Vector3<Real>& rkV1,
    const Vector3<Real>& rkV2, const Vector3<Real>& rkV3,
    Sphere3<Real>& rkSphere)
{
    Vector3<Real> kE10 = rkV1 - rkV0;
    Vector3<Real> kE20 = rkV2 - rkV0;
    Vector3<Real> kE30 = rkV3 - rkV0;

    Real aafA[3][3] =
    {
        {kE10.X(), kE10.Y(), kE10.Z()},
        {kE20.X(), kE20.Y(), kE20.Z()},
        {kE30.X(), kE30.Y(), kE30.Z()}
    };

    Real afB[3] =
    {
        0.5f*kE10.SquaredLength(),
        0.5f*kE20.SquaredLength(),
        0.5f*kE30.SquaredLength()
    };

    Vector3<Real> kSol;
    if ( LinearSystem<Real>::Solve3(aafA,afB,(Real*)&kSol) )
    {
        rkSphere.Center() = rkV0 + kSol;
        rkSphere.Radius() = kSol.Length();
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Inscribe (const Vector3<Real>& rkV0, const Vector3<Real>& rkV1,
    const Vector3<Real>& rkV2, Circle3<Real>& rkCircle)
{
    // edges
    Vector3<Real> kE0 = rkV1 - rkV0;
    Vector3<Real> kE1 = rkV2 - rkV1;
    Vector3<Real> kE2 = rkV0 - rkV2;

    // plane normal
    rkCircle.N() = kE1.Cross(kE0);

    // edge normals within the plane
    Vector3<Real> kN0 = rkCircle.N().UnitCross(kE0);
    Vector3<Real> kN1 = rkCircle.N().UnitCross(kE1);
    Vector3<Real> kN2 = rkCircle.N().UnitCross(kE2);

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

    rkCircle.U() = kN0;
    rkCircle.V() = rkCircle.N().Cross(rkCircle.U());

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Inscribe (const Vector3<Real>& rkV0, const Vector3<Real>& rkV1,
    const Vector3<Real>& rkV2, const Vector3<Real>& rkV3,
    Sphere3<Real>& rkSphere)
{
    // edges
    Vector3<Real> kE10 = rkV1 - rkV0;
    Vector3<Real> kE20 = rkV2 - rkV0;
    Vector3<Real> kE30 = rkV3 - rkV0;
    Vector3<Real> kE21 = rkV2 - rkV1;
    Vector3<Real> kE31 = rkV3 - rkV1;

    // normals
    Vector3<Real> kN0 = kE31.Cross(kE21);
    Vector3<Real> kN1 = kE20.Cross(kE30);
    Vector3<Real> kN2 = kE30.Cross(kE10);
    Vector3<Real> kN3 = kE10.Cross(kE20);

    // normalize the normals
    if ( Math<Real>::FAbs(kN0.Normalize()) < Math<Real>::EPSILON )
        return false;
    if ( Math<Real>::FAbs(kN1.Normalize()) < Math<Real>::EPSILON )
        return false;
    if ( Math<Real>::FAbs(kN2.Normalize()) < Math<Real>::EPSILON )
        return false;
    if ( Math<Real>::FAbs(kN3.Normalize()) < Math<Real>::EPSILON )
        return false;

    Real aafA[3][3] =
    {
        {kN1.X()-kN0.X(), kN1.Y()-kN0.Y(), kN1.Z()-kN0.Z()},
        {kN2.X()-kN0.X(), kN2.Y()-kN0.Y(), kN2.Z()-kN0.Z()},
        {kN3.X()-kN0.X(), kN3.Y()-kN0.Y(), kN3.Z()-kN0.Z()}
    };

    Real afB[3] =
    {
        (Real)0.0,
        (Real)0.0,
        -kN3.Dot(kE30)
    };

    Vector3<Real> kSol;
    if ( LinearSystem<Real>::Solve3(aafA,afB,(Real*)&kSol) )
    {
        rkSphere.Center() = rkV3 + kSol;
        rkSphere.Radius() = Math<Real>::FAbs(kN0.Dot(kSol));
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool Circumscribe<float> (const Vector3<float>&,
    const Vector3<float>&, const Vector3<float>&, Circle3<float>&);
template WML_ITEM bool Circumscribe<float> (const Vector3<float>&,
    const Vector3<float>&, const Vector3<float>&, const Vector3<float>&,
    Sphere3<float>&);
template WML_ITEM bool Inscribe<float> (const Vector3<float>&,
    const Vector3<float>&, const Vector3<float>&, Circle3<float>&);
template WML_ITEM bool Inscribe<float> (const Vector3<float>&,
    const Vector3<float>&, const Vector3<float>&, const Vector3<float>&,
    Sphere3<float>&);

template WML_ITEM bool Circumscribe<double> (const Vector3<double>&,
    const Vector3<double>&, const Vector3<double>&, Circle3<double>&);
template WML_ITEM bool Circumscribe<double> (const Vector3<double>&,
    const Vector3<double>&, const Vector3<double>&, const Vector3<double>&,
    Sphere3<double>&);
template WML_ITEM bool Inscribe<double> (const Vector3<double>&,
    const Vector3<double>&, const Vector3<double>&, Circle3<double>&);
template WML_ITEM bool Inscribe<double> (const Vector3<double>&,
    const Vector3<double>&, const Vector3<double>&, const Vector3<double>&,
    Sphere3<double>&);
}
//----------------------------------------------------------------------------
