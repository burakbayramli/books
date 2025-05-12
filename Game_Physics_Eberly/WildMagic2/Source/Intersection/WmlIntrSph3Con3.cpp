// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrSph3Con3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Sphere3<Real>& rkSphere,
    const Cone3<Real>& rkCone)
{
    // TO DO.  Add these as members of Cone3 to avoid recomputing on each
    // intersection call.
    Real fInvSin = ((Real)1.0)/rkCone.SinAngle();
    Real fCosSqr = rkCone.CosAngle()*rkCone.CosAngle();

    Vector3<Real> kCmV = rkSphere.Center() - rkCone.Vertex();
    Vector3<Real> kD = kCmV + (rkSphere.Radius()*fInvSin)*rkCone.Axis();
    Real fDSqrLen = kD.SquaredLength();
    Real fE = kD.Dot(rkCone.Axis());
    if ( fE > (Real)0.0 && fE*fE >= fDSqrLen*fCosSqr )
    {
        // TO DO.  Add this as member of Cone3 to avoid recomputing on each
        // intersection call.
        Real fSinSqr = rkCone.SinAngle()*rkCone.SinAngle();

        fDSqrLen = kCmV.SquaredLength();
        fE = -kCmV.Dot(rkCone.Axis());
        if ( fE > (Real)0.0 && fE*fE >= fDSqrLen*fSinSqr )
        {
            // TO DO.  Add this as member of Sphere to avoid recomputing on
            // each intersection call.  (Useful for other functions using
            // squared radius.)
            Real fRSqr = rkSphere.Radius()*rkSphere.Radius();
            return fDSqrLen <= fRSqr;
        }
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Sphere3<Real>& rkSphere,
    const Cone3<Real>& rkCone, Vector3<Real>& rkClosest)
{
    // test if cone vertex is in sphere
    Vector3<Real> kDiff = rkSphere.Center() - rkCone.Vertex();
    Real fRSqr = rkSphere.Radius()*rkSphere.Radius();
    Real fLSqr = kDiff.SquaredLength();
    if ( fLSqr <= fRSqr)
        return true;

    // test if sphere center is in cone
    Real fDot = kDiff.Dot(rkCone.Axis());
    Real fDotSqr = fDot*fDot;
    Real fCosSqr = rkCone.CosAngle()*rkCone.CosAngle();
    if ( fDotSqr >= fLSqr*fCosSqr && fDot > (Real)0.0 )
    {
        // sphere center is inside cone, so sphere and cone intersect
        return true;
    }

    // Sphere center is outside cone.  Problem now reduces to looking for
    // an intersection between circle and ray in the plane containing
    // cone vertex and spanned by cone axis and vector from vertex to
    // sphere center.

    // Ray is t*D+V (t >= 0) where |D| = 1 and dot(A,D) = cos(angle).
    // Also, D = e*A+f*(C-V).  Plugging ray equation into sphere equation
    // yields R^2 = |t*D+V-C|^2, so the quadratic for intersections is
    // t^2 - 2*dot(D,C-V)*t + |C-V|^2 - R^2 = 0.  An intersection occurs
    // if and only if the discriminant is nonnegative.  This test becomes
    //
    //     dot(D,C-V)^2 >= dot(C-V,C-V) - R^2
    //
    // Note that if the right-hand side is nonpositive, then the inequality
    // is true (the sphere contains V).  I have already ruled this out in
    // the first block of code in this function.

    Real fULen = Math<Real>::Sqrt(Math<Real>::FAbs(fLSqr-fDotSqr));
    Real fTest = rkCone.CosAngle()*fDot + rkCone.SinAngle()*fULen;
    Real fDiscr = fTest*fTest - fLSqr + fRSqr;

    // compute point of intersection closest to vertex V
    Real fT = fTest - Math<Real>::Sqrt(fDiscr);
    Vector3<Real> kB = kDiff - fDot*rkCone.Axis();
    Real fTmp = rkCone.SinAngle()/fULen;
    rkClosest = fT*(rkCone.CosAngle()*rkCone.Axis() + fTmp*kB);

    return fDiscr >= (Real)0.0 && fTest >= (Real)0.0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Sphere3<float>&,
    const Cone3<float>&);
template WML_ITEM bool FindIntersection<float> (const Sphere3<float>&,
    const Cone3<float>&, Vector3<float>&);

template WML_ITEM bool TestIntersection<double> (const Sphere3<double>&,
    const Cone3<double>&);
template WML_ITEM bool FindIntersection<double> (const Sphere3<double>&,
    const Cone3<double>&, Vector3<double>&);
}
//----------------------------------------------------------------------------
