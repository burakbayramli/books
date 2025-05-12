// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Tor3.h"
#include "WmlPolynomial1.h"
#include "WmlPolynomialRoots.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray3<Real>& rkRay,
    const Torus3<Real>& rkTorus, Real& rfS, Real& rfT)
{
    // compute coefficients of quartic polynomial
    Real fRo2 = rkTorus.Ro()*rkTorus.Ro();
    Real fRi2 = rkTorus.Ri()*rkTorus.Ri();
    const Vector3<Real>& rkEye = rkRay.Origin();
    const Vector3<Real>& rkDir = rkRay.Direction();
    Real fDD = rkDir.Dot(rkDir);
    Real fDE = rkEye.Dot(rkDir);
    Real fVal = rkEye.Dot(rkEye) - (fRo2 + fRi2);

    Polynomial1<Real> kPoly(5);
    kPoly[0] = fVal*fVal - ((Real)4.0)*fRo2*(fRi2 - rkEye.Z()*rkEye.Z());
    kPoly[1] = ((Real)4.0)*fDE*fVal + ((Real)8.0)*fRo2*rkDir.Z()*rkEye.Z();
    kPoly[2] = ((Real)2.0)*fDD*fVal + ((Real)4.0)*fDE*fDE +
        ((Real)4.0)*fRo2*rkDir.Z()*rkDir.Z();
    kPoly[3] = ((Real)4.0)*fDD*fDE;
    kPoly[4] = fDD*fDD;

    // solve the quartic
    PolynomialRoots<Real> kPR(Math<Real>::EPSILON);
    kPR.FindB(kPoly,6);
    int iCount = kPR.GetCount();
    const Real* afRoot = kPR.GetRoots();

    // search for closest point
    Real fPMin = Math<Real>::MAX_REAL;
    rfS = (Real)0.0;
    rfT = (Real)0.0;
    for (int i = 0; i < iCount; i++)
    {
        if ( 0 <= afRoot[i] && afRoot[i] < fPMin )
        {
            fPMin = afRoot[i];
            Vector3<Real> kPos = rkEye + fPMin*rkDir;
            rkTorus.GetParameters(kPos,rfS,rfT);
        }
    }

    return fPMin != Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool FindIntersection<float> (const Ray3<float>&,
     const Torus3<float>&, float&, float&);

template WML_ITEM bool FindIntersection<double> (const Ray3<double>&,
     const Torus3<double>&, double&, double&);
}
//----------------------------------------------------------------------------
