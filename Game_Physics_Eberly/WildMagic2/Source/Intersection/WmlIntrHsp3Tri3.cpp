// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrHsp3Tri3.h"
#include "WmlIntrUtilityTri3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Triangle3<Real>& rkTri)
{
    Real fMin, fMax;
    Vector3<Real> akTri[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };
    TriProjection(rkHalfSpace.GetNormal(),akTri,fMin,fMax);
    return ( fMin <= rkHalfSpace.GetConstant() );
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Triangle3<Real>& rkTri, int& riQuantity, Vector3<Real> akP[3])
{
    riQuantity = 3;
    akP[0] = rkTri.Origin();
    akP[1] = rkTri.Origin() + rkTri.Edge0();
    akP[2] = rkTri.Origin() + rkTri.Edge1();

    ClipConvexPolygonAgainstPlane<Real>(-rkHalfSpace.GetNormal(),
        -rkHalfSpace.GetConstant(),riQuantity,akP);
    
    return riQuantity > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax)
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkTriVelocity - rkHspVelocity;

    Real fMin, fMax;
    Vector3<Real> akTri[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };
    TriProjection(rkHalfSpace.GetNormal(),akTri,fMin,fMax);

    return AxisTest(kVel,rkHalfSpace.GetNormal(),-Math<Real>::MAX_REAL,
        rkHalfSpace.GetConstant(),fMin,fMax,rfTFirst,fTLast,fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity,  Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[3])
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkTriVelocity - rkHspVelocity;

    Vector3<Real> akTri[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };
    ContactConfig<Real> kContact;
    GetTriConfiguration(rkHalfSpace.GetNormal(),akTri,kContact);

    if ( !AxisTest(kVel,rkHalfSpace.GetNormal(),-Math<Real>::MAX_REAL,
        rkHalfSpace.GetConstant(),kContact.m_fMin,kContact.m_fMax,rfTFirst,
        fTLast,fTMax) )
    {
        // never intersecting
        return false;
    }

    if ( rfTFirst == (Real)0.0 )
    {
        // intersecting now
        return false;
    }

    // tri on positive side (right)
    if ( kContact.m_kMap == m12 || kContact.m_kMap == m111 )
    {
        // point
        riQuantity = 1;
        akP[0] = akTri[kContact.m_aiIndex[0]];
    }
    else if ( kContact.m_kMap == m21 )
    {
        // segment
        riQuantity = 2;
        akP[0] = akTri[kContact.m_aiIndex[0]];
        akP[1] = akTri[kContact.m_aiIndex[1]];
    }
    else
    {
        // face
        riQuantity = 3;
        memcpy(akP,akTri,3*sizeof(Vector3<Real>));
    } 

    // adjust points to the correct place in time, as well
    Vector3<Real> kDistance = rfTFirst*rkTriVelocity;
    for (int i = 0; i < riQuantity; i++)
        akP[i] += kDistance;

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const HalfSpace3<float>&,
    const Triangle3<float>&);
template WML_ITEM bool FindIntersection<float> (const HalfSpace3<float>&,
    const Triangle3<float>&, int&, Vector3<float>[3]);
template WML_ITEM bool TestIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Triangle3<float>&, const Vector3<float>&,
    float&, float);
template WML_ITEM bool FindIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Triangle3<float>&, const Vector3<float>&,
    float&, float, int&, Vector3<float>[3]);

template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Triangle3<double>&);
template WML_ITEM bool FindIntersection<double> (const HalfSpace3<double>&,
    const Triangle3<double>&, int&, Vector3<double>[3]);
template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Triangle3<double>&, const Vector3<double>&,
    double&, double);
template WML_ITEM bool FindIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Triangle3<double>&, const Vector3<double>&,
    double&, double, int&, Vector3<double>[3]);
}
//----------------------------------------------------------------------------
