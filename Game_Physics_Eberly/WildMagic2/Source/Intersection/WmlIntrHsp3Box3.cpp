// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrHsp3Box3.h"
#include "WmlIntrUtilityBox3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static Real DistanceHalfSpaceBox (const HalfSpace3<Real>& rkHalfSpace,
    const Box3<Real>& rkBox)
{
    Real fMin, fMax;
    BoxProjection(rkHalfSpace.GetNormal(),rkBox,fMin,fMax);
    return fMin - rkHalfSpace.GetConstant();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Box3<Real>& rkBox)
{
    Real fMin, fMax;
    BoxProjection(rkHalfSpace.GetNormal(),rkBox,fMin,fMax);
    return fMin <= rkHalfSpace.GetConstant();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax)
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkBoxVelocity - rkHspVelocity;

    Real fMin, fMax;
    BoxProjection(rkHalfSpace.GetNormal(),rkBox,fMin,fMax);

    return AxisTest(kVel,rkHalfSpace.GetNormal(),-Math<Real>::MAX_REAL,
        rkHalfSpace.GetConstant(),fMin,fMax,rfTFirst,fTLast,fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[4])
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkBoxVelocity - rkHspVelocity;

    ContactConfig<Real> kContact;
    GetBoxConfiguration(rkHalfSpace.GetNormal(),rkBox,kContact);

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

    // box on positive side (right)
    if ( kContact.m_kMap == m1_1 )
    {
        // point intersection
        riQuantity = 1;
        akP[0] = GetPoint(kContact.m_aiIndex[0],rkBox);
    }
    else if ( kContact.m_kMap == m2_2 )
    {
        // segment intersection
        riQuantity = 2;
        akP[0] = GetPoint(kContact.m_aiIndex[0],rkBox);
        akP[1] = GetPoint(kContact.m_aiIndex[1],rkBox);
    }
    else // ( kContact.m_kMap == m44 )
    {
        // face intersection
        riQuantity = 4;
        akP[0] = GetPoint(kContact.m_aiIndex[0],rkBox);
        akP[1] = GetPoint(kContact.m_aiIndex[1],rkBox);
        akP[2] = GetPoint(kContact.m_aiIndex[2],rkBox);
        akP[3] = GetPoint(kContact.m_aiIndex[3],rkBox);
    }

    // adjust points to the correct place in time, as well
    Vector3<Real> kDistance = rfTFirst*rkBoxVelocity;
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
    const Box3<float>&);
template WML_ITEM bool TestIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Box3<float>&, const Vector3<float>&,
    float&, float);
template WML_ITEM bool FindIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Box3<float>&, const Vector3<float>&,
    float&, float, int&, Vector3<float>[4]);

template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Box3<double>&);
template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Box3<double>&, const Vector3<double>&,
    double&, double);
template WML_ITEM bool FindIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Box3<double>&, const Vector3<double>&,
    double&, double, int&, Vector3<double>[4]);
}
//----------------------------------------------------------------------------
