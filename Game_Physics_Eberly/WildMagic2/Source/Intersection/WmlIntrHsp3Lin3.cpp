// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrHsp3Lin3.h"
#include "WmlIntrUtilityLin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Segment3<Real>& rkSegment)
{
    Real fMin, fMax;
    Vector3<Real> akSeg[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };
    LineProjection(rkHalfSpace.GetNormal(),akSeg,fMin,fMax);
    return fMin <= rkHalfSpace.GetConstant();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Segment3<Real>& rkSegment, int& riQuantity, Vector3<Real> akP[2])
{
    riQuantity = 2;
    akP[0] = rkSegment.Origin();
    akP[0] = rkSegment.Origin() + rkSegment.Direction();

    ClipConvexPolygonAgainstPlane<Real>(-rkHalfSpace.GetNormal(),
        -rkHalfSpace.GetConstant(),riQuantity,akP);
    
    return riQuantity > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, Real& rfTFirst, Real fTMax)
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkSegVelocity - rkHspVelocity;

    Real fMin, fMax;
    Vector3<Real> akSeg[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };
    LineProjection(rkHalfSpace.GetNormal(),akSeg,fMin,fMax);

    return AxisTest(kVel,rkHalfSpace.GetNormal(),-Math<Real>::MAX_REAL,
        rkHalfSpace.GetConstant(),fMin,fMax,rfTFirst,fTLast,fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const HalfSpace3<Real>& rkHalfSpace,
    const Vector3<Real>& rkHspVelocity, const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[2])
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;
    Vector3<Real> kVel = rkSegVelocity - rkHspVelocity;

    Vector3<Real> akSeg[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };
    ContactConfig<Real> kContact;
    GetLineConfiguration(rkHalfSpace.GetNormal(),akSeg,kContact);

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

    // line on positive side (right)
    if ( kContact.m_kMap == m11 )
    {
        riQuantity = 1;
        akP[0] = akSeg[kContact.m_aiIndex[0]];
    }
    else // kContact.m_kMap == m2
    {
        riQuantity = 2;
        akP[0] = akSeg[kContact.m_aiIndex[0]];
        akP[1] = akSeg[kContact.m_aiIndex[1]];
    }

    // adjust points to the correct place in time, as well
    Vector3<Real> kDistance = rfTFirst*rkSegVelocity;
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
    const Segment3<float>&);
template WML_ITEM bool FindIntersection<float> (const HalfSpace3<float>&,
    const Segment3<float>&, int&, Vector3<float>[2]);
template WML_ITEM bool TestIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Segment3<float>&, const Vector3<float>&,
    float&, float);
template WML_ITEM bool FindIntersection<float> (const HalfSpace3<float>&,
    const Vector3<float>&, const Segment3<float>&, const Vector3<float>&,
    float&, float, int&, Vector3<float>[2]);

template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Segment3<double>&);
template WML_ITEM bool FindIntersection<double> (const HalfSpace3<double>&,
    const Segment3<double>&, int&, Vector3<double>[2]);
template WML_ITEM bool TestIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Segment3<double>&, const Vector3<double>&,
    double&, double);
template WML_ITEM bool FindIntersection<double> (const HalfSpace3<double>&,
    const Vector3<double>&, const Segment3<double>&, const Vector3<double>&,
    double&, double, int&, Vector3<double>[2]);
}
//----------------------------------------------------------------------------
