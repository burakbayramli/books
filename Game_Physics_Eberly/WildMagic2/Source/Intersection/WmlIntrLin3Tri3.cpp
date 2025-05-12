// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Tri3.h"
#include "WmlDistLin3Tri3.h"
#include "WmlIntrUtilityLin3.h"
#include "WmlIntrUtilityTri3.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary objects
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Triangle3<Real>& rkTriangle)
{
    return SqrDistance(rkSegment,rkTriangle) <= Math<Real>::EPSILON;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray3<Real>& rkRay,
    const Triangle3<Real>& rkTriangle)
{
    return SqrDistance(rkRay,rkTriangle) <= Math<Real>::EPSILON;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line3<Real>& rkLine,
    const Triangle3<Real>& rkTriangle)
{
    return SqrDistance(rkLine,rkTriangle) <= Math<Real>::EPSILON;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Triangle3<Real>& rkTriangle, Vector3<Real>& rkPoint)
{
    Real fSegP;
    if ( SqrDistance(rkSegment,rkTriangle,&fSegP) <= Math<Real>::EPSILON )
    {
        rkPoint = rkSegment.Origin() + fSegP*rkSegment.Direction();
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray3<Real>& rkRay,
    const Triangle3<Real>& rkTriangle, Vector3<Real>& rkPoint)
{
    Real fRayP;
    if ( SqrDistance(rkRay,rkTriangle,&fRayP) <= Math<Real>::EPSILON )
    {
        rkPoint = rkRay.Origin() + fRayP*rkRay.Direction();
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line3<Real>& rkLine,
    const Triangle3<Real>& rkTriangle, Vector3<Real>& rkPoint)
{
    Real fLinP;
    if ( SqrDistance(rkLine,rkTriangle,&fLinP) <= Math<Real>::EPSILON )
    {
        rkPoint = rkLine.Origin() + fLinP*rkLine.Direction();
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
// moving objects
//----------------------------------------------------------------------------
template <class Real>
static bool SegTriAxisTest (const Vector3<Real>& rkVel,
    const Vector3<Real>& rkAxis, const Vector3<Real> akU[2],
    const Vector3<Real> akV[3], Real& rfTFirst, Real& rfTLast,Real fTMax)
{
    Real fUMin, fUMax;
    LineProjection(rkAxis,akU,fUMin,fUMax);

    Real fVMin, fVMax;
    TriProjection(rkAxis,akV,fVMin,fVMax);

    return AxisTest(rkVel,rkAxis,fUMin,fUMax,fVMin,fVMax,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static bool SegTriAxisFind (const Vector3<Real>& rkVel, 
    const Vector3<Real>& rkAxis, const Vector3<Real> akU[2], 
    const Vector3<Real> akV[3], Real& rfTFirst, Real& rfTLast, Real fTMax,
    ContactSide& reSide, ContactConfig<Real>& rkTUC,
    ContactConfig<Real>& rkTVC)
{
	ContactConfig<Real> kUC;
    GetLineConfiguration(rkAxis,akU,kUC);

	ContactConfig<Real> kVC;
    GetTriConfiguration(rkAxis,akV,kVC);

    return AxisFind(rkVel,rkAxis,kUC,kVC,reSide,rkTUC,rkTVC,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSet (const Vector3<Real> akU[2],
    const Vector3<Real> akV[3], ContactSide eSide,
    const ContactConfig<Real>& rkUC, const ContactConfig<Real>& rkVC,
    const Vector3<Real>& rkVelU, const Vector3<Real>& rkVelV, Real fTime,
    int& riQuantity, Vector3<Real>* akP)
{
    Vector3<Real> akUFinal[2] =
    {
        akU[0] + fTime*rkVelU, 
        akU[1] + fTime*rkVelU
    };

    Vector3<Real> akVFinal[3] =
    {
        akV[0] + fTime*rkVelV,
        akV[1] + fTime*rkVelV,
        akV[2] + fTime*rkVelV
    };

    if ( eSide == LEFT ) // V on left of U
    {
        if ( rkUC.m_kMap == m11 )
        {
            riQuantity = 1;
            akP[0] = akUFinal[rkUC.m_aiIndex[0]];
        }
        else if ( ( rkVC.m_kMap == m111 ) || ( rkVC.m_kMap == m21 ) )
        {
            riQuantity = 1;
            akP[0] = akVFinal[rkVC.m_aiIndex[2]];
        }
        else if ( rkVC.m_kMap == m12 )
        {
            Vector3<Real> kTemp[2];
            kTemp[0] = akVFinal[rkVC.m_aiIndex[1]];
            kTemp[1] = akVFinal[rkVC.m_aiIndex[2]];
            FindContactSetLinLin(akUFinal,kTemp,riQuantity,akP);
        }
        else // U is m2, V is m3
        {
            FindContactSetColinearLineTri(akUFinal,akV,riQuantity,akP);
        }
    }
    else // U on left of V
    {
        if ( rkUC.m_kMap == m11 )
        {
            riQuantity = 1;
            akP[0] = akUFinal[rkUC.m_aiIndex[1]];
        }
        else if ( ( rkVC.m_kMap == m111 ) || ( rkVC.m_kMap == m12 ) )
        {
            riQuantity = 1;
            akP[0] = akVFinal[rkVC.m_aiIndex[0]];
        }
        else if ( rkVC.m_kMap == m21 )
        {
            Vector3<Real> kTemp[2];
            kTemp[0] = akVFinal[rkVC.m_aiIndex[0]];
            kTemp[1] = akVFinal[rkVC.m_aiIndex[1]];
            FindContactSetLinLin(akUFinal,kTemp,riQuantity,akP);
        }
        else // U is m2, V is m3
        {
            FindContactSetColinearLineTri(akUFinal,akVFinal,riQuantity,akP);
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Triangle3<Real>& rkTriangle,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax)
{
    // get end points of segment
    Vector3<Real> akU[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };

    // get vertices of triangle
    Vector3<Real> akV[3] =
    {
        rkTriangle.Origin(),
        rkTriangle.Origin() + rkTriangle.Edge0(),
        rkTriangle.Origin() + rkTriangle.Edge1()
    };

    // get triangle velocity relative to segment
    Vector3<Real> kVel = rkTriVelocity - rkSegVelocity;

    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    // test tri-normal
    Vector3<Real> kNormV = rkTriangle.Edge0().Cross(rkTriangle.Edge1());
    if ( !SegTriAxisTest(kVel,kNormV,akU,akV,rfTFirst,fTLast,fTMax) )
        return false;

    // Test if the segment is parallel to the triangle, effectively the
    // test:  sin(Angle(NormV,DirU)) > 1-epsilon
    Vector3<Real> kDirU = akU[1] - akU[0];
    Vector3<Real> kNormU = kNormV.Cross(kDirU);
    Real fDirUSqrLen = kDirU.SquaredLength();
    Real fNorUSqrLen = kNormU.SquaredLength();
    Real fNorVSqrLen = kNormV.SquaredLength();
    Real fOmEpsilon = (Real)1.0 - Math<Real>::EPSILON;

    int i0, i1;
    Vector3<Real> kAxis;

    if ( fNorUSqrLen > fOmEpsilon*fNorVSqrLen*fDirUSqrLen )  // parallel
    {
        // test tri-normal cross seg-direction
        if ( !SegTriAxisTest(kVel,kNormU,akU,akV,rfTFirst,fTLast,fTMax) )
            return false;

        // test tri-normal cross tri-edges
        for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++)
        {
            kAxis = kNormV.Cross(akV[i1]-akV[i0]);
            if ( !SegTriAxisTest(kVel,kAxis,akU,akV,rfTFirst,fTLast,fTMax) )
                return false;
        }
    }
    else  // not parallel
    {
        // test seg-direction cross tri-edges
        for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++)
        {
            kAxis = kDirU.Cross(akV[i1]-akV[i0]);
            if ( !SegTriAxisTest(kVel,kAxis,akU,akV,rfTFirst,fTLast,fTMax) )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity,  const Triangle3<Real>& rkTriangle,
    const Vector3<Real>& rkTriVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[2])
{
    // get end points of segment
    Vector3<Real> akU[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };

    // get vertices of triangle
    Vector3<Real> akV[3] =
    {
        rkTriangle.Origin(),
        rkTriangle.Origin() + rkTriangle.Edge0(),
        rkTriangle.Origin() + rkTriangle.Edge1()
    };

    // get triangle velocity relative to segment
    Vector3<Real> kVel = rkTriVelocity - rkSegVelocity;

    riQuantity = 0;
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    // test tri-normal
    Vector3<Real> kNormV = rkTriangle.Edge0().Cross(rkTriangle.Edge1());
    ContactConfig<Real> kUC, kVC;
    ContactSide eSide;
    if ( !SegTriAxisFind(kVel,kNormV,akU,akV,rfTFirst,fTLast,fTMax,eSide,
         kUC,kVC) )
    {
        return false;
    }

    // Test if the segment is parallel to the triangle, effectively the
    // test:  sin(Angle(NormV,DirU)) > 1-epsilon
    Vector3<Real> kDirU = akU[1] - akU[0];
    Vector3<Real> kNormU = kNormV.Cross(kDirU);
    Real fDirUSqrLen = kDirU.SquaredLength();
    Real fNorUSqrLen = kNormU.SquaredLength();
    Real fNorVSqrLen = kNormV.SquaredLength();
    Real fOmEpsilon = (Real)1.0 - Math<Real>::EPSILON;

    int i0, i1;
    Vector3<Real> kAxis;

    if ( fNorUSqrLen > fOmEpsilon*fNorVSqrLen*fDirUSqrLen )  // parallel
    {
        // find tri-normal cross seg-direction
        if ( !SegTriAxisFind(kVel,kNormU,akU,akV,rfTFirst,fTLast,fTMax,eSide,
             kUC,kVC) )
        {
            return false;
        }

        // test tri-normal cross tri-edges
        for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++)
        {
            kAxis = kNormV.Cross(akV[i1]-akV[i0]);
            if ( !SegTriAxisFind(kVel,kAxis,akU,akV,rfTFirst,fTLast,fTMax,
                 eSide,kUC,kVC) )
            {
                return false;
            }
        }
    } 
    else 
    {
        // test seg-direction cross tri-edges
        for (i0 = 2, i1 = 0; i1 < 3; i0 = i1++)
        {
            kDirU.Cross(akV[i1]-akV[i0]);
            if ( !SegTriAxisFind(kVel,kAxis,akU,akV,rfTFirst,fTLast,fTMax,
                 eSide,kUC,kVC) )
            {
                return false;
            }
        }
    }

    if ( rfTFirst < (Real)0.0 )
        return false;

    FindContactSet(akU,akV,eSide,kUC,kVC,rkSegVelocity,rkTriVelocity,
        rfTFirst,riQuantity,akP);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Triangle3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Ray3<float>&, const Triangle3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Line3<float>&, const Triangle3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Triangle3<float>&, Vector3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Ray3<float>&, const Triangle3<float>&, Vector3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Line3<float>&, const Triangle3<float>&, Vector3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Vector3<float>&,
    const Triangle3<float>&, const Vector3<float>&, float&, float);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Vector3<float>&,
    const Triangle3<float>&, const Vector3<float>&, float&, float, int&,
    Vector3<float>[2]);

template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Triangle3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Ray3<double>&, const Triangle3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Line3<double>&, const Triangle3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Triangle3<double>&, Vector3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Ray3<double>&, const Triangle3<double>&, Vector3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Line3<double>&, const Triangle3<double>&, Vector3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Vector3<double>&,
    const Triangle3<double>&, const Vector3<double>&, double&, double);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Vector3<double>&,
    const Triangle3<double>&, const Vector3<double>&, double&, double, int&,
    Vector3<double>[2]);
}
//----------------------------------------------------------------------------
