// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Box3.h"
#include "WmlIntrUtilityLin3.h"
#include "WmlIntrUtilityBox3.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary objects
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Box3<Real>& rkBox)
{
    Real fAWdU[3], fADdU[3], fAWxDdU[3], fRhs;
    Vector3<Real> kSDir = ((Real)0.5)*rkSegment.Direction();
    Vector3<Real> kSCen = rkSegment.Origin() + kSDir;

    Vector3<Real> kDiff = kSCen - rkBox.Center();

    fAWdU[0] = Math<Real>::FAbs(kSDir.Dot(rkBox.Axis(0)));
    fADdU[0] = Math<Real>::FAbs(kDiff.Dot(rkBox.Axis(0)));
    fRhs = rkBox.Extent(0) + fAWdU[0];
    if ( fADdU[0] > fRhs )
        return false;

    fAWdU[1] = Math<Real>::FAbs(kSDir.Dot(rkBox.Axis(1)));
    fADdU[1] = Math<Real>::FAbs(kDiff.Dot(rkBox.Axis(1)));
    fRhs = rkBox.Extent(1) + fAWdU[1];
    if ( fADdU[1] > fRhs )
        return false;

    fAWdU[2] = Math<Real>::FAbs(kSDir.Dot(rkBox.Axis(2)));
    fADdU[2] = Math<Real>::FAbs(kDiff.Dot(rkBox.Axis(2)));
    fRhs = rkBox.Extent(2) + fAWdU[2];
    if ( fADdU[2] > fRhs )
        return false;

    Vector3<Real> kWxD = kSDir.Cross(kDiff);

    fAWxDdU[0] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(0)));
    fRhs = rkBox.Extent(1)*fAWdU[2] + rkBox.Extent(2)*fAWdU[1];
    if ( fAWxDdU[0] > fRhs )
        return false;

    fAWxDdU[1] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(1)));
    fRhs = rkBox.Extent(0)*fAWdU[2] + rkBox.Extent(2)*fAWdU[0];
    if ( fAWxDdU[1] > fRhs )
        return false;

    fAWxDdU[2] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(2)));
    fRhs = rkBox.Extent(0)*fAWdU[1] + rkBox.Extent(1)*fAWdU[0];
    if ( fAWxDdU[2] > fRhs )
        return false;

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray3<Real>& rkRay, const Box3<Real>& rkBox)
{
    Real fWdU[3], fAWdU[3], fDdU[3], fADdU[3], fAWxDdU[3], fRhs;

    Vector3<Real> kDiff = rkRay.Origin() - rkBox.Center();

    fWdU[0] = rkRay.Direction().Dot(rkBox.Axis(0));
    fAWdU[0] = Math<Real>::FAbs(fWdU[0]);
    fDdU[0] = kDiff.Dot(rkBox.Axis(0));
    fADdU[0] = Math<Real>::FAbs(fDdU[0]);
    if ( fADdU[0] > rkBox.Extent(0) && fDdU[0]*fWdU[0] >= (Real)0.0 )
        return false;

    fWdU[1] = rkRay.Direction().Dot(rkBox.Axis(1));
    fAWdU[1] = Math<Real>::FAbs(fWdU[1]);
    fDdU[1] = kDiff.Dot(rkBox.Axis(1));
    fADdU[1] = Math<Real>::FAbs(fDdU[1]);
    if ( fADdU[1] > rkBox.Extent(1) && fDdU[1]*fWdU[1] >= (Real)0.0 )
        return false;

    fWdU[2] = rkRay.Direction().Dot(rkBox.Axis(2));
    fAWdU[2] = Math<Real>::FAbs(fWdU[2]);
    fDdU[2] = kDiff.Dot(rkBox.Axis(2));
    fADdU[2] = Math<Real>::FAbs(fDdU[2]);
    if ( fADdU[2] > rkBox.Extent(2) && fDdU[2]*fWdU[2] >= (Real)0.0 )
        return false;

    Vector3<Real> kWxD = rkRay.Direction().Cross(kDiff);

    fAWxDdU[0] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(0)));
    fRhs = rkBox.Extent(1)*fAWdU[2] + rkBox.Extent(2)*fAWdU[1];
    if ( fAWxDdU[0] > fRhs )
        return false;

    fAWxDdU[1] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(1)));
    fRhs = rkBox.Extent(0)*fAWdU[2] + rkBox.Extent(2)*fAWdU[0];
    if ( fAWxDdU[1] > fRhs )
        return false;

    fAWxDdU[2] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(2)));
    fRhs = rkBox.Extent(0)*fAWdU[1] + rkBox.Extent(1)*fAWdU[0];
    if ( fAWxDdU[2] > fRhs )
        return false;

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line3<Real>& rkLine,
    const Box3<Real>& rkBox)
{
    Real fAWdU[3], fAWxDdU[3], fRhs;

    Vector3<Real> kDiff = rkLine.Origin() - rkBox.Center();
    Vector3<Real> kWxD = rkLine.Direction().Cross(kDiff);

    fAWdU[1] = Math<Real>::FAbs(rkLine.Direction().Dot(rkBox.Axis(1)));
    fAWdU[2] = Math<Real>::FAbs(rkLine.Direction().Dot(rkBox.Axis(2)));
    fAWxDdU[0] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(0)));
    fRhs = rkBox.Extent(1)*fAWdU[2] + rkBox.Extent(2)*fAWdU[1];
    if ( fAWxDdU[0] > fRhs )
        return false;

    fAWdU[0] = Math<Real>::FAbs(rkLine.Direction().Dot(rkBox.Axis(0)));
    fAWxDdU[1] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(1)));
    fRhs = rkBox.Extent(0)*fAWdU[2] + rkBox.Extent(2)*fAWdU[0];
    if ( fAWxDdU[1] > fRhs )
        return false;

    fAWxDdU[2] = Math<Real>::FAbs(kWxD.Dot(rkBox.Axis(2)));
    fRhs = rkBox.Extent(0)*fAWdU[1] + rkBox.Extent(1)*fAWdU[0];
    if ( fAWxDdU[2] > fRhs )
        return false;

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
static bool Clip (Real fDenom, Real fNumer, Real& rfT0, Real& rfT1)
{
    // Return value is 'true' if line segment intersects the current test
    // plane.  Otherwise 'false' is returned in which case the line segment
    // is entirely clipped.

    if ( fDenom > (Real)0.0 )
    {
        if ( fNumer > fDenom*rfT1 )
            return false;
        if ( fNumer > fDenom*rfT0 )
            rfT0 = fNumer/fDenom;
        return true;
    }
    else if ( fDenom < (Real)0.0 )
    {
        if ( fNumer > fDenom*rfT0 )
            return false;
        if ( fNumer > fDenom*rfT1 )
            rfT1 = fNumer/fDenom;
        return true;
    }
    else
    {
        return fNumer <= (Real)0.0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Vector3<Real>& rkOrigin,
    const Vector3<Real>& rkDirection, const Real afExtent[3], Real& rfT0,
    Real& rfT1)
{
    Real fSaveT0 = rfT0, fSaveT1 = rfT1;

    bool bNotEntirelyClipped =
        Clip(+rkDirection.X(),-rkOrigin.X()-afExtent[0],rfT0,rfT1) &&
        Clip(-rkDirection.X(),+rkOrigin.X()-afExtent[0],rfT0,rfT1) &&
        Clip(+rkDirection.Y(),-rkOrigin.Y()-afExtent[1],rfT0,rfT1) &&
        Clip(-rkDirection.Y(),+rkOrigin.Y()-afExtent[1],rfT0,rfT1) &&
        Clip(+rkDirection.Z(),-rkOrigin.Z()-afExtent[2],rfT0,rfT1) &&
        Clip(-rkDirection.Z(),+rkOrigin.Z()-afExtent[2],rfT0,rfT1);

    return bNotEntirelyClipped && ( rfT0 != fSaveT0 || rfT1 != fSaveT1 );
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akPoint[2])
{
    // convert segment to box coordinates
    Vector3<Real> kDiff = rkSegment.Origin() - rkBox.Center();
    Vector3<Real> kOrigin(
        kDiff.Dot(rkBox.Axis(0)),
        kDiff.Dot(rkBox.Axis(1)),
        kDiff.Dot(rkBox.Axis(2))
    );
    Vector3<Real> kDirection(
        rkSegment.Direction().Dot(rkBox.Axis(0)),
        rkSegment.Direction().Dot(rkBox.Axis(1)),
        rkSegment.Direction().Dot(rkBox.Axis(2))
    );

    Real fT0 = (Real)0.0, fT1 = (Real)1.0;
    bool bIntersects = FindIntersection(kOrigin,kDirection,rkBox.Extents(),
        fT0,fT1);

    if ( bIntersects )
    {
        if ( fT0 > (Real)0.0 )
        {
            if ( fT1 < (Real)1.0 )
            {
                riQuantity = 2;
                akPoint[0] = rkSegment.Origin() + fT0*rkSegment.Direction();
                akPoint[1] = rkSegment.Origin() + fT1*rkSegment.Direction();
            }
            else
            {
                riQuantity = 1;
                akPoint[0] = rkSegment.Origin() + fT0*rkSegment.Direction();
            }
        }
        else  // fT0 == 0
        {
            if ( fT1 < (Real)1.0 )
            {
                riQuantity = 1;
                akPoint[0] = rkSegment.Origin() + fT1*rkSegment.Direction();
            }
            else  // fT1 == 1
            {
                // segment entirely in box
                riQuantity = 0;
            }
        }
    }
    else
    {
        riQuantity = 0;
    }

    return bIntersects;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray3<Real>& rkRay, const Box3<Real>& rkBox,
    int& riQuantity, Vector3<Real> akPoint[2])
{
    // convert ray to box coordinates
    Vector3<Real> kDiff = rkRay.Origin() - rkBox.Center();
    Vector3<Real> kOrigin(
        kDiff.Dot(rkBox.Axis(0)),
        kDiff.Dot(rkBox.Axis(1)),
        kDiff.Dot(rkBox.Axis(2))
    );
    Vector3<Real> kDirection(
        rkRay.Direction().Dot(rkBox.Axis(0)),
        rkRay.Direction().Dot(rkBox.Axis(1)),
        rkRay.Direction().Dot(rkBox.Axis(2))
    );

    Real fT0 = (Real)0.0, fT1 = Math<Real>::MAX_REAL;
    bool bIntersects = FindIntersection(kOrigin,kDirection,rkBox.Extents(),
        fT0,fT1);

    if ( bIntersects )
    {
        if ( fT0 > (Real)0.0 )
        {
            riQuantity = 2;
            akPoint[0] = rkRay.Origin() + fT0*rkRay.Direction();
            akPoint[1] = rkRay.Origin() + fT1*rkRay.Direction();
        }
        else  // fT0 == 0
        {
            riQuantity = 1;
            akPoint[0] = rkRay.Origin() + fT1*rkRay.Direction();
        }
    }
    else
    {
        riQuantity = 0;
    }

    return bIntersects;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line3<Real>& rkLine,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akPoint[2])
{
    // convert line to box coordinates
    Vector3<Real> kDiff = rkLine.Origin() - rkBox.Center();
    Vector3<Real> kOrigin(
        kDiff.Dot(rkBox.Axis(0)),
        kDiff.Dot(rkBox.Axis(1)),
        kDiff.Dot(rkBox.Axis(2))
    );
    Vector3<Real> kDirection(
        rkLine.Direction().Dot(rkBox.Axis(0)),
        rkLine.Direction().Dot(rkBox.Axis(1)),
        rkLine.Direction().Dot(rkBox.Axis(2))
    );

    Real fT0 = -Math<Real>::MAX_REAL, fT1 = Math<Real>::MAX_REAL;
    bool bIntersects = FindIntersection(kOrigin,kDirection,rkBox.Extents(),
        fT0,fT1);

    if ( bIntersects )
    {
        if ( fT0 != fT1 )
        {
            riQuantity = 2;
            akPoint[0] = rkLine.Origin() + fT0*rkLine.Direction();
            akPoint[1] = rkLine.Origin() + fT1*rkLine.Direction();
        }
        else
        {
            riQuantity = 1;
            akPoint[0] = rkLine.Origin() + fT0*rkLine.Direction();
        }
    }
    else
    {
        riQuantity = 0;
    }

    return bIntersects;
}
//----------------------------------------------------------------------------
// moving objects
//----------------------------------------------------------------------------
template <class Real>
static bool SegBoxAxisTest (const Vector3<Real>& rkAxis,
    const Vector3<Real> akU[2], const Vector3<Real>& rkVel,
    const Box3<Real>& rkBox, Real& rfTFirst, Real& rfTLast, Real fTMax)
{
    Real fMin0, fMax0;
    LineProjection(rkAxis,akU,fMin0,fMax0);

    Real fMin1, fMax1;
    BoxProjection(rkAxis,rkBox,fMin1,fMax1);
    
    return AxisTest(rkVel,rkAxis,fMin0,fMax0,fMin1,fMax1,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static bool SegBoxAxisFind (const Vector3<Real>& rkAxis,  
    const Vector3<Real>& rkVel, const Vector3<Real> akU[2],
    const Box3<Real>& rkBox, Real& rfTFirst, Real& rfTLast, Real fTMax,
    ContactSide& reSide, ContactConfig<Real>& rkTUC,
    ContactConfig<Real>& rkTVC)
{
    ContactConfig<Real> kUC;
    GetLineConfiguration(rkAxis,akU,kUC);

    ContactConfig<Real> kVC;
    GetBoxConfiguration(rkAxis,rkBox,kVC);

    return AxisFind(rkVel,rkAxis,kUC,kVC,reSide,rkTUC,rkTVC,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSet (const Vector3<Real> akU[2],
    const Box3<Real>& rkBox, ContactSide eSide, 
    const ContactConfig<Real>& rkSegContact,
    const ContactConfig<Real>& rkBoxContact,
    const Vector3<Real>& rkSegVelocity, const Vector3<Real>& rkBoxVelocity,
    Real fTFirst, int& riQuantity, Vector3<Real>* akP)
{
    // move segment to new position
    Vector3<Real> akNewSeg[2] =
    {
        akU[0] + fTFirst*rkSegVelocity,
        akU[1] + fTFirst*rkSegVelocity
    };

    // move box to new position
    Box3<Real> kNewBox;
    kNewBox.Center() = rkBox.Center() + fTFirst*rkBoxVelocity;
    for (int i = 0; i < 3; i++)
    {
        kNewBox.Axis(i) = rkBox.Axis(i);
        kNewBox.Extent(i) = rkBox.Extent(i);
    }

    if ( eSide == LEFT )
    {
        // box on left of line
        if ( rkSegContact.m_kMap == m11 )
        {
            riQuantity = 1;
            akP[0] = akNewSeg[rkSegContact.m_aiIndex[0]];
        }
        else if ( rkBoxContact.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
        }
        else if ( rkBoxContact.m_kMap == m2_2 )
        {
            // segment-segment intersection
            Vector3<Real> akBoxSeg[2];
            akBoxSeg[0] = GetPoint(rkBoxContact.m_aiIndex[6],kNewBox);
            akBoxSeg[1] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
            FindContactSetLinLin(akNewSeg,akBoxSeg,riQuantity,akP);
        }
        else // rkBoxContact.m_kMap == m44
        {
            // segment-boxface intersection
            Vector3<Real> akBoxFace[4];
            akBoxFace[0] = GetPoint(rkBoxContact.m_aiIndex[4],kNewBox);
            akBoxFace[1] = GetPoint(rkBoxContact.m_aiIndex[5],kNewBox);
            akBoxFace[2] = GetPoint(rkBoxContact.m_aiIndex[6],kNewBox);
            akBoxFace[3] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
            FindContactSetCoplanarLineRect(akNewSeg,akBoxFace,riQuantity,akP);
        }
    }
    else // eSide == RIGHT 
    {
        // box on right of line
        if ( rkSegContact.m_kMap == m11 )
        {
            riQuantity = 1;
            akP[0] = akNewSeg[rkSegContact.m_aiIndex[1]];
        }
        else if ( rkBoxContact.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
        }
        else if ( rkBoxContact.m_kMap == m2_2 )
        {
            // segment-segment intersection
            Vector3<Real> akBoxSeg[2];
            akBoxSeg[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
            akBoxSeg[1] = GetPoint(rkBoxContact.m_aiIndex[1],kNewBox);
            FindContactSetLinLin(akNewSeg,akBoxSeg,riQuantity,akP);
        }
        else // rkBoxContact.m_kMap == m44
        {
            // segment-boxface intersection
            Vector3<Real> akBoxFace[4];
            akBoxFace[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
            akBoxFace[1] = GetPoint(rkBoxContact.m_aiIndex[1],kNewBox);
            akBoxFace[2] = GetPoint(rkBoxContact.m_aiIndex[2],kNewBox);
            akBoxFace[3] = GetPoint(rkBoxContact.m_aiIndex[3],kNewBox);
            FindContactSetCoplanarLineRect(akNewSeg,akBoxFace,riQuantity,akP);
        }
     }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax)
{
    // get end points of segment
    Vector3<Real> akU[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };

    // get box velocity relative to segment
    Vector3<Real> kVel = rkBoxVelocity - rkSegVelocity;

    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    int i;
    Vector3<Real> kAxis;

    // test box normals
    for (i = 0; i < 3; i++)
    {
        kAxis = rkBox.Axis(i);
        if ( !SegBoxAxisTest(kAxis,akU,kVel,rkBox,rfTFirst,fTLast,fTMax) )
            return false;
    }

    // test seg-direction cross box-edges
    for (i = 0; i < 3; i++ )
    {
        kAxis = rkBox.Axis(i).Cross(rkSegment.Direction());
        if ( !SegBoxAxisTest(kAxis,akU,kVel,rkBox,rfTFirst,fTLast,fTMax) )
            return false;
    }

    // test velocity cross box-faces
    for (i = 0; i < 3; i++)
    {
        kAxis = kVel.Cross(rkBox.Axis(i));
        if ( !SegBoxAxisTest(kAxis,akU,kVel,rkBox,rfTFirst,fTLast,fTMax) )
            return false;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment3<Real>& rkSegment,
    const Vector3<Real>& rkSegVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[2])
{
    // get end points of segment
    Vector3<Real> akU[2] =
    {
        rkSegment.Origin(),
        rkSegment.Origin() + rkSegment.Direction()
    };

    // get box velocity relative to segment
    Vector3<Real> kVel = rkBoxVelocity - rkSegVelocity;

    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    int i;
    Vector3<Real> kAxis;
    ContactSide eSide = NONE;
    ContactConfig<Real> kSegContact, kBoxContact;

    // test box normals
    for (i = 0; i < 3; i++)
    {
        kAxis = rkBox.Axis(i);
        if ( !SegBoxAxisFind(kAxis,kVel,akU,rkBox,rfTFirst,fTLast,fTMax,
             eSide,kSegContact,kBoxContact) )
        {
            return false;
        }
    }

    // test seg-direction cross box-edges
    for (i = 0; i < 3; i++ )
    {
        kAxis = rkBox.Axis(i).Cross(rkSegment.Direction());
        if ( !SegBoxAxisFind(kAxis,kVel,akU,rkBox,rfTFirst,fTLast,fTMax,
             eSide,kSegContact,kBoxContact) )
        {
            return false;
        }
    }

    // test velocity cross box-faces
    for (i = 0; i < 3; i++)
    {
        kAxis = kVel.Cross(rkBox.Axis(i));
        if ( !SegBoxAxisFind(kAxis,kVel,akU,rkBox,rfTFirst,fTLast,fTMax,
             eSide,kSegContact,kBoxContact) )
        {
            return false;
        }
    }

    if ( rfTFirst < (Real)0.0 || eSide == NONE )
    {
        // intersecting now
        return false;
    }

    FindContactSet(akU,rkBox,eSide,kSegContact,kBoxContact,rkSegVelocity,
        rkBoxVelocity,rfTFirst,riQuantity,akP);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Box3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Ray3<float>&, const Box3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Line3<float>&, const Box3<float>& rkBox);
template WML_ITEM bool FindIntersection<float> (
    const Vector3<float>&, const Vector3<float>&, const float[3],
    float&, float&);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Box3<float>&, int&, Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Ray3<float>&, const Box3<float>&, int&, Vector3<float>[2]);
template WML_ITEM bool FindIntersection<float> (
    const Line3<float>&, const Box3<float>&, int&, Vector3<float>[2]);
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Vector3<float>&, const Box3<float>&,
    const Vector3<float>&, float&, float);
template WML_ITEM bool FindIntersection<float> (
    const Segment3<float>&, const Vector3<float>&, const Box3<float>&,
    const Vector3<float>&, float&, float, int&, Vector3<float>[2]);

template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Box3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Ray3<double>&, const Box3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Line3<double>&, const Box3<double>& rkBox);
template WML_ITEM bool FindIntersection<double> (
    const Vector3<double>&, const Vector3<double>&, const double[3],
    double&, double&);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Box3<double>&, int&, Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Ray3<double>&, const Box3<double>&, int&, Vector3<double>[2]);
template WML_ITEM bool FindIntersection<double> (
    const Line3<double>&, const Box3<double>&, int&, Vector3<double>[2]);
template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Vector3<double>&, const Box3<double>&,
    const Vector3<double>&, double&, double);
template WML_ITEM bool FindIntersection<double> (
    const Segment3<double>&, const Vector3<double>&, const Box3<double>&,
    const Vector3<double>&, double&, double, int&, Vector3<double>[2]);
}
//----------------------------------------------------------------------------
