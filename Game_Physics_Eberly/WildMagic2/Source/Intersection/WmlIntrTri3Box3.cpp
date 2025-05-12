// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrTri3Box3.h"
#include "WmlIntrUtilityLin3.h"
#include "WmlIntrUtilityTri3.h"
#include "WmlIntrUtilityBox3.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary objects
//----------------------------------------------------------------------------
template <class Real>
static void ProjectTriangle (const Vector3<Real>& rkD,
    const Vector3<Real> akV[3], Real& rfMin, Real& rfMax)
{
    rfMin = rkD.Dot(akV[0]);
    rfMax = rfMin;

    Real fDot = rkD.Dot(akV[1]);
    if ( fDot < rfMin )
        rfMin = fDot;
    else if ( fDot > rfMax )
        rfMax = fDot;

    fDot = rkD.Dot(akV[2]);
    if ( fDot < rfMin )
        rfMin = fDot;
    else if ( fDot > rfMax )
        rfMax = fDot;
}
//----------------------------------------------------------------------------
template <class Real>
static void ProjectBox (const Vector3<Real>& rkD, const Box3<Real>& rkBox,
    Real& rfMin, Real& rfMax)
{
    Real fDdC = rkD.Dot(rkBox.Center());
    Real fR =
        rkBox.Extent(0)*Math<Real>::FAbs(rkD.Dot(rkBox.Axis(0))) +
        rkBox.Extent(1)*Math<Real>::FAbs(rkD.Dot(rkBox.Axis(1))) +
        rkBox.Extent(2)*Math<Real>::FAbs(rkD.Dot(rkBox.Axis(2)));
    rfMin = fDdC - fR;
    rfMax = fDdC + fR;
}
//----------------------------------------------------------------------------
template <class Real>
static bool NoIntersect (Real fTMax, Real fSpeed, Real fMin0, Real fMax0,
    Real fMin1, Real fMax1, Real& rfTFirst, Real& rfTLast)
{
    Real fInvSpeed, fT;

    if ( fMax1 < fMin0 )  // C1 initially on left of C0
    {
        if ( fSpeed <= (Real)0.0 )
        {
            // intervals moving apart
            return true;
        }

        fInvSpeed = ((Real)1.0)/fSpeed;

        fT = (fMin0 - fMax1)*fInvSpeed;
        if ( fT > rfTFirst )
            rfTFirst = fT;
        if ( rfTFirst > fTMax )
            return true;

        fT = (fMax0 - fMin1)*fInvSpeed;
        if ( fT < rfTLast )
            rfTLast = fT;
        if ( rfTFirst > rfTLast )
            return true;
    }
    else if ( fMax0 < fMin1 )  // C1 initially on right of C0
    {
        if ( fSpeed >= (Real)0.0 )
        {
            // intervals moving apart
            return true;
        }

        fInvSpeed = ((Real)1.0)/fSpeed;

        fT = (fMax0 - fMin1)*fInvSpeed;
        if ( fT > rfTFirst )
            rfTFirst = fT;
        if ( rfTFirst > fTMax )
            return true;

        fT = (fMin0 - fMax1)*fInvSpeed;
        if ( fT < rfTLast )
            rfTLast = fT;
        if ( rfTFirst > rfTLast )
            return true;
    }
    else  // C0 and C1 overlap
    {
        if ( fSpeed > (Real)0.0 )
        {
            fT = (fMax0 - fMin1)/fSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;
            if ( rfTFirst > rfTLast )
                return true;
        }
        else if ( fSpeed < (Real)0.0 )
        {
            fT = (fMin0 - fMax1)/fSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;
            if ( rfTFirst > rfTLast )
                return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle3<Real>& rkTri,
    const Box3<Real>& rkBox)
{
    // get triangle vertices
    Vector3<Real> akV[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };

    Real fMin0, fMax0, fMin1, fMax1;
    Vector3<Real> kD, akE[3];

    // test direction of triangle normal
    akE[0] = akV[1] - akV[0];
    akE[1] = akV[2] - akV[0];
    kD = akE[0].Cross(akE[1]);
    fMin0 = kD.Dot(akV[0]);
    fMax0 = fMin0;
    ProjectBox(kD,rkBox,fMin1,fMax1);
    if ( fMax1 < fMin0 || fMax0 < fMin1 )
        return false;

    // test direction of box faces
    for (int i = 0; i < 3; i++)
    {
        kD = rkBox.Axis(i);
        ProjectTriangle(kD,akV,fMin0,fMax0);
        Real fDdC = kD.Dot(rkBox.Center());
        fMin1 = fDdC - rkBox.Extent(i);
        fMax1 = fDdC + rkBox.Extent(i);
        if ( fMax1 < fMin0 || fMax0 < fMin1 )
            return false;
    }

    // test direction of triangle-box edge cross products
    akE[2] = akE[1] - akE[0];
    for (int i0 = 0; i0 < 3; i0++)
    {
        for (int i1 = 0; i1 < 3; i1++)
        {
            kD = akE[i0].Cross(rkBox.Axis(i1));
            ProjectTriangle(kD,akV,fMin0,fMax0);
            ProjectBox(kD,rkBox,fMin1,fMax1);
            if ( fMax1 < fMin0 || fMax0 < fMin1 )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Triangle3<Real>& rkTri,
    const Box3<Real>& rkBox, int& riQuantity, Vector3<Real> akP[6])
{
    riQuantity = 3;
    akP[0] = rkTri.Origin();
    akP[1] = rkTri.Origin() + rkTri.Edge0();
    akP[2] = rkTri.Origin() + rkTri.Edge1();

    for (int iDir = -1; iDir <= 1; iDir += 2)
    {
        for (int iSide = 0; iSide < 3; iSide++)
        {
            Vector3<Real> kInnerNormal = ((Real)iDir)*rkBox.Axis(iSide);
            Real fConstant = kInnerNormal.Dot(rkBox.Center()) -
                rkBox.Extent(iSide);
            ClipConvexPolygonAgainstPlane(kInnerNormal,fConstant,riQuantity,
                akP);
        }
    }

    return riQuantity > 0;
}
//----------------------------------------------------------------------------
// moving objects
//----------------------------------------------------------------------------
template <class Real>
static bool TriBoxAxisFind (const Vector3<Real>& rkAxis,  
    const Vector3<Real>& rkVel, const Vector3<Real> akV[3],
    const Box3<Real>& rkBox, Real& rfTFirst, Real& rfTLast, Real fTMax,
    ContactSide& reSide, ContactConfig<Real>& rkTUC,
    ContactConfig<Real>& rkTVC)
{
    ContactConfig<Real> kUC;
    GetTriConfiguration(rkAxis,akV,kUC);

    ContactConfig<Real> kVC;
    GetBoxConfiguration(rkAxis,rkBox,kVC);

    return AxisFind(rkVel,rkAxis,kUC,kVC,reSide,rkTUC,rkTVC,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSetCoplanarTriRect (const Vector3<Real> akTri[3],
    const Vector3<Real> akBox[4], int& riQuantity, Vector3<Real>* akP)
{
    // The potential intersection is initialized to the triangle, and then
    // clipped against the sides of the box

    riQuantity = 3;
    memcpy(akP,akTri,3*sizeof(Vector3<Real>));

    for (int i0 = 3, i1 = 0; i1 < 4; i0 = i1++)
    {
        Vector3<Real> kNormal = akBox[i1] - akBox[i0];
        Real fConstant = kNormal.Dot(akBox[i0]);
        ClipConvexPolygonAgainstPlane(kNormal,fConstant,riQuantity,akP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSet (const Vector3<Real> akV[3],
    const Box3<Real>& rkBox, ContactSide eSide,
    const ContactConfig<Real>& rkTriContact,
    const ContactConfig<Real>& rkBoxContact,
    const Vector3<Real>& rkTriVelocity, const Vector3<Real>& rkBoxVelocity,
    Real fTFirst, int& riQuantity, Vector3<Real>* akP)
{
    // move triangle to new position
    Vector3<Real> akNewTri[2] =
    {
        akV[0] + fTFirst*rkTriVelocity,
        akV[1] + fTFirst*rkTriVelocity
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
        // box on left of tri
        if ( rkTriContact.m_kMap == m111 || rkTriContact.m_kMap == m12 )
        {
            riQuantity = 1;
            akP[0] = akNewTri[rkTriContact.m_aiIndex[0]];
        }
        else if ( rkBoxContact.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
        }
        else if ( rkTriContact.m_kMap == m21 )
        {
            if ( rkBoxContact.m_kMap == m2_2 )
            {
                // triseg-boxseg intersection
                Vector3<Real> akTriSeg[2], akBoxSeg[2];
                akTriSeg[0] = akNewTri[rkTriContact.m_aiIndex[0]];
                akTriSeg[1] = akNewTri[rkTriContact.m_aiIndex[1]];
                akBoxSeg[0] = GetPoint(rkBoxContact.m_aiIndex[6],kNewBox);
                akBoxSeg[1] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
                FindContactSetLinLin(akTriSeg,akBoxSeg,riQuantity,akP);
            }
            else // rkBoxContact.m_kMap == m44
            {
                // triseg-boxface intersection
                Vector3<Real> akTriSeg[2], akBoxFace[4];
                akTriSeg[0] = akNewTri[rkTriContact.m_aiIndex[0]];
                akTriSeg[1] = akNewTri[rkTriContact.m_aiIndex[1]];
                akBoxFace[0] = GetPoint(rkBoxContact.m_aiIndex[4],kNewBox);
                akBoxFace[1] = GetPoint(rkBoxContact.m_aiIndex[5],kNewBox);
                akBoxFace[2] = GetPoint(rkBoxContact.m_aiIndex[6],kNewBox);
                akBoxFace[3] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
                FindContactSetCoplanarLineRect(akTriSeg,akBoxFace,riQuantity,
                    akP);
            }
        }
        else // rkTriContact.m_kMap == m3
        {
            if ( rkBoxContact.m_kMap == m2_2 )
            {
                // boxseg-triface intersection
                Vector3<Real> akBoxSeg[2];
                akBoxSeg[0] = GetPoint(rkBoxContact.m_aiIndex[6],kNewBox);
                akBoxSeg[1] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
                FindContactSetColinearLineTri(akBoxSeg,akNewTri,riQuantity,
                    akP);
            }
            else
            {
                // triface-boxface intersection
                Vector3<Real> akBoxFace[4];
                akBoxFace[0] = GetPoint(rkBoxContact.m_aiIndex[4],kNewBox);
                akBoxFace[1] = GetPoint(rkBoxContact.m_aiIndex[5],kNewBox);
                akBoxFace[2] = GetPoint(rkBoxContact.m_aiIndex[6],kNewBox);
                akBoxFace[3] = GetPoint(rkBoxContact.m_aiIndex[7],kNewBox);
                FindContactSetCoplanarTriRect(akNewTri,akBoxFace,riQuantity,
                    akP);
            }
        }
    }
    else // eSide == RIGHT 
    {
        // box on right of tri
        if ( rkTriContact.m_kMap == m111 || rkTriContact.m_kMap == m21 )
        {
            riQuantity = 1;
            akP[0] = akNewTri[rkTriContact.m_aiIndex[2]];
        }
        else if ( rkBoxContact.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
        }
        else if ( rkTriContact.m_kMap == m12 )
        {
            if ( rkBoxContact.m_kMap == m2_2 )
            {
                // segment-segment intersection
                Vector3<Real> akTriSeg[2], akBoxSeg[2];
                akTriSeg[0] = akNewTri[rkTriContact.m_aiIndex[1]];
                akTriSeg[1] = akNewTri[rkTriContact.m_aiIndex[2]];
                akBoxSeg[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
                akBoxSeg[1] = GetPoint(rkBoxContact.m_aiIndex[1],kNewBox);
                FindContactSetLinLin(akTriSeg,akBoxSeg,riQuantity,akP);
            }
            else // rkBoxContact.m_kMap == m44
            {
                // triseg-boxface intersection
                Vector3<Real> akTriSeg[2], akBoxFace[4];
                akTriSeg[0] = akNewTri[rkTriContact.m_aiIndex[1]];
                akTriSeg[1] = akNewTri[rkTriContact.m_aiIndex[2]];
                akBoxFace[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
                akBoxFace[1] = GetPoint(rkBoxContact.m_aiIndex[1],kNewBox);
                akBoxFace[2] = GetPoint(rkBoxContact.m_aiIndex[2],kNewBox);
                FindContactSetCoplanarLineRect(akTriSeg,akBoxFace,riQuantity,
                    akP);
            }
        }
        else // rkTriContact.m_kMap == m3
        {
            if ( rkBoxContact.m_kMap == m2_2 )
            {
                // boxseg-triface intersection
                Vector3<Real> akBoxSeg[2];
                akBoxSeg[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
                akBoxSeg[1] = GetPoint(rkBoxContact.m_aiIndex[1],kNewBox);
                FindContactSetColinearLineTri(akBoxSeg,akNewTri,riQuantity,
                    akP);
            }
            else
            {
                // triface-boxface intersection
                Vector3<Real> akBoxFace[4];
                akBoxFace[0] = GetPoint(rkBoxContact.m_aiIndex[0],kNewBox);
                akBoxFace[1] = GetPoint(rkBoxContact.m_aiIndex[1],kNewBox);
                akBoxFace[2] = GetPoint(rkBoxContact.m_aiIndex[2],kNewBox);
                akBoxFace[3] = GetPoint(rkBoxContact.m_aiIndex[3],kNewBox);
                FindContactSetCoplanarTriRect(akNewTri,akBoxFace,riQuantity,
                    akP);
            }
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real fTMax, Real& rfTFirst,
    Real& rfTLast)
{
    // get triangle vertices
    Vector3<Real> akV[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };

    Real fMin0, fMax0, fMin1, fMax1, fSpeed;
    Vector3<Real> kD, akE[3];

    // process as if triangle is stationary, box is moving
    Vector3<Real> kW = rkBoxVelocity - rkTriVelocity;
    rfTFirst = (Real)0.0;
    rfTLast = Math<Real>::MAX_REAL;

    // test direction of triangle normal
    akE[0] = akV[1] - akV[0];
    akE[1] = akV[2] - akV[0];
    kD = akE[0].Cross(akE[1]);
    fMin0 = kD.Dot(akV[0]);
    fMax0 = fMin0;
    ProjectBox(kD,rkBox,fMin1,fMax1);
    fSpeed = kD.Dot(kW);
    if ( NoIntersect(fTMax,fSpeed,fMin0,fMax0,fMin1,fMax1,rfTFirst,rfTLast) )
        return false;

    // test direction of box faces
    for (int i = 0; i < 3; i++)
    {
        kD = rkBox.Axis(i);
        ProjectTriangle(kD,akV,fMin0,fMax0);
        Real fDdC = kD.Dot(rkBox.Center());
        fMin1 = fDdC - rkBox.Extent(i);
        fMax1 = fDdC + rkBox.Extent(i);
        fSpeed = kD.Dot(kW);
        if ( NoIntersect(fTMax,fSpeed,fMin0,fMax0,fMin1,fMax1,rfTFirst,
             rfTLast) )
        {
            return false;
        }
    }

    // test direction of triangle-box edge cross products
    akE[2] = akE[1] - akE[0];
    for (int i0 = 0; i0 < 3; i0++)
    {
        for (int i1 = 0; i1 < 3; i1++)
        {
            kD = akE[i0].Cross(rkBox.Axis(i1));
            ProjectTriangle(kD,akV,fMin0,fMax0);
            ProjectBox(kD,rkBox,fMin1,fMax1);
            fSpeed = kD.Dot(kW);
            if ( NoIntersect(fTMax,fSpeed,fMin0,fMax0,fMin1,fMax1,rfTFirst,
                 rfTLast) )
            {
                return false;
            }
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Triangle3<Real>& rkTri,
    const Vector3<Real>& rkTriVelocity, const Box3<Real>& rkBox,
    const Vector3<Real>& rkBoxVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6])
{
    // get triangle vertices
    Vector3<Real> akV[3] =
    {
        rkTri.Origin(),
        rkTri.Origin() + rkTri.Edge0(),
        rkTri.Origin() + rkTri.Edge1()
    };

    // process as if triangle is stationary, box is moving
    Vector3<Real> kVel = rkBoxVelocity - rkTriVelocity;

    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    ContactSide eSide = NONE;
    ContactConfig<Real> kTriContact, kBoxContact;

    // test tri-normal
    Vector3<Real> akE[3] = { akV[1]-akV[0], akV[2]-akV[1],  akV[0]-akV[2] };
    Vector3<Real> kTriNorm = akE[0].Cross(akE[1]);
    if ( !TriBoxAxisFind(kTriNorm,kVel,akV,rkBox,rfTFirst,fTLast,fTMax,eSide,
         kTriContact,kBoxContact) )
    {
        return false;
    }

    Vector3<Real> kAxis;
    int iCoplanar = -1; // triangle coplanar to no box normals
    int i0;
    for (i0 = 0; i0 < 3; i0++)
    {
        kAxis = rkBox.Axis(i0);
        if ( !TriBoxAxisFind(kAxis,kVel,akV,rkBox,rfTFirst,fTLast,fTMax,
             eSide,kTriContact,kBoxContact) )
        {
            return false;
        }

        // Test if axis is parallel to triangle normal.  The test is:
        // sin(Angle(normal,axis)) < epsilon
        Real fNdA = kTriNorm.Dot(kAxis);
        Real fNdN = kTriNorm.SquaredLength();
        Real fAdA = kAxis.SquaredLength();
        Real fSin = Math<Real>::Sqrt(Math<Real>::FAbs((Real)1.0 -
            fNdA*fNdA/(fNdN*fAdA)));
        if ( fSin < Math<Real>::EPSILON )
            iCoplanar = i0;
    }

    if ( iCoplanar == -1 )
    {
        // test triedges cross boxfaces
        for (i0 = 0; i0 < 3; i0++ )
        {
            for (int i1 = 0; i1 < 3; i1++ )
            {
                kAxis = akE[i0].Cross(rkBox.Axis(i1));
                if ( !TriBoxAxisFind(kAxis,kVel,akV,rkBox,rfTFirst,fTLast,
                     fTMax,eSide,kTriContact,kBoxContact) )
                {
                    return false;
                }
            }
        }
    }
    else
    {
        // test triedges cross coplanar box axis
        for (i0 = 0; i0 < 3; i0++)
        {
            kAxis = akE[i0].Cross(kTriNorm);
            if ( !TriBoxAxisFind(kAxis,kVel,akV,rkBox,rfTFirst,fTLast,fTMax,
                 eSide,kTriContact,kBoxContact) )
            {
                return false;
            }
        }
    }

    // test velocity cross box faces
    for (i0 = 0; i0 < 3; i0++)
    {
        kAxis = kVel.Cross(rkBox.Axis(i0));
        if ( !TriBoxAxisFind(kAxis,kVel,akV,rkBox,rfTFirst,fTLast,fTMax,
             eSide,kTriContact,kBoxContact) )
        {
            return false;
        }
    }

    if ( rfTFirst < (Real)0.0 || eSide == NONE )
        return false;

    FindContactSet(akV,rkBox,eSide,kTriContact,kBoxContact,rkTriVelocity,
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
    const Triangle3<float>&, const Box3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Triangle3<float>&, const Box3<float>&, int&,
    Vector3<float>[6]);
template WML_ITEM bool TestIntersection<float> (
    const Triangle3<float>&, const Vector3<float>&, const Box3<float>&,
    const Vector3<float>&, float, float&, float&);
template WML_ITEM bool FindIntersection<float> (
    const Triangle3<float>&, const Vector3<float>&, const Box3<float>&,
    const Vector3<float>&, float&, float, int&, Vector3<float>[6]);

template WML_ITEM bool TestIntersection<double> (
    const Triangle3<double>&, const Box3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Triangle3<double>&, const Box3<double>&, int&,
    Vector3<double>[6]);
template WML_ITEM bool TestIntersection<double> (
    const Triangle3<double>&, const Vector3<double>&, const Box3<double>&,
    const Vector3<double>&, double, double&, double&);
template WML_ITEM bool FindIntersection<double> (
    const Triangle3<double>&, const Vector3<double>&, const Box3<double>&,
    const Vector3<double>&, double&, double, int&, Vector3<double>[6]);
}
//----------------------------------------------------------------------------
