// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrBox3Box3.h"
#include "WmlIntrUtilityLin3.h"
#include "WmlIntrUtilityBox3.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary objects
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Box3<Real>& rkBox0,
    const Box3<Real>& rkBox1)
{
    // Cutoff for cosine of angles between box axes.  This is used to catch
    // the cases when at least one pair of axes are parallel.  If this happens,
    // there is no need to test for separation along the Cross(A[i],B[j])
    // directions.
    const Real fCutoff = (Real)0.999999;
    bool bExistsParallelPair = false;
    int i;

    // convenience variables
    const Vector3<Real>* akA = rkBox0.Axes();
    const Vector3<Real>* akB = rkBox1.Axes();
    const Real* afEA = rkBox0.Extents();
    const Real* afEB = rkBox1.Extents();

    // compute difference of box centers, D = C1-C0
    Vector3<Real> kD = rkBox1.Center() - rkBox0.Center();

    Real aafC[3][3];     // matrix C = A^T B, c_{ij} = Dot(A_i,B_j)
    Real aafAbsC[3][3];  // |c_{ij}|
    Real afAD[3];        // Dot(A_i,D)
    Real fR0, fR1, fR;   // interval radii and distance between centers
    Real fR01;           // = R0 + R1

    // axis C0+t*A0
    for (i = 0; i < 3; i++)
    {
        aafC[0][i] = akA[0].Dot(akB[i]);
        aafAbsC[0][i] = Math<Real>::FAbs(aafC[0][i]);
        if ( aafAbsC[0][i] > fCutoff )
            bExistsParallelPair = true;
    }
    afAD[0] = akA[0].Dot(kD);
    fR = Math<Real>::FAbs(afAD[0]);
    fR1 = afEB[0]*aafAbsC[0][0]+afEB[1]*aafAbsC[0][1]+afEB[2]*aafAbsC[0][2];
    fR01 = afEA[0] + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A1
    for (i = 0; i < 3; i++)
    {
        aafC[1][i] = akA[1].Dot(akB[i]);
        aafAbsC[1][i] = Math<Real>::FAbs(aafC[1][i]);
        if ( aafAbsC[1][i] > fCutoff )
            bExistsParallelPair = true;
    }
    afAD[1] = akA[1].Dot(kD);
    fR = Math<Real>::FAbs(afAD[1]);
    fR1 = afEB[0]*aafAbsC[1][0]+afEB[1]*aafAbsC[1][1]+afEB[2]*aafAbsC[1][2];
    fR01 = afEA[1] + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A2
    for (i = 0; i < 3; i++)
    {
        aafC[2][i] = akA[2].Dot(akB[i]);
        aafAbsC[2][i] = Math<Real>::FAbs(aafC[2][i]);
        if ( aafAbsC[2][i] > fCutoff )
            bExistsParallelPair = true;
    }
    afAD[2] = akA[2].Dot(kD);
    fR = Math<Real>::FAbs(afAD[2]);
    fR1 = afEB[0]*aafAbsC[2][0]+afEB[1]*aafAbsC[2][1]+afEB[2]*aafAbsC[2][2];
    fR01 = afEA[2] + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*B0
    fR = Math<Real>::FAbs(akB[0].Dot(kD));
    fR0 = afEA[0]*aafAbsC[0][0]+afEA[1]*aafAbsC[1][0]+afEA[2]*aafAbsC[2][0];
    fR01 = fR0 + afEB[0];
    if ( fR > fR01 )
        return false;

    // axis C0+t*B1
    fR = Math<Real>::FAbs(akB[1].Dot(kD));
    fR0 = afEA[0]*aafAbsC[0][1]+afEA[1]*aafAbsC[1][1]+afEA[2]*aafAbsC[2][1];
    fR01 = fR0 + afEB[1];
    if ( fR > fR01 )
        return false;

    // axis C0+t*B2
    fR = Math<Real>::FAbs(akB[2].Dot(kD));
    fR0 = afEA[0]*aafAbsC[0][2]+afEA[1]*aafAbsC[1][2]+afEA[2]*aafAbsC[2][2];
    fR01 = fR0 + afEB[2];
    if ( fR > fR01 )
        return false;

    // At least one pair of box axes was parallel, so the separation is
    // effectively in 2D where checking the "edge" normals is sufficient for
    // the separation of the boxes.
    if ( bExistsParallelPair )
        return true;

    // axis C0+t*A0xB0
    fR = Math<Real>::FAbs(afAD[2]*aafC[1][0]-afAD[1]*aafC[2][0]);
    fR0 = afEA[1]*aafAbsC[2][0] + afEA[2]*aafAbsC[1][0];
    fR1 = afEB[1]*aafAbsC[0][2] + afEB[2]*aafAbsC[0][1];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A0xB1
    fR = Math<Real>::FAbs(afAD[2]*aafC[1][1]-afAD[1]*aafC[2][1]);
    fR0 = afEA[1]*aafAbsC[2][1] + afEA[2]*aafAbsC[1][1];
    fR1 = afEB[0]*aafAbsC[0][2] + afEB[2]*aafAbsC[0][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A0xB2
    fR = Math<Real>::FAbs(afAD[2]*aafC[1][2]-afAD[1]*aafC[2][2]);
    fR0 = afEA[1]*aafAbsC[2][2] + afEA[2]*aafAbsC[1][2];
    fR1 = afEB[0]*aafAbsC[0][1] + afEB[1]*aafAbsC[0][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A1xB0
    fR = Math<Real>::FAbs(afAD[0]*aafC[2][0]-afAD[2]*aafC[0][0]);
    fR0 = afEA[0]*aafAbsC[2][0] + afEA[2]*aafAbsC[0][0];
    fR1 = afEB[1]*aafAbsC[1][2] + afEB[2]*aafAbsC[1][1];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A1xB1
    fR = Math<Real>::FAbs(afAD[0]*aafC[2][1]-afAD[2]*aafC[0][1]);
    fR0 = afEA[0]*aafAbsC[2][1] + afEA[2]*aafAbsC[0][1];
    fR1 = afEB[0]*aafAbsC[1][2] + afEB[2]*aafAbsC[1][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A1xB2
    fR = Math<Real>::FAbs(afAD[0]*aafC[2][2]-afAD[2]*aafC[0][2]);
    fR0 = afEA[0]*aafAbsC[2][2] + afEA[2]*aafAbsC[0][2];
    fR1 = afEB[0]*aafAbsC[1][1] + afEB[1]*aafAbsC[1][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A2xB0
    fR = Math<Real>::FAbs(afAD[1]*aafC[0][0]-afAD[0]*aafC[1][0]);
    fR0 = afEA[0]*aafAbsC[1][0] + afEA[1]*aafAbsC[0][0];
    fR1 = afEB[1]*aafAbsC[2][2] + afEB[2]*aafAbsC[2][1];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A2xB1
    fR = Math<Real>::FAbs(afAD[1]*aafC[0][1]-afAD[0]*aafC[1][1]);
    fR0 = afEA[0]*aafAbsC[1][1] + afEA[1]*aafAbsC[0][1];
    fR1 = afEB[0]*aafAbsC[2][2] + afEB[2]*aafAbsC[2][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0+t*A2xB2
    fR = Math<Real>::FAbs(afAD[1]*aafC[0][2]-afAD[0]*aafC[1][2]);
    fR0 = afEA[0]*aafAbsC[1][2] + afEA[1]*aafAbsC[0][2];
    fR1 = afEB[0]*aafAbsC[2][1] + afEB[1]*aafAbsC[2][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    return true;
}
//----------------------------------------------------------------------------
// moving objects
//----------------------------------------------------------------------------
template <class Real>
static bool BoxAxisFind (const Vector3<Real>& rkVelocity, 
    const Vector3<Real>& rkAxis, const Box3<Real>& rkBoxU,
    const Box3<Real>& rkBoxV, Real& rfTFirst, Real& rfTLast, Real fTMax,
    ContactSide& reSide, ContactConfig<Real>& rkTUC,
    ContactConfig<Real>& rkTVC)
{
    ContactConfig<Real> kUC;
    GetBoxConfiguration(rkAxis,rkBoxU,kUC);

    ContactConfig<Real> kVC;
    GetBoxConfiguration(rkAxis,rkBoxV,kVC);

    return AxisFind(rkVelocity,rkAxis,kUC,kVC,reSide,rkTUC,rkTVC,rfTFirst,
        rfTLast,fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSetCoplanarRectRect (const Vector3<Real> akFace0[4],
    const Vector3<Real> akFace1[4], int& riQuantity, Vector3<Real>* akP)
{
    // The potential intersection is initialized to face 0, and then clipped
    // against the four sides of face 1.

    riQuantity = 4;
    memcpy(akP,akFace0,4*sizeof(Vector3<Real>));

    for (int i0 = 3, i1 = 0; i1 < 4; i0 = i1++)
    {
        Vector3<Real> kNormal = akFace1[i1] - akFace1[i0];
        Real fConstant = kNormal.Dot(akFace1[i0]);
        ClipConvexPolygonAgainstPlane(kNormal,fConstant,riQuantity,akP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSet (const Box3<Real>& rkBox0,
    const Box3<Real>& rkBox1, ContactSide eSide,
    const ContactConfig<Real>& rkUC, const ContactConfig<Real>& rkVC, 
    const Vector3<Real>& rkVel0, const Vector3<Real>& rkVel1,
    Real fTFirst, int& riQuantity, Vector3<Real>* akP)
{
    // move boxes to new position
    Box3<Real> kNewBox0, kNewBox1;
    kNewBox0.Center() = rkBox0.Center() + fTFirst*rkVel0;
    kNewBox1.Center() = rkBox1.Center() + fTFirst*rkVel1;
    for (int i = 0; i < 3; i++)
    {
        kNewBox0.Axis(i) = rkBox0.Axis(i);
        kNewBox0.Extent(i) = rkBox0.Extent(i);
        kNewBox1.Axis(i) = rkBox1.Axis(i);
        kNewBox1.Extent(i) = rkBox1.Extent(i);
    }

    if ( eSide == LEFT )
    {
        // box1 on left of box0
        if ( rkUC.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkUC.m_aiIndex[0],kNewBox0);
        }
        else if ( rkVC.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkVC.m_aiIndex[7],kNewBox1);
        }
        else if ( rkUC.m_kMap == m2_2 )
        {
            if ( rkVC.m_kMap == m2_2 )
            {
                // box0edge-box1edge intersection
                Vector3<Real> akEdge0[2], akEdge1[2];
                akEdge0[0] = GetPoint(rkUC.m_aiIndex[0],kNewBox0);
                akEdge0[1] = GetPoint(rkUC.m_aiIndex[1],kNewBox0);
                akEdge1[0] = GetPoint(rkVC.m_aiIndex[6],kNewBox1);
                akEdge1[1] = GetPoint(rkVC.m_aiIndex[7],kNewBox1);
                FindContactSetLinLin(akEdge0,akEdge1,riQuantity,akP);
            }
            else // rkVC.m_kMap == m44
            {
                // box0edge-box1face intersection
                Vector3<Real> akEdge0[2], akFace1[4];
                akEdge0[0] = GetPoint(rkUC.m_aiIndex[0],kNewBox0);
                akEdge0[1] = GetPoint(rkUC.m_aiIndex[1],kNewBox0); 
                akFace1[0] = GetPoint(rkVC.m_aiIndex[4],kNewBox1); 
                akFace1[1] = GetPoint(rkVC.m_aiIndex[5],kNewBox1); 
                akFace1[2] = GetPoint(rkVC.m_aiIndex[6],kNewBox1); 
                akFace1[3] = GetPoint(rkVC.m_aiIndex[7],kNewBox1); 
                FindContactSetCoplanarLineRect(akEdge0,akFace1,riQuantity,
                    akP);
            }
        }
        else // rkUC.m_kMap == m44
        {
            if ( rkVC.m_kMap == m2_2 )
            {
                // box0face-box1edge intersection
                Vector3<Real> akFace0[4], akEdge1[2];
                akFace0[0] = GetPoint(rkUC.m_aiIndex[0],kNewBox0);
                akFace0[1] = GetPoint(rkUC.m_aiIndex[1],kNewBox0);
                akFace0[2] = GetPoint(rkUC.m_aiIndex[2],kNewBox0);
                akFace0[3] = GetPoint(rkUC.m_aiIndex[3],kNewBox0);
                akEdge1[0] = GetPoint(rkVC.m_aiIndex[6],kNewBox1);
                akEdge1[1] = GetPoint(rkVC.m_aiIndex[7],kNewBox1);
                FindContactSetCoplanarLineRect(akEdge1,akFace0,riQuantity,
                    akP);
            }
            else
            {
                // box0face-box1face intersection
                Vector3<Real> akFace0[4], akFace1[4];
                akFace0[0] = GetPoint(rkUC.m_aiIndex[0],kNewBox0);
                akFace0[1] = GetPoint(rkUC.m_aiIndex[1],kNewBox0);
                akFace0[2] = GetPoint(rkUC.m_aiIndex[2],kNewBox0);
                akFace0[3] = GetPoint(rkUC.m_aiIndex[3],kNewBox0);
                akFace1[0] = GetPoint(rkVC.m_aiIndex[4],kNewBox1);
                akFace1[1] = GetPoint(rkVC.m_aiIndex[5],kNewBox1);
                akFace1[2] = GetPoint(rkVC.m_aiIndex[6],kNewBox1);
                akFace1[3] = GetPoint(rkVC.m_aiIndex[7],kNewBox1);
                FindContactSetCoplanarRectRect(akFace0,akFace1,riQuantity,
                    akP);
            }
        }
    }
    else // rkSide == RIGHT 
    {
        // box1 on right of box0
        if ( rkUC.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkUC.m_aiIndex[7],kNewBox0);
        }
        else if ( rkVC.m_kMap == m1_1 )
        {
            riQuantity = 1;
            akP[0] = GetPoint(rkUC.m_aiIndex[0],kNewBox1);
        }
        else if ( rkUC.m_kMap == m2_2 )
        {
            if ( rkVC.m_kMap == m2_2 )
            {
                // box0edge-box1edge intersection
                Vector3<Real> akEdge0[2], akEdge1[2];
                akEdge0[0] = GetPoint(rkUC.m_aiIndex[6],kNewBox0);
                akEdge0[1] = GetPoint(rkUC.m_aiIndex[7],kNewBox0);
                akEdge1[0] = GetPoint(rkVC.m_aiIndex[0],kNewBox1);
                akEdge1[1] = GetPoint(rkVC.m_aiIndex[1],kNewBox1);
                FindContactSetLinLin(akEdge0,akEdge1,riQuantity,akP);
            }
            else // rkVC.m_kMap == m44
            {
                // box0edge-box1face intersection
                Vector3<Real> akEdge0[2], akFace1[4];
                akEdge0[0] = GetPoint(rkUC.m_aiIndex[6],kNewBox0);
                akEdge0[1] = GetPoint(rkUC.m_aiIndex[7],kNewBox0);
                akFace1[0] = GetPoint(rkVC.m_aiIndex[0],kNewBox1);
                akFace1[1] = GetPoint(rkVC.m_aiIndex[1],kNewBox1);
                akFace1[2] = GetPoint(rkVC.m_aiIndex[2],kNewBox1);
                akFace1[3] = GetPoint(rkVC.m_aiIndex[3],kNewBox1);
                FindContactSetCoplanarLineRect(akEdge0,akFace1,riQuantity,
                    akP);
            }
        }
        else // rkUC.m_kMap == m44
        {
            if ( rkVC.m_kMap == m2_2 )
            {
                // box0face-box1edge intersection
                Vector3<Real> akFace0[4], akEdge1[2];
                akFace0[0] = GetPoint(rkUC.m_aiIndex[4],kNewBox0);
                akFace0[1] = GetPoint(rkUC.m_aiIndex[5],kNewBox0);
                akFace0[2] = GetPoint(rkUC.m_aiIndex[6],kNewBox0);
                akFace0[3] = GetPoint(rkUC.m_aiIndex[7],kNewBox0);
                akEdge1[0] = GetPoint(rkVC.m_aiIndex[0],kNewBox1);
                akEdge1[1] = GetPoint(rkVC.m_aiIndex[1],kNewBox1);
                FindContactSetCoplanarLineRect(akEdge1,akFace0,riQuantity,
                    akP);
            }
            else // rkVC.m_kMap == m44
            {
                // box0face-box1face intersection
                Vector3<Real> akFace0[4], akFace1[4];
                akFace0[0] = GetPoint(rkUC.m_aiIndex[4],kNewBox0);
                akFace0[1] = GetPoint(rkUC.m_aiIndex[5],kNewBox0);
                akFace0[2] = GetPoint(rkUC.m_aiIndex[6],kNewBox0);
                akFace0[3] = GetPoint(rkUC.m_aiIndex[7],kNewBox0);
                akFace1[0] = GetPoint(rkVC.m_aiIndex[0],kNewBox1);
                akFace1[1] = GetPoint(rkVC.m_aiIndex[1],kNewBox1);
                akFace1[2] = GetPoint(rkVC.m_aiIndex[2],kNewBox1);
                akFace1[3] = GetPoint(rkVC.m_aiIndex[3],kNewBox1);
                FindContactSetCoplanarRectRect(akFace0,akFace1,riQuantity,
                    akP);
            }
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (Real fTime, const Box3<Real>& rkBox0,
    const Vector3<Real>& rkVel0, const Box3<Real>& rkBox1,
    const Vector3<Real>& rkVel1)
{
    // convenience variables
    const Vector3<Real>* akA = rkBox0.Axes();
    const Vector3<Real>* akB = rkBox1.Axes();
    const Real* afEA = rkBox0.Extents();
    const Real* afEB = rkBox1.Extents();

    // Compute relative velocity of box1 with respect to box0 so that box0
    // may as well be stationary.
    Vector3<Real> kW = rkVel1 - rkVel0;

    // Compute difference of box centers at time 0 and time 'fTime'.
    Vector3<Real> kD0 = rkBox1.Center() - rkBox0.Center();
    Vector3<Real> kD1 = kD0 + fTime*kW;

    Real aafC[3][3];     // matrix C = A^T B, c_{ij} = Dot(A_i,B_j)
    Real aafAbsC[3][3];  // |c_{ij}|
    Real afAD0[3];       // Dot(A_i,D0)
    Real afAD1[3];       // Dot(A_i,D1)
    Real fR0, fR1, fR;   // interval radii and distance between centers
    Real fR01;           // = R0 + R1
    
    // axis C0+t*A0
    aafC[0][0] = akA[0].Dot(akB[0]);
    aafC[0][1] = akA[0].Dot(akB[1]);
    aafC[0][2] = akA[0].Dot(akB[2]);
    afAD0[0] = akA[0].Dot(kD0);
    afAD1[0] = akA[0].Dot(kD1);
    aafAbsC[0][0] = Math<Real>::FAbs(aafC[0][0]);
    aafAbsC[0][1] = Math<Real>::FAbs(aafC[0][1]);
    aafAbsC[0][2] = Math<Real>::FAbs(aafC[0][2]);
    fR1 = afEB[0]*aafAbsC[0][0]+afEB[1]*aafAbsC[0][1]+afEB[2]*aafAbsC[0][2];
    fR01 = afEA[0] + fR1;
    if ( afAD0[0] > fR01 )
    {
        if ( afAD1[0] > fR01 )
            return false;
    }
    else if ( afAD0[0] < -fR01 )
    {
        if ( afAD1[0] < -fR01 )
            return false;
    }

    // axis C0+t*A1
    aafC[1][0] = akA[1].Dot(akB[0]);
    aafC[1][1] = akA[1].Dot(akB[1]);
    aafC[1][2] = akA[1].Dot(akB[2]);
    afAD0[1] = akA[1].Dot(kD0);
    afAD1[1] = akA[1].Dot(kD1);
    aafAbsC[1][0] = Math<Real>::FAbs(aafC[1][0]);
    aafAbsC[1][1] = Math<Real>::FAbs(aafC[1][1]);
    aafAbsC[1][2] = Math<Real>::FAbs(aafC[1][2]);
    fR1 = afEB[0]*aafAbsC[1][0]+afEB[1]*aafAbsC[1][1]+afEB[2]*aafAbsC[1][2];
    fR01 = afEA[1] + fR1;
    if ( afAD0[1] > fR01 )
    {
        if ( afAD1[1] > fR01 )
            return false;
    }
    else if ( afAD0[1] < -fR01 )
    {
        if ( afAD1[1] < -fR01 )
            return false;
    }

    // axis C0+t*A2
    aafC[2][0] = akA[2].Dot(akB[0]);
    aafC[2][1] = akA[2].Dot(akB[1]);
    aafC[2][2] = akA[2].Dot(akB[2]);
    afAD0[2] = akA[2].Dot(kD0);
    afAD1[2] = akA[2].Dot(kD1);
    aafAbsC[2][0] = Math<Real>::FAbs(aafC[2][0]);
    aafAbsC[2][1] = Math<Real>::FAbs(aafC[2][1]);
    aafAbsC[2][2] = Math<Real>::FAbs(aafC[2][2]);
    fR1 = afEB[0]*aafAbsC[2][0]+afEB[1]*aafAbsC[2][1]+afEB[2]*aafAbsC[2][2];
    fR01 = afEA[2] + fR1;
    if ( afAD0[2] > fR01 )
    {
        if ( afAD1[2] > fR01 )
            return false;
    }
    else if ( afAD0[2] < -fR01 )
    {
        if ( afAD1[2] < -fR01 )
            return false;
    }

    // axis C0+t*B0
    fR = akB[0].Dot(kD0);
    fR0 = afEA[0]*aafAbsC[0][0]+afEA[1]*aafAbsC[1][0]+afEA[2]*aafAbsC[2][0];
    fR01 = fR0 + afEB[0];
    if ( fR > fR01 )
    {
        fR = akB[0].Dot(kD1);
        if ( fR > fR01)
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = akB[0].Dot(kD1);
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*B1
    fR = akB[1].Dot(kD0);
    fR0 = afEA[0]*aafAbsC[0][1]+afEA[1]*aafAbsC[1][1]+afEA[2]*aafAbsC[2][1];
    fR01 = fR0 + afEB[1];
    if ( fR > fR01 )
    {
        fR = akB[1].Dot(kD1);
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = akB[1].Dot(kD1);
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*B2
    fR = akB[2].Dot(kD0);
    fR0 = afEA[0]*aafAbsC[0][2]+afEA[1]*aafAbsC[1][2]+afEA[2]*aafAbsC[2][2];
    fR01 = fR0 + afEB[2];
    if ( fR > fR01 )
    {
        fR = akB[2].Dot(kD1);
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = akB[2].Dot(kD1);
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A0xB0
    fR = afAD0[2]*aafC[1][0]-afAD0[1]*aafC[2][0];
    fR0 = afEA[1]*aafAbsC[2][0] + afEA[2]*aafAbsC[1][0];
    fR1 = afEB[1]*aafAbsC[0][2] + afEB[2]*aafAbsC[0][1];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[2]*aafC[1][0]-afAD1[1]*aafC[2][0];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[2]*aafC[1][0]-afAD1[1]*aafC[2][0];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A0xB1
    fR = afAD0[2]*aafC[1][1]-afAD0[1]*aafC[2][1];
    fR0 = afEA[1]*aafAbsC[2][1] + afEA[2]*aafAbsC[1][1];
    fR1 = afEB[0]*aafAbsC[0][2] + afEB[2]*aafAbsC[0][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[2]*aafC[1][1]-afAD1[1]*aafC[2][1];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[2]*aafC[1][1]-afAD1[1]*aafC[2][1];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A0xB2
    fR = afAD0[2]*aafC[1][2]-afAD0[1]*aafC[2][2];
    fR0 = afEA[1]*aafAbsC[2][2] + afEA[2]*aafAbsC[1][2];
    fR1 = afEB[0]*aafAbsC[0][1] + afEB[1]*aafAbsC[0][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[2]*aafC[1][2]-afAD1[1]*aafC[2][2];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[2]*aafC[1][2]-afAD1[1]*aafC[2][2];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A1xB0
    fR = afAD0[0]*aafC[2][0]-afAD0[2]*aafC[0][0];
    fR0 = afEA[0]*aafAbsC[2][0] + afEA[2]*aafAbsC[0][0];
    fR1 = afEB[1]*aafAbsC[1][2] + afEB[2]*aafAbsC[1][1];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[0]*aafC[2][0]-afAD1[2]*aafC[0][0];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[0]*aafC[2][0]-afAD1[2]*aafC[0][0];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A1xB1
    fR = afAD0[0]*aafC[2][1]-afAD0[2]*aafC[0][1];
    fR0 = afEA[0]*aafAbsC[2][1] + afEA[2]*aafAbsC[0][1];
    fR1 = afEB[0]*aafAbsC[1][2] + afEB[2]*aafAbsC[1][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[0]*aafC[2][1]-afAD1[2]*aafC[0][1];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[0]*aafC[2][1]-afAD1[2]*aafC[0][1];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A1xB2
    fR = afAD0[0]*aafC[2][2]-afAD0[2]*aafC[0][2];
    fR0 = afEA[0]*aafAbsC[2][2] + afEA[2]*aafAbsC[0][2];
    fR1 = afEB[0]*aafAbsC[1][1] + afEB[1]*aafAbsC[1][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[0]*aafC[2][2]-afAD1[2]*aafC[0][2];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[0]*aafC[2][2]-afAD1[2]*aafC[0][2];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A2xB0
    fR = afAD0[1]*aafC[0][0]-afAD0[0]*aafC[1][0];
    fR0 = afEA[0]*aafAbsC[1][0] + afEA[1]*aafAbsC[0][0];
    fR1 = afEB[1]*aafAbsC[2][2] + afEB[2]*aafAbsC[2][1];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[1]*aafC[0][0]-afAD1[0]*aafC[1][0];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[1]*aafC[0][0]-afAD1[0]*aafC[1][0];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A2xB1
    fR = afAD0[1]*aafC[0][1]-afAD0[0]*aafC[1][1];
    fR0 = afEA[0]*aafAbsC[1][1] + afEA[1]*aafAbsC[0][1];
    fR1 = afEB[0]*aafAbsC[2][2] + afEB[2]*aafAbsC[2][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[1]*aafC[0][1]-afAD1[0]*aafC[1][1];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[1]*aafC[0][1]-afAD1[0]*aafC[1][1];
        if ( fR < -fR01 )
            return false;
    }

    // axis C0+t*A2xB2
    fR = afAD0[1]*aafC[0][2]-afAD0[0]*aafC[1][2];
    fR0 = afEA[0]*aafAbsC[1][2] + afEA[1]*aafAbsC[0][2];
    fR1 = afEB[0]*aafAbsC[2][1] + afEB[1]*aafAbsC[2][0];
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
    {
        fR = afAD1[1]*aafC[0][2]-afAD1[0]*aafC[1][2];
        if ( fR > fR01 )
            return false;
    }
    else if ( fR < -fR01 )
    {
        fR = afAD1[1]*aafC[0][2]-afAD1[0]*aafC[1][2];
        if ( fR < -fR01 )
            return false;
    }

    // At this point none of the 15 axes separate the boxes.  It is still
    // possible that they are separated as viewed in any plane orthogonal
    // to the relative direction of motion W.  In the worst case, the two
    // projected boxes are hexagons.  This requires three separating axis
    // tests per box.
    Vector3<Real> kWxD0 = kW.Cross(kD0);
    Real afWA[3], afWB[3];

    // axis C0 + t*WxA0
    afWA[1] = kW.Dot(akA[1]);
    afWA[2] = kW.Dot(akA[2]);
    fR = Math<Real>::FAbs(akA[0].Dot(kWxD0));
    fR0 = afEA[1]*Math<Real>::FAbs(afWA[2]) + 
        afEA[2]*Math<Real>::FAbs(afWA[1]);
    fR1 =
        afEB[0]*Math<Real>::FAbs(aafC[1][0]*afWA[2] - aafC[2][0]*afWA[1]) +
        afEB[1]*Math<Real>::FAbs(aafC[1][1]*afWA[2] - aafC[2][1]*afWA[1]) +
        afEB[2]*Math<Real>::FAbs(aafC[1][2]*afWA[2] - aafC[2][2]*afWA[1]);
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0 + t*WxA1
    afWA[0] = kW.Dot(akA[0]);
    fR = Math<Real>::FAbs(akA[1].Dot(kWxD0));
    fR0 = afEA[2]*Math<Real>::FAbs(afWA[0]) + 
        afEA[0]*Math<Real>::FAbs(afWA[2]);
    fR1 =
        afEB[0]*Math<Real>::FAbs(aafC[2][0]*afWA[0] - aafC[0][0]*afWA[2]) +
        afEB[1]*Math<Real>::FAbs(aafC[2][1]*afWA[0] - aafC[0][1]*afWA[2]) +
        afEB[2]*Math<Real>::FAbs(aafC[2][2]*afWA[0] - aafC[0][2]*afWA[2]);
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0 + t*WxA2
    fR = Math<Real>::FAbs(akA[2].Dot(kWxD0));
    fR0 = afEA[0]*Math<Real>::FAbs(afWA[1]) +
        afEA[1]*Math<Real>::FAbs(afWA[0]);
    fR1 =
        afEB[0]*Math<Real>::FAbs(aafC[0][0]*afWA[1] - aafC[1][0]*afWA[0]) +
        afEB[1]*Math<Real>::FAbs(aafC[0][1]*afWA[1] - aafC[1][1]*afWA[0]) +
        afEB[2]*Math<Real>::FAbs(aafC[0][2]*afWA[1] - aafC[1][2]*afWA[0]);
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0 + t*WxB0
    afWB[1] = kW.Dot(akB[1]);
    afWB[2] = kW.Dot(akB[2]);
    fR = Math<Real>::FAbs(akB[0].Dot(kWxD0));
    fR0 =
        afEA[0]*Math<Real>::FAbs(aafC[0][1]*afWB[2] - aafC[0][2]*afWB[1]) +
        afEA[1]*Math<Real>::FAbs(aafC[1][1]*afWB[2] - aafC[1][2]*afWB[1]) +
        afEA[2]*Math<Real>::FAbs(aafC[2][1]*afWB[2] - aafC[2][2]*afWB[1]);
    fR1 = afEB[1]*Math<Real>::FAbs(afWB[2]) +
        afEB[2]*Math<Real>::FAbs(afWB[1]);
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0 + t*WxB1
    afWB[0] = kW.Dot(akB[0]);
    fR = Math<Real>::FAbs(akB[1].Dot(kWxD0));
    fR0 =
        afEA[0]*Math<Real>::FAbs(aafC[0][2]*afWB[0] - aafC[0][0]*afWB[2]) +
        afEA[1]*Math<Real>::FAbs(aafC[1][2]*afWB[0] - aafC[1][0]*afWB[2]) +
        afEA[2]*Math<Real>::FAbs(aafC[2][2]*afWB[0] - aafC[2][0]*afWB[2]);
    fR1 = afEB[2]*Math<Real>::FAbs(afWB[0]) +
        afEB[0]*Math<Real>::FAbs(afWB[2]);
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    // axis C0 + t*WxB2
    fR = Math<Real>::FAbs(akB[2].Dot(kWxD0));
    fR0 =
        afEA[0]*Math<Real>::FAbs(aafC[0][0]*afWB[1] - aafC[0][1]*afWB[0]) +
        afEA[1]*Math<Real>::FAbs(aafC[1][0]*afWB[1] - aafC[1][1]*afWB[0]) +
        afEA[2]*Math<Real>::FAbs(aafC[2][0]*afWB[1] - aafC[2][1]*afWB[0]);
    fR1 = afEB[0]*Math<Real>::FAbs(afWB[1]) +
        afEB[1]*Math<Real>::FAbs(afWB[0]);
    fR01 = fR0 + fR1;
    if ( fR > fR01 )
        return false;

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (Real fTime, int iNumSteps,
    const Box3<Real>& rkBox0, const Vector3<Real>& rkVel0,
    const Vector3<Real>& rkRotCen0, const Vector3<Real>& rkRotAxis0,
    const Box3<Real>& rkBox1, const Vector3<Real>& rkVel1,
    const Vector3<Real>& rkRotCen1, const Vector3<Real>& rkRotAxis1)
{
    // time step for the integration
    Real fStep = fTime/Real(iNumSteps);

    // initialize subinterval boxes
    Box3<Real> kSubBox0, kSubBox1;
    kSubBox0.Center() = rkBox0.Center();
    kSubBox1.Center() = rkBox1.Center();
    int i;
    for (i = 0; i < 3; i++)
    {
        kSubBox0.Axis(i) = rkBox0.Axis(i);
        kSubBox1.Axis(i) = rkBox1.Axis(i);
    }

    // integrate the differential equations using Euler's method
    for (int iStep = 1; iStep <= iNumSteps; iStep++)
    {
        // compute box velocities and test boxes for intersection
        Real fSubTime = fStep*Real(iStep);
        Vector3<Real> kNewRotCen0 = rkRotCen0 + fSubTime*rkVel0;
        Vector3<Real> kNewRotCen1 = rkRotCen1 + fSubTime*rkVel1;
        Vector3<Real> kDiff0 = kSubBox0.Center() - kNewRotCen0;
        Vector3<Real> kDiff1 = kSubBox1.Center() - kNewRotCen1;
        Vector3<Real> kSubVel0 = fStep*(rkVel0 + rkRotAxis0.Cross(kDiff0));
        Vector3<Real> kSubVel1 = fStep*(rkVel1 + rkRotAxis1.Cross(kDiff1));
        if ( TestIntersection(fStep,kSubBox0,kSubVel0,kSubBox1,kSubVel1) )
            return true;

        // update the box centers
        kSubBox0.Center() = kSubBox0.Center() + kSubVel0;
        kSubBox1.Center() = kSubBox1.Center() + kSubVel1;

        // update the box axes
        for (i = 0; i < 3; i++)
        {
            kSubBox0.Axis(i) = kSubBox0.Axis(i) +
                fStep*rkRotAxis0.Cross(kSubBox0.Axis(i));

            kSubBox1.Axis(i) = kSubBox1.Axis(i) +
                fStep*rkRotAxis1.Cross(kSubBox1.Axis(i));
        }

        // Use Gram-Schmidt to orthonormalize the updated axes.  NOTE:  If
        // T/N is small and N is small, you can remove this expensive step
        // with the assumption that the updated axes are nearly orthonormal.
        Vector3<Real>::Orthonormalize(kSubBox0.Axes());
        Vector3<Real>::Orthonormalize(kSubBox1.Axes());
    }

    // NOTE:  If the boxes do not intersect, then the application might
    // want to move/rotate the boxes to their new locations.  In this case
    // you want to return the final values of kSubBox0 and kSubBox1 so that
    // the application can set rkBox0 <- kSubBox0 and rkBox1 <- kSubBox1.
    // Otherwise, the application would have to solve the differential
    // equation again or compute the new box locations using the closed form
    // solution for the rigid motion.

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Box3<Real>& rkBox0,
    const Vector3<Real>& rkVel0, const Box3<Real>& rkBox1,
    const Vector3<Real>& rkVel1, Real& rfTFirst, Real fTMax, int& riQuantity,
    Vector3<Real>* akP)
{
    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    // relative velocity of box1 relative to box0
    Vector3<Real> kVel = rkVel1 - rkVel0;

    int i0, i1;
    ContactSide eSide = NONE;
    ContactConfig<Real> kUC, kVC;
    Vector3<Real> kAxis;

    // box 0 normals
    for (i0 = 0; i0 < 3; i0++)
    {
        kAxis = rkBox0.Axis(i0);
        if ( !BoxAxisFind(kVel,kAxis,rkBox0,rkBox1,rfTFirst,fTLast,fTMax,
             eSide,kUC,kVC) )
        {
            return false;
        }
    }

    // box 1 normals
    for (i1 = 0; i1 < 3; i1++)
    {
        kAxis = rkBox1.Axis(i1);
        if ( !BoxAxisFind(kVel,kAxis,rkBox0,rkBox1,rfTFirst,fTLast,fTMax,
             eSide,kUC,kVC) )
        {
            return false;
        }
    }

    // box 0 edges cross box 1 edges
    for (i0 = 0; i0 < 3; i0++)
    {
        for (i1 = 0; i1 < 3; i1++)
        {
            kAxis = rkBox0.Axis(i0).Cross(rkBox1.Axis(i1));

            // Since all axes are unit length (assumed), then can
            // just compare against a constant (not relative) epsilon
            if ( kAxis.SquaredLength() <= Math<Real>::EPSILON )
            {
                // Simple separation, axis i0 and i1 are parallel
                // If any two axes are parallel, then the only comparisons 
                // that need to be done are between the faces themselves, 
                // which have already been done.  Therefore, if they haven't 
                // been separated yet, nothing else will.  Quick out.
                return true;
            }

            if ( !BoxAxisFind(kVel,kAxis,rkBox0,rkBox1,rfTFirst,fTLast,
                 fTMax,eSide,kUC,kVC) )
            {
                return false;
            }
        }
    }

    // velocity cross box 0 edges
    for (i0 = 0; i0 < 3; i0++)
    {
        kAxis = kVel.Cross(rkBox0.Axis(i0));
        if ( !BoxAxisFind(kVel,kAxis,rkBox0,rkBox1,rfTFirst,fTLast,fTMax,
             eSide,kUC,kVC) )
        {
            return false;
        }
    }

    // velocity cross box 1 edges
    for (i1 = 0; i1 < 3; i1++)
    {
        kAxis = kVel.Cross(rkBox1.Axis(i1));
        if ( !BoxAxisFind(kVel,kAxis,rkBox0,rkBox1,rfTFirst,fTLast,fTMax,
             eSide,kUC,kVC) )
        {
            return false;
        }
    }

    if ( rfTFirst <= (Real)0.0 || eSide == NONE )
        return false;

    FindContactSet(rkBox0,rkBox1,eSide,kUC,kVC,rkVel0,rkVel1,rfTFirst,
        riQuantity,akP);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Box3<float>&,
    const Box3<float>&);
template WML_ITEM bool TestIntersection<float> (float,
    const Box3<float>&, const Vector3<float>&, const Box3<float>&,
    const Vector3<float>&);
template WML_ITEM bool TestIntersection<float> (float, int,
    const Box3<float>&, const Vector3<float>&, const Vector3<float>&,
    const Vector3<float>&, const Box3<float>&, const Vector3<float>&,
    const Vector3<float>&, const Vector3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Box3<float>&, const Vector3<float>&, const Box3<float>&,
    const Vector3<float>&, float&, float, int&, Vector3<float>*);

template WML_ITEM bool TestIntersection<double> (const Box3<double>&,
    const Box3<double>&);
template WML_ITEM bool TestIntersection<double> (double,
    const Box3<double>&, const Vector3<double>&, const Box3<double>&,
    const Vector3<double>&);
template WML_ITEM bool TestIntersection<double> (double, int,
    const Box3<double>&, const Vector3<double>&, const Vector3<double>&,
    const Vector3<double>&, const Box3<double>&, const Vector3<double>&,
    const Vector3<double>&, const Vector3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Box3<double>&, const Vector3<double>&, const Box3<double>&,
    const Vector3<double>&, double&, double, int&, Vector3<double>*);
}
//----------------------------------------------------------------------------
