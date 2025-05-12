// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistLin3Box3.h"
#include "WmlDistVec3Box3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static void Face (int i0, int i1, int i2, Vector3<Real>& rkPnt,
    const Vector3<Real>& rkDir, const Box3<Real>& rkBox,
    const Vector3<Real>& rkPmE, Real* pfLParam, Real& rfSqrDistance)
{
    Vector3<Real> kPpE;
    Real fLSqr, fInv, fTmp, fParam, fT, fDelta;

    kPpE[i1] = rkPnt[i1] + rkBox.Extent(i1);
    kPpE[i2] = rkPnt[i2] + rkBox.Extent(i2);
    if ( rkDir[i0]*kPpE[i1] >= rkDir[i1]*rkPmE[i0] )
    {
        if ( rkDir[i0]*kPpE[i2] >= rkDir[i2]*rkPmE[i0] )
        {
            // v[i1] >= -e[i1], v[i2] >= -e[i2] (distance = 0)
            if ( pfLParam )
            {
                rkPnt[i0] = rkBox.Extent(i0);
                fInv = ((Real)1.0)/rkDir[i0];
                rkPnt[i1] -= rkDir[i1]*rkPmE[i0]*fInv;
                rkPnt[i2] -= rkDir[i2]*rkPmE[i0]*fInv;
                *pfLParam = -rkPmE[i0]*fInv;
            }
        }
        else
        {
            // v[i1] >= -e[i1], v[i2] < -e[i2]
            fLSqr = rkDir[i0]*rkDir[i0] + rkDir[i2]*rkDir[i2];
            fTmp = fLSqr*kPpE[i1] - rkDir[i1]*(rkDir[i0]*rkPmE[i0] +
                rkDir[i2]*kPpE[i2]);
            if ( fTmp <= ((Real)2.0)*fLSqr*rkBox.Extent(i1) )
            {
                fT = fTmp/fLSqr;
                fLSqr += rkDir[i1]*rkDir[i1];
                fTmp = kPpE[i1] - fT;
                fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*fTmp +
                    rkDir[i2]*kPpE[i2];
                fParam = -fDelta/fLSqr;
                rfSqrDistance += rkPmE[i0]*rkPmE[i0] + fTmp*fTmp +
                    kPpE[i2]*kPpE[i2] + fDelta*fParam;

                if ( pfLParam )
                {
                    *pfLParam = fParam;
                    rkPnt[i0] = rkBox.Extent(i0);
                    rkPnt[i1] = fT - rkBox.Extent(i1);
                    rkPnt[i2] = -rkBox.Extent(i2);
                }
            }
            else
            {
                fLSqr += rkDir[i1]*rkDir[i1];
                fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*rkPmE[i1] +
                    rkDir[i2]*kPpE[i2];
                fParam = -fDelta/fLSqr;
                rfSqrDistance += rkPmE[i0]*rkPmE[i0] + rkPmE[i1]*rkPmE[i1] +
                    kPpE[i2]*kPpE[i2] + fDelta*fParam;

                if ( pfLParam )
                {
                    *pfLParam = fParam;
                    rkPnt[i0] = rkBox.Extent(i0);
                    rkPnt[i1] = rkBox.Extent(i1);
                    rkPnt[i2] = -rkBox.Extent(i2);
                }
            }
        }
    }
    else
    {
        if ( rkDir[i0]*kPpE[i2] >= rkDir[i2]*rkPmE[i0] )
        {
            // v[i1] < -e[i1], v[i2] >= -e[i2]
            fLSqr = rkDir[i0]*rkDir[i0] + rkDir[i1]*rkDir[i1];
            fTmp = fLSqr*kPpE[i2] - rkDir[i2]*(rkDir[i0]*rkPmE[i0] +
                rkDir[i1]*kPpE[i1]);
            if ( fTmp <= ((Real)2.0)*fLSqr*rkBox.Extent(i2) )
            {
                fT = fTmp/fLSqr;
                fLSqr += rkDir[i2]*rkDir[i2];
                fTmp = kPpE[i2] - fT;
                fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*kPpE[i1] +
                    rkDir[i2]*fTmp;
                fParam = -fDelta/fLSqr;
                rfSqrDistance += rkPmE[i0]*rkPmE[i0] + kPpE[i1]*kPpE[i1] +
                    fTmp*fTmp + fDelta*fParam;

                if ( pfLParam )
                {
                    *pfLParam = fParam;
                    rkPnt[i0] = rkBox.Extent(i0);
                    rkPnt[i1] = -rkBox.Extent(i1);
                    rkPnt[i2] = fT - rkBox.Extent(i2);
                }
            }
            else
            {
                fLSqr += rkDir[i2]*rkDir[i2];
                fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*kPpE[i1] +
                    rkDir[i2]*rkPmE[i2];
                fParam = -fDelta/fLSqr;
                rfSqrDistance += rkPmE[i0]*rkPmE[i0] + kPpE[i1]*kPpE[i1] +
                    rkPmE[i2]*rkPmE[i2] + fDelta*fParam;

                if ( pfLParam )
                {
                    *pfLParam = fParam;
                    rkPnt[i0] = rkBox.Extent(i0);
                    rkPnt[i1] = -rkBox.Extent(i1);
                    rkPnt[i2] = rkBox.Extent(i2);
                }
            }
        }
        else
        {
            // v[i1] < -e[i1], v[i2] < -e[i2]
            fLSqr = rkDir[i0]*rkDir[i0]+rkDir[i2]*rkDir[i2];
            fTmp = fLSqr*kPpE[i1] - rkDir[i1]*(rkDir[i0]*rkPmE[i0] +
                rkDir[i2]*kPpE[i2]);
            if ( fTmp >= (Real)0.0 )
            {
                // v[i1]-edge is closest
                if ( fTmp <= ((Real)2.0)*fLSqr*rkBox.Extent(i1) )
                {
                    fT = fTmp/fLSqr;
                    fLSqr += rkDir[i1]*rkDir[i1];
                    fTmp = kPpE[i1] - fT;
                    fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*fTmp +
                        rkDir[i2]*kPpE[i2];
                    fParam = -fDelta/fLSqr;
                    rfSqrDistance += rkPmE[i0]*rkPmE[i0] + fTmp*fTmp +
                        kPpE[i2]*kPpE[i2] + fDelta*fParam;

                    if ( pfLParam )
                    {
                        *pfLParam = fParam;
                        rkPnt[i0] = rkBox.Extent(i0);
                        rkPnt[i1] = fT - rkBox.Extent(i1);
                        rkPnt[i2] = -rkBox.Extent(i2);
                    }
                }
                else
                {
                    fLSqr += rkDir[i1]*rkDir[i1];
                    fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*rkPmE[i1] +
                        rkDir[i2]*kPpE[i2];
                    fParam = -fDelta/fLSqr;
                    rfSqrDistance += rkPmE[i0]*rkPmE[i0] + rkPmE[i1]*rkPmE[i1]
                        + kPpE[i2]*kPpE[i2] + fDelta*fParam;

                    if ( pfLParam )
                    {
                        *pfLParam = fParam;
                        rkPnt[i0] = rkBox.Extent(i0);
                        rkPnt[i1] = rkBox.Extent(i1);
                        rkPnt[i2] = -rkBox.Extent(i2);
                    }
                }
                return;
            }

            fLSqr = rkDir[i0]*rkDir[i0] + rkDir[i1]*rkDir[i1];
            fTmp = fLSqr*kPpE[i2] - rkDir[i2]*(rkDir[i0]*rkPmE[i0] +
                rkDir[i1]*kPpE[i1]);
            if ( fTmp >= (Real)0.0 )
            {
                // v[i2]-edge is closest
                if ( fTmp <= ((Real)2.0)*fLSqr*rkBox.Extent(i2) )
                {
                    fT = fTmp/fLSqr;
                    fLSqr += rkDir[i2]*rkDir[i2];
                    fTmp = kPpE[i2] - fT;
                    fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*kPpE[i1] +
                        rkDir[i2]*fTmp;
                    fParam = -fDelta/fLSqr;
                    rfSqrDistance += rkPmE[i0]*rkPmE[i0] + kPpE[i1]*kPpE[i1] +
                        fTmp*fTmp + fDelta*fParam;

                    if ( pfLParam )
                    {
                        *pfLParam = fParam;
                        rkPnt[i0] = rkBox.Extent(i0);
                        rkPnt[i1] = -rkBox.Extent(i1);
                        rkPnt[i2] = fT - rkBox.Extent(i2);
                    }
                }
                else
                {
                    fLSqr += rkDir[i2]*rkDir[i2];
                    fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*kPpE[i1] +
                        rkDir[i2]*rkPmE[i2];
                    fParam = -fDelta/fLSqr;
                    rfSqrDistance += rkPmE[i0]*rkPmE[i0] + kPpE[i1]*kPpE[i1] +
                        rkPmE[i2]*rkPmE[i2] + fDelta*fParam;

                    if ( pfLParam )
                    {
                        *pfLParam = fParam;
                        rkPnt[i0] = rkBox.Extent(i0);
                        rkPnt[i1] = -rkBox.Extent(i1);
                        rkPnt[i2] = rkBox.Extent(i2);
                    }
                }
                return;
            }

            // (v[i1],v[i2])-corner is closest
            fLSqr += rkDir[i2]*rkDir[i2];
            fDelta = rkDir[i0]*rkPmE[i0] + rkDir[i1]*kPpE[i1] +
                rkDir[i2]*kPpE[i2];
            fParam = -fDelta/fLSqr;
            rfSqrDistance += rkPmE[i0]*rkPmE[i0] + kPpE[i1]*kPpE[i1] +
                kPpE[i2]*kPpE[i2] + fDelta*fParam;

            if ( pfLParam )
            {
                *pfLParam = fParam;
                rkPnt[i0] = rkBox.Extent(i0);
                rkPnt[i1] = -rkBox.Extent(i1);
                rkPnt[i2] = -rkBox.Extent(i2);
            }
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
static void CaseNoZeros (Vector3<Real>& rkPnt, const Vector3<Real>& rkDir,
    const Box3<Real>& rkBox, Real* pfLParam, Real& rfSqrDistance)
{
    Vector3<Real> kPmE(
        rkPnt.X() - rkBox.Extent(0),
        rkPnt.Y() - rkBox.Extent(1),
        rkPnt.Z() - rkBox.Extent(2));

    Real fProdDxPy = rkDir.X()*kPmE.Y();
    Real fProdDyPx = rkDir.Y()*kPmE.X();
    Real fProdDzPx, fProdDxPz, fProdDzPy, fProdDyPz;

    if ( fProdDyPx >= fProdDxPy )
    {
        fProdDzPx = rkDir.Z()*kPmE.X();
        fProdDxPz = rkDir.X()*kPmE.Z();
        if ( fProdDzPx >= fProdDxPz )
        {
            // line intersects x = e0
            Face(0,1,2,rkPnt,rkDir,rkBox,kPmE,pfLParam,rfSqrDistance);
        }
        else
        {
            // line intersects z = e2
            Face(2,0,1,rkPnt,rkDir,rkBox,kPmE,pfLParam,rfSqrDistance);
        }
    }
    else
    {
        fProdDzPy = rkDir.Z()*kPmE.Y();
        fProdDyPz = rkDir.Y()*kPmE.Z();
        if ( fProdDzPy >= fProdDyPz )
        {
            // line intersects y = e1
            Face(1,2,0,rkPnt,rkDir,rkBox,kPmE,pfLParam,rfSqrDistance);
        }
        else
        {
            // line intersects z = e2
            Face(2,0,1,rkPnt,rkDir,rkBox,kPmE,pfLParam,rfSqrDistance);
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
static void Case0 (int i0, int i1, int i2, Vector3<Real>& rkPnt,
    const Vector3<Real>& rkDir, const Box3<Real>& rkBox, Real* pfLParam,
    Real& rfSqrDistance)
{
    Real fPmE0 = rkPnt[i0] - rkBox.Extent(i0);
    Real fPmE1 = rkPnt[i1] - rkBox.Extent(i1);
    Real fProd0 = rkDir[i1]*fPmE0;
    Real fProd1 = rkDir[i0]*fPmE1;
    Real fDelta, fInvLSqr, fInv;

    if ( fProd0 >= fProd1 )
    {
        // line intersects P[i0] = e[i0]
        rkPnt[i0] = rkBox.Extent(i0);

        Real fPpE1 = rkPnt[i1] + rkBox.Extent(i1);
        fDelta = fProd0 - rkDir[i0]*fPpE1;
        if ( fDelta >= (Real)0.0 )
        {
            fInvLSqr = ((Real)1.0)/(rkDir[i0]*rkDir[i0]+rkDir[i1]*rkDir[i1]);
            rfSqrDistance += fDelta*fDelta*fInvLSqr;
            if ( pfLParam )
            {
                rkPnt[i1] = -rkBox.Extent(i1);
                *pfLParam = -(rkDir[i0]*fPmE0+rkDir[i1]*fPpE1)*fInvLSqr;
            }
        }
        else
        {
            if ( pfLParam )
            {
                fInv = ((Real)1.0)/rkDir[i0];
                rkPnt[i1] -= fProd0*fInv;
                *pfLParam = -fPmE0*fInv;
            }
        }
    }
    else
    {
        // line intersects P[i1] = e[i1]
        rkPnt[i1] = rkBox.Extent(i1);

        Real fPpE0 = rkPnt[i0] + rkBox.Extent(i0);
        fDelta = fProd1 - rkDir[i1]*fPpE0;
        if ( fDelta >= (Real)0.0 )
        {
            fInvLSqr = ((Real)1.0)/(rkDir[i0]*rkDir[i0]+rkDir[i1]*rkDir[i1]);
            rfSqrDistance += fDelta*fDelta*fInvLSqr;
            if ( pfLParam )
            {
                rkPnt[i0] = -rkBox.Extent(i0);
                *pfLParam = -(rkDir[i0]*fPpE0+rkDir[i1]*fPmE1)*fInvLSqr;
            }
        }
        else
        {
            if ( pfLParam )
            {
                fInv = ((Real)1.0)/rkDir[i1];
                rkPnt[i0] -= fProd1*fInv;
                *pfLParam = -fPmE1*fInv;
            }
        }
    }

    if ( rkPnt[i2] < -rkBox.Extent(i2) )
    {
        fDelta = rkPnt[i2] + rkBox.Extent(i2);
        rfSqrDistance += fDelta*fDelta;
        rkPnt[i2] = -rkBox.Extent(i2);
    }
    else if ( rkPnt[i2] > rkBox.Extent(i2) )
    {
        fDelta = rkPnt[i2] - rkBox.Extent(i2);
        rfSqrDistance += fDelta*fDelta;
        rkPnt[i2] = rkBox.Extent(i2);
    }
}
//----------------------------------------------------------------------------
template <class Real>
static void Case00 (int i0, int i1, int i2, Vector3<Real>& rkPnt,
    const Vector3<Real>& rkDir, const Box3<Real>& rkBox, Real* pfLParam,
    Real& rfSqrDistance)
{
    Real fDelta;

    if ( pfLParam )
        *pfLParam = (rkBox.Extent(i0) - rkPnt[i0])/rkDir[i0];

    rkPnt[i0] = rkBox.Extent(i0);

    if ( rkPnt[i1] < -rkBox.Extent(i1) )
    {
        fDelta = rkPnt[i1] + rkBox.Extent(i1);
        rfSqrDistance += fDelta*fDelta;
        rkPnt[i1] = -rkBox.Extent(i1);
    }
    else if ( rkPnt[i1] > rkBox.Extent(i1) )
    {
        fDelta = rkPnt[i1] - rkBox.Extent(i1);
        rfSqrDistance += fDelta*fDelta;
        rkPnt[i1] = rkBox.Extent(i1);
    }

    if ( rkPnt[i2] < -rkBox.Extent(i2) )
    {
        fDelta = rkPnt[i2] + rkBox.Extent(i2);
        rfSqrDistance += fDelta*fDelta;
        rkPnt[i1] = -rkBox.Extent(i2);
    }
    else if ( rkPnt[i2] > rkBox.Extent(i2) )
    {
        fDelta = rkPnt[i2] - rkBox.Extent(i2);
        rfSqrDistance += fDelta*fDelta;
        rkPnt[i2] = rkBox.Extent(i2);
    }
}
//----------------------------------------------------------------------------
template <class Real>
static void Case000 (Vector3<Real>& rkPnt, const Box3<Real>& rkBox,
    Real& rfSqrDistance)
{
    Real fDelta;

    if ( rkPnt.X() < -rkBox.Extent(0) )
    {
        fDelta = rkPnt.X() + rkBox.Extent(0);
        rfSqrDistance += fDelta*fDelta;
        rkPnt.X() = -rkBox.Extent(0);
    }
    else if ( rkPnt.X() > rkBox.Extent(0) )
    {
        fDelta = rkPnt.X() - rkBox.Extent(0);
        rfSqrDistance += fDelta*fDelta;
        rkPnt.X() = rkBox.Extent(0);
    }

    if ( rkPnt.Y() < -rkBox.Extent(1) )
    {
        fDelta = rkPnt.Y() + rkBox.Extent(1);
        rfSqrDistance += fDelta*fDelta;
        rkPnt.Y() = -rkBox.Extent(1);
    }
    else if ( rkPnt.Y() > rkBox.Extent(1) )
    {
        fDelta = rkPnt.Y() - rkBox.Extent(1);
        rfSqrDistance += fDelta*fDelta;
        rkPnt.Y() = rkBox.Extent(1);
    }

    if ( rkPnt.Z() < -rkBox.Extent(2) )
    {
        fDelta = rkPnt.Z() + rkBox.Extent(2);
        rfSqrDistance += fDelta*fDelta;
        rkPnt.Z() = -rkBox.Extent(2);
    }
    else if ( rkPnt.Z() > rkBox.Extent(2) )
    {
        fDelta = rkPnt.Z() - rkBox.Extent(2);
        rfSqrDistance += fDelta*fDelta;
        rkPnt.Z() = rkBox.Extent(2);
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Line3<Real>& rkLine, const Box3<Real>& rkBox,
    Real* pfLParam, Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    // compute coordinates of line in box coordinate system
    Vector3<Real> kDiff = rkLine.Origin() - rkBox.Center();
    Vector3<Real> kPnt(kDiff.Dot(rkBox.Axis(0)),kDiff.Dot(rkBox.Axis(1)),
        kDiff.Dot(rkBox.Axis(2)));
    Vector3<Real> kDir(rkLine.Direction().Dot(rkBox.Axis(0)),
        rkLine.Direction().Dot(rkBox.Axis(1)),
        rkLine.Direction().Dot(rkBox.Axis(2)));

    // Apply reflections so that direction vector has nonnegative components.
    bool bReflect[3];
    int i;
    for (i = 0; i < 3; i++)
    {
        if ( kDir[i] < (Real)0.0 )
        {
            kPnt[i] = -kPnt[i];
            kDir[i] = -kDir[i];
            bReflect[i] = true;
        }
        else
        {
            bReflect[i] = false;
        }
    }

    Real fSqrDistance = (Real)0.0;

    if ( kDir.X() > (Real)0.0 )
    {
        if ( kDir.Y() > (Real)0.0 )
        {
            if ( kDir.Z() > (Real)0.0 )
            {
                // (+,+,+)
                CaseNoZeros(kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
            else
            {
                // (+,+,0)
                Case0(0,1,2,kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
        }
        else
        {
            if ( kDir.Z() > (Real)0.0 )
            {
                // (+,0,+)
                Case0(0,2,1,kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
            else
            {
                // (+,0,0)
                Case00(0,1,2,kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
        }
    }
    else
    {
        if ( kDir.Y() > (Real)0.0 )
        {
            if ( kDir.Z() > (Real)0.0 )
            {
                // (0,+,+)
                Case0(1,2,0,kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
            else
            {
                // (0,+,0)
                Case00(1,0,2,kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
        }
        else
        {
            if ( kDir.Z() > (Real)0.0 )
            {
                // (0,0,+)
                Case00(2,0,1,kPnt,kDir,rkBox,pfLParam,fSqrDistance);
            }
            else
            {
                // (0,0,0)
                Case000(kPnt,rkBox,fSqrDistance);
                if ( pfLParam )
                    *pfLParam = (Real)0.0;
            }
        }
    }

    // undo reflections
    for (i = 0; i < 3; i++)
    {
        if ( bReflect[i] )
            kPnt[i] = -kPnt[i];
    }

    if ( pfBParam0 )
        *pfBParam0 = kPnt.X();

    if ( pfBParam1 )
        *pfBParam1 = kPnt.Y();

    if ( pfBParam2 )
        *pfBParam2 = kPnt.Z();

    return fSqrDistance;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Ray3<Real>& rkRay, const Box3<Real>& rkBox,
    Real* pfLParam, Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    Line3<Real> kLine;
    kLine.Origin() = rkRay.Origin();
    kLine.Direction() = rkRay.Direction();

    Real fLP, fBP0, fBP1, fBP2;
    Real fSqrDistance = SqrDistance(kLine,rkBox,&fLP,&fBP0,&fBP1,&fBP2);
    if ( fLP >= (Real)0.0 )
    {
        if ( pfLParam )
            *pfLParam = fLP;

        if ( pfBParam0 )
            *pfBParam0 = fBP0;

        if ( pfBParam1 )
            *pfBParam1 = fBP1;

        if ( pfBParam2 )
            *pfBParam2 = fBP2;

        return fSqrDistance;
    }
    else
    {
        fSqrDistance = SqrDistance(rkRay.Origin(),rkBox,pfBParam0,
            pfBParam1,pfBParam2);

        if ( pfLParam )
            *pfLParam = (Real)0.0;

        return fSqrDistance;
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Segment3<Real>& rkSeg, const Box3<Real>& rkBox,
    Real* pfLParam, Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    Line3<Real> kLine;
    kLine.Origin() = rkSeg.Origin();
    kLine.Direction() = rkSeg.Direction();

    Real fLP, fBP0, fBP1, fBP2;
    Real fSqrDistance = SqrDistance(kLine,rkBox,&fLP,&fBP0,&fBP1,&fBP2);
    if ( fLP >= (Real)0.0 )
    {
        if ( fLP <= (Real)1.0 )
        {
            if ( pfLParam )
                *pfLParam = fLP;

            if ( pfBParam0 )
                *pfBParam0 = fBP0;

            if ( pfBParam1 )
                *pfBParam1 = fBP1;

            if ( pfBParam2 )
                *pfBParam2 = fBP2;

            return fSqrDistance;
        }
        else
        {
            fSqrDistance = SqrDistance<Real>(rkSeg.Origin()+rkSeg.Direction(),
                rkBox,pfBParam0,pfBParam1,pfBParam2);

            if ( pfLParam )
                *pfLParam = (Real)1.0;

            return fSqrDistance;
        }
    }
    else
    {
        fSqrDistance = SqrDistance(rkSeg.Origin(),rkBox,pfBParam0,
            pfBParam1,pfBParam2);

        if ( pfLParam )
            *pfLParam = (Real)0.0;

        return fSqrDistance;
    }
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Line3<Real>& rkLine, const Box3<Real>& rkBox,
    Real* pfLParam, Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    return Math<Real>::Sqrt(SqrDistance(rkLine,rkBox,pfLParam,pfBParam0,
        pfBParam1,pfBParam2));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Ray3<Real>& rkRay, const Box3<Real>& rkBox,
    Real* pfLParam, Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    return Math<Real>::Sqrt(SqrDistance(rkRay,rkBox,pfLParam,pfBParam0,
        pfBParam1,pfBParam2));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Segment3<Real>& rkSeg, const Box3<Real>& rkBox,
    Real* pfLParam, Real* pfBParam0, Real* pfBParam1, Real* pfBParam2)
{
    return Math<Real>::Sqrt(SqrDistance(rkSeg,rkBox,pfLParam,pfBParam0,
        pfBParam1,pfBParam2));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Line3<float>&,
    const Box3<float>&, float*, float*, float*, float*);
template WML_ITEM float SqrDistance<float> (const Ray3<float>&,
    const Box3<float>&, float*, float*, float*, float*);
template WML_ITEM float SqrDistance<float> (const Segment3<float>&,
    const Box3<float>&, float*, float*, float*, float*);
template WML_ITEM float Distance<float> (const Line3<float>&,
    const Box3<float>&, float*, float*, float*, float*);
template WML_ITEM float Distance<float> (const Ray3<float>&,
    const Box3<float>&, float*, float*, float*, float*);
template WML_ITEM float Distance<float> (const Segment3<float>&,
    const Box3<float>&, float*, float*, float*, float*);

template WML_ITEM double SqrDistance<double> (const Line3<double>&,
    const Box3<double>&, double*, double*, double*, double*);
template WML_ITEM double SqrDistance<double> (const Ray3<double>&,
    const Box3<double>&, double*, double*, double*, double*);
template WML_ITEM double SqrDistance<double> (const Segment3<double>&,
    const Box3<double>&, double*, double*, double*, double*);
template WML_ITEM double Distance<double> (const Line3<double>&,
    const Box3<double>&, double*, double*, double*, double*);
template WML_ITEM double Distance<double> (const Ray3<double>&,
    const Box3<double>&, double*, double*, double*, double*);
template WML_ITEM double Distance<double> (const Segment3<double>&,
    const Box3<double>&, double*, double*, double*, double*);
}
//----------------------------------------------------------------------------
