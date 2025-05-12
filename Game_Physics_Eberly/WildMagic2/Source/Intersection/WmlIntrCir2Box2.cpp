// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrCir2Box2.h"
using namespace Wml;

//----------------------------------------------------------------------------
// All functions return -1 if initially intersecting, 0 if no intersection,
// +1 if intersects at some positive time.
//----------------------------------------------------------------------------
template <class Real>
static int TestVertexRegion (Real fCx, Real fCy, Real fR, Real fVx, Real fVy,
    Real fEx, Real fEy, Real& rfTFirst, Real& rfIx, Real& rfIy)
{
    Real fDx = fCx + fEx;
    Real fDy = fCy + fEy;
    Real fRSqr = fR*fR;
    Real fDiff = fDx*fDx + fDy*fDy - fRSqr;
    if ( fDiff <= (Real)0.0 )
    {
        // circle is already intersecting the box
        rfTFirst = (Real)0.0;
        return -1;
    }

    Real fDot = fVx*fDx + fVy*fDy;
    if ( fDot >= (Real)0.0 )
    {
        // circle not moving towards box
        return 0;
    }

    Real fKross = fVx*fDy - fVy*fDx;
    Real fVSqr, fInv;

    if ( fKross >= (Real)0.0 )
    {
        // potential contact on left edge
        if ( fKross <= fR*fVy )
        {
            // lower left corner is first point of contact
            rfIx = -fEx;
            rfIy = -fEy;
            fVSqr = fVx*fVx + fVy*fVy;
            fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(
                fDot*fDot-fVSqr*fDiff));
            rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
            return 1;
        }

        if ( fVx <= (Real)0.0 )
        {
            // passed corner, moving away from box
            return 0;
        }

        fVSqr = fVx*fVx + fVy*fVy;
        fDy = fCy - fEy;
        fKross = fVx*fDy - fVy*fDx;
        if ( fKross >= 0.0f && fKross*fKross > fRSqr*fVSqr )
        {
            // circle misses box
            return 0;
        }

        // Circle will intersect box.  Determine first time and place of
        // contact with x = xmin.
        rfIx = -fEx;

        if ( fKross <= fR*fVy )
        {
            // first contact on left edge of box
            rfTFirst = -(fDx+fR)/fVx;
            rfIy = fCy + rfTFirst*fVy;
        }
        else
        {
            // first contact at upper left corner of box
            fDot = fVx*fDx + fVy*fDy;
            fDiff = fDx*fDx + fDy*fDy - fRSqr;
            fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(
                fDot*fDot-fVSqr*fDiff));
            rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
            rfIy = fEy;
        }
    }
    else
    {
        // potential contact on bottom edge
        if ( -fKross <= fR*fVx )
        {
            // lower left corner is first point of contact
            rfIx = -fEx;
            rfIy = -fEy;
            fVSqr = fVx*fVx + fVy*fVy;
            fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(
                fDot*fDot-fVSqr*fDiff));
            rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
            return 1;
        }

        if ( fVy <= (Real)0.0 )
        {
            // passed corner, moving away from box
            return 0;
        }

        fVSqr = fVx*fVx + fVy*fVy;
        fDx = fCx - fEx;
        fKross = fVx*fDy - fVy*fDx;
        if ( -fKross >= (Real)0.0 && fKross*fKross > fRSqr*fVSqr )
        {
            // circle misses box
            return 0;
        }

        // Circle will intersect box.  Determine first time and place of
        // contact with y = ymin.
        rfIy = -fEy;

        if ( -fKross <= fR*fVx )
        {
            // first contact on bottom edge of box
            rfTFirst = -(fDy+fR)/fVy;
            rfIx = fCx + rfTFirst*fVx;
        }
        else
        {
            // first contact at lower right corner of box
            fDot = fVx*fDx + fVy*fDy;
            fDiff = fDx*fDx + fDy*fDy - fRSqr;
            fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(
                fDot*fDot-fVSqr*fDiff));
            rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
            rfIx = fEx;
        }
    }

    return 1;
}
//----------------------------------------------------------------------------
template <class Real>
static int TestEdgeRegion (Real fCx, Real fCy, Real fR, Real fVx, Real fVy,
    Real fEx, Real fEy, Real& rfTFirst, Real& rfIx, Real& rfIy)
{
    Real fDx = fCx + fEx;
    Real fXSignedDist = fDx + fR;
    if ( fXSignedDist >= (Real)0.0 )
    {
        // circle is already intersecting the box
        rfTFirst = (Real)0.0;
        return -1;
    }

    if ( fVx <= (Real)0.0 )
    {
        // circle not moving towards box
        return 0;
    }

    Real fRSqr = fR*fR;
    Real fVSqr = fVx*fVx + fVy*fVy;
    Real fDy, fDot, fKross, fDiff, fInv;

    if ( fVy >= (Real)0.0 )
    {
        fDy = fCy - fEy;
        fKross = fVx*fDy - fVy*fDx;
        if ( fKross >= (Real)0.0 && fKross*fKross > fRSqr*fVSqr )
        {
            // circle misses box
            return 0;
        }

        // Circle will intersect box.  Determine first time and place of
        // contact with x = xmin.
        rfIx = -fEx;

        if ( fKross <= fR*fVy )
        {
            // first contact on left edge of box
            rfTFirst = -fXSignedDist/fVx;
            rfIy = fCy + rfTFirst*fVy;
        }
        else
        {
            // first contact at corner of box
            fDot = fVx*fDx + fVy*fDy;
            fDiff = fDx*fDx + fDy*fDy - fRSqr;
            fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(
                fDot*fDot-fVSqr*fDiff));
            rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
            rfIy = fEy;
        }
    }
    else
    {
        fDy = fCy + fEy;
        fKross = fVx*fDy - fVy*fDx;
        if ( fKross <= (Real)0.0 && fKross*fKross > fRSqr*fVSqr )
        {
            // circle misses box
            return 0;
        }

        // Circle will intersect box.  Determine first time and place of
        // contact with x = xmin.
        rfIx = -fEx;

        if ( fKross >= fR*fVy )
        {
            // first contact on left edge of box
            rfTFirst = -fXSignedDist/fVx;
            rfIy = fCy + rfTFirst*fVy;
        }
        else
        {
            // first contact at corner of box
            fDot = fVx*fDx + fVy*fDy;
            fDiff = fDx*fDx + fDy*fDy - fRSqr;
            fInv = Math<Real>::InvSqrt(Math<Real>::FAbs(
                fDot*fDot-fVSqr*fDiff));
            rfTFirst = fDiff*fInv/((Real)1.0-fDot*fInv);
            rfIy = -fEy;
        }
    }

    return 1;
}
//----------------------------------------------------------------------------
template <class Real>
int Wml::FindIntersection (const Circle2<Real>& rkCircle,
    const Vector2<Real>& rkV, const Box2<Real>& rkBox, Real& rfTFirst,
    Vector2<Real>& rkIntr)
{
    // convert circle center to box coordinates
    Vector2<Real> kDiff = rkCircle.Center() - rkBox.Center();
    Real fR = rkCircle.Radius();
    Real fCx = kDiff.Dot(rkBox.Axis(0));
    Real fCy = kDiff.Dot(rkBox.Axis(1));
    Real fVx = rkV.Dot(rkBox.Axis(0));
    Real fVy = rkV.Dot(rkBox.Axis(1));
    Real fEx = rkBox.Extent(0);
    Real fEy = rkBox.Extent(1);
    Real fIx, fIy;

    int iType = 0;

    if ( fCx < -fEx )
    {
        if ( fCy < -fEy )
        {
            // region Rmm
            iType = TestVertexRegion(fCx,fCy,fR,fVx,fVy,fEx,fEy,
                rfTFirst,fIx,fIy);
        }
        else if ( fCy <= fEy )
        {
            // region Rmz
            iType = TestEdgeRegion(fCx,fCy,fR,fVx,fVy,fEx,fEy,
                rfTFirst,fIx,fIy);
        }
        else
        {
            // region Rmp
            iType = TestVertexRegion(fCx,-fCy,fR,fVx,-fVy,fEx,fEy,
                rfTFirst,fIx,fIy);
            fIy = -fIy;
        }
    }
    else if ( fCx <= fEx )
    {
        if ( fCy < -fEy )
        {
            // region Rzm
            iType = TestEdgeRegion(fCy,fCx,fR,fVy,fVx,fEy,fEx,
                rfTFirst,fIy,fIx);
        }
        else if ( fCy <= fEy )
        {
            // region Rzz: circle is already intersecting the box
            rfTFirst = 0.0f;
            return -1;
        }
        else
        {
            // region Rzp
            iType = TestEdgeRegion(-fCy,fCx,fR,-fVy,fVx,fEy,fEx,
                rfTFirst,fIy,fIx);
            fIy = -fIy;
        }
    }
    else
    {
        if ( fCy < -fEy )
        {
            // region Rpm
            iType = TestVertexRegion(-fCx,fCy,fR,-fVx,fVy,fEx,fEy,
                rfTFirst,fIx,fIy);
            fIx = -fIx;
        }
        else if ( fCy <= fEy )
        {
            // region Rpz
            iType = TestEdgeRegion(-fCx,fCy,fR,-fVx,fVy,fEx,fEy,
                rfTFirst,fIx,fIy);
            fIx = -fIx;
        }
        else
        {
            // region Rpp
            iType = TestVertexRegion(-fCx,-fCy,fR,-fVx,-fVy,fEx,fEy,
                rfTFirst,fIx,fIy);
            fIx = -fIx;
            fIy = -fIy;
        }
    }

    if ( iType == 1 )
        rkIntr = rkBox.Center() + fIx*rkBox.Axis(0) + fIy*rkBox.Axis(1);

    return iType;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM int FindIntersection<float> (const Circle2<float>&,
    const Vector2<float>&, const Box2<float>&, float&, Vector2<float>&);

template WML_ITEM int FindIntersection<double> (const Circle2<double>&,
    const Vector2<double>&, const Box2<double>&, double&, Vector2<double>&);
}
//----------------------------------------------------------------------------
