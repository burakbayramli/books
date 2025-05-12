// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin2Box2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment2<Real>& rkSegment,
    const Box2<Real>& rkBox)
{
    Real fAWdU[2], fADdU[2], fRhs;
    Vector2<Real> kSDir = ((Real)0.5)*rkSegment.Direction();
    Vector2<Real> kSCen = rkSegment.Origin() + kSDir;
    Vector2<Real> kDiff = kSCen - rkBox.Center();

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

    Vector2<Real> kPerp = rkSegment.Direction().Perp();
    Real fLhs = Math<Real>::FAbs(kPerp.Dot(kDiff));
    Real fPart0 = Math<Real>::FAbs(kPerp.Dot(rkBox.Axis(0)));
    Real fPart1 = Math<Real>::FAbs(kPerp.Dot(rkBox.Axis(1)));
    fRhs = rkBox.Extent(0)*fPart0 + rkBox.Extent(1)*fPart1;
    return fLhs <= fRhs;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray2<Real>& rkRay, const Box2<Real>& rkBox)
{
    Real fWdU[2], fAWdU[2], fDdU[2], fADdU[2], fRhs;

    Vector2<Real> kDiff = rkRay.Origin() - rkBox.Center();

    fWdU[0] = rkRay.Direction().Dot(rkBox.Axis(0));
    fAWdU[0] = Math<Real>::FAbs(fWdU[0]);
    fDdU[0] = kDiff.Dot(rkBox.Axis(0));
    fADdU[0] = Math<Real>::FAbs(fDdU[0]);
    if ( fADdU[0] > rkBox.Extent(0) && fDdU[0]*fWdU[0] >= 0.0f )
        return false;

    fWdU[1] = rkRay.Direction().Dot(rkBox.Axis(1));
    fAWdU[1] = Math<Real>::FAbs(fWdU[1]);
    fDdU[1] = kDiff.Dot(rkBox.Axis(1));
    fADdU[1] = Math<Real>::FAbs(fDdU[1]);
    if ( fADdU[1] > rkBox.Extent(1) && fDdU[1]*fWdU[1] >= 0.0f )
        return false;

    Vector2<Real> kPerp = rkRay.Direction().Perp();
    Real fLhs = Math<Real>::FAbs(kPerp.Dot(kDiff));
    Real fPart0 = Math<Real>::FAbs(kPerp.Dot(rkBox.Axis(0)));
    Real fPart1 = Math<Real>::FAbs(kPerp.Dot(rkBox.Axis(1)));
    fRhs = rkBox.Extent(0)*fPart0 + rkBox.Extent(1)*fPart1;
    return fLhs <= fRhs;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line2<Real>& rkLine,
    const Box2<Real>& rkBox)
{
    Vector2<Real> kDiff = rkLine.Origin() - rkBox.Center();
    Vector2<Real> kPerp = rkLine.Direction().Perp();
    Real fLhs = Math<Real>::FAbs(kPerp.Dot(kDiff));
    Real fPart0 = Math<Real>::FAbs(kPerp.Dot(rkBox.Axis(0)));
    Real fPart1 = Math<Real>::FAbs(kPerp.Dot(rkBox.Axis(1)));
    Real fRhs = rkBox.Extent(0)*fPart0 + rkBox.Extent(1)*fPart1;
    return fLhs <= fRhs;
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
bool Wml::FindIntersection (const Vector2<Real>& rkOrigin,
    const Vector2<Real>& rkDirection, const Real afExtent[3], Real& rfT0,
    Real& rfT1)
{
    Real fSaveT0 = rfT0, fSaveT1 = rfT1;

    bool bNotEntirelyClipped =
        Clip(+rkDirection.X(),-rkOrigin.X()-afExtent[0],rfT0,rfT1) &&
        Clip(-rkDirection.X(),+rkOrigin.X()-afExtent[0],rfT0,rfT1) &&
        Clip(+rkDirection.Y(),-rkOrigin.Y()-afExtent[1],rfT0,rfT1) &&
        Clip(-rkDirection.Y(),+rkOrigin.Y()-afExtent[1],rfT0,rfT1);

    return bNotEntirelyClipped && ( rfT0 != fSaveT0 || rfT1 != fSaveT1 );
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment2<Real>& rkSegment,
    const Box2<Real>& rkBox, int& riQuantity, Vector2<Real> akPoint[2])
{
    // convert segment to box coordinates
    Vector2<Real> kDiff = rkSegment.Origin() - rkBox.Center();
    Vector2<Real> kOrigin(
        kDiff.Dot(rkBox.Axis(0)),
        kDiff.Dot(rkBox.Axis(1))
    );
    Vector2<Real> kDirection(
        rkSegment.Direction().Dot(rkBox.Axis(0)),
        rkSegment.Direction().Dot(rkBox.Axis(1))
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
bool Wml::FindIntersection (const Ray2<Real>& rkRay, const Box2<Real>& rkBox,
    int& riQuantity, Vector2<Real> akPoint[2])
{
    // convert ray to box coordinates
    Vector2<Real> kDiff = rkRay.Origin() - rkBox.Center();
    Vector2<Real> kOrigin(
        kDiff.Dot(rkBox.Axis(0)),
        kDiff.Dot(rkBox.Axis(1))
    );
    Vector2<Real> kDirection(
        rkRay.Direction().Dot(rkBox.Axis(0)),
        rkRay.Direction().Dot(rkBox.Axis(1))
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
bool Wml::FindIntersection (const Line2<Real>& rkLine,
    const Box2<Real>& rkBox, int& riQuantity, Vector2<Real> akPoint[2])
{
    // convert line to box coordinates
    Vector2<Real> kDiff = rkLine.Origin() - rkBox.Center();
    Vector2<Real> kOrigin(
        kDiff.Dot(rkBox.Axis(0)),
        kDiff.Dot(rkBox.Axis(1))
    );
    Vector2<Real> kDirection(
        rkLine.Direction().Dot(rkBox.Axis(0)),
        rkLine.Direction().Dot(rkBox.Axis(1))
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

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Segment2<float>&,
    const Box2<float>&);
template WML_ITEM bool TestIntersection<float> (const Ray2<float>&,
    const Box2<float>&);
template WML_ITEM bool TestIntersection<float> (const Line2<float>&,
    const Box2<float>&);
template WML_ITEM bool FindIntersection<float> (const Vector2<float>&,
    const Vector2<float>&, const float[2], float&, float&);
template WML_ITEM bool FindIntersection<float> (const Segment2<float>&,
    const Box2<float>&, int&, Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float> (const Ray2<float>&,
    const Box2<float>&, int&, Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float> (const Line2<float>&,
    const Box2<float>&, int&, Vector2<float>[2]);

template WML_ITEM bool TestIntersection<double> (const Segment2<double>&,
    const Box2<double>&);
template WML_ITEM bool TestIntersection<double> (const Ray2<double>&,
    const Box2<double>&);
template WML_ITEM bool TestIntersection<double> (const Line2<double>&,
    const Box2<double>&);
template WML_ITEM bool FindIntersection<double> (const Vector2<double>&,
    const Vector2<double>&, const double[2], double&, double&);
template WML_ITEM bool FindIntersection<double> (const Segment2<double>&,
    const Box2<double>&, int&, Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double> (const Ray2<double>&,
    const Box2<double>&, int&, Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double> (const Line2<double>&,
    const Box2<double>&, int&, Vector2<double>[2]);
}
//----------------------------------------------------------------------------
