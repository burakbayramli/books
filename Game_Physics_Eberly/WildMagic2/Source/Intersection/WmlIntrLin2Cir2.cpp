// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin2Cir2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static bool Find (const Vector2<Real>& rkP, const Vector2<Real>& rkD,
    const Vector2<Real>& rkC, Real fR, int& riRootCount, Real afT[2])
{
    // Intersection of a the line P+t*D and the circle |X-C| = R.  The t
    // value is a root to the quadratic equation:
    //   0 = |t*D+P-C|^2 - R^2
    //     = |D|^2*t^2 + 2*D.Dot(P-C)*t + |P-C|^2-R^2
    //     = a2*t^2 + 2*a1*t + a0
    // If two roots are returned, the order is T[0] < T[1].  Hopefully the
    // application will be kind and provide line directions D that are not so
    // small that a2 is nearly zero and potentially creates numerical
    // problems.

    Vector2<Real> kDiff = rkP - rkC;
    Real fA0 = kDiff.SquaredLength() - fR*fR;
    Real fA1 = rkD.Dot(kDiff);
    Real fA2 = rkD.SquaredLength();
    Real fDiscr = fA1*fA1 - fA0*fA2;
    if ( fDiscr > (Real)0.0 )
    {
        riRootCount = 2;
        fDiscr = Math<Real>::Sqrt(fDiscr);

        Real fInvDenom;

        if ( Math<Real>::FAbs(fA2) >= Math<Real>::FAbs(fA0) )
        {
            fInvDenom = ((Real)1.0)/fA2;
            afT[0] = (-fA1 - fDiscr)*fInvDenom;
            afT[1] = (-fA1 + fDiscr)*fInvDenom;
        }
        else
        {
            if ( fA1 >= (Real)0.0 )
            {
                fInvDenom = -((Real)1.0)/(fA1+fDiscr);
                afT[1] = fA0*fInvDenom;
                afT[0] = fA0/(fA2*afT[1]);
            }
            else
            {
                fInvDenom = -((Real)1.0)/(fA1-fDiscr);
                afT[0] = fA0*fInvDenom;
                afT[1] = fA0/(fA2*afT[0]);
            }
        }
    }
    else if ( fDiscr < (Real)0.0 )
    {
        riRootCount = 0;
    }
    else  // fDiscr == 0
    {
        riRootCount = 1;
        afT[0] = -fA1/fA2;
    }

    return riRootCount != 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line2<Real>& rkLine,
    const Circle2<Real>& rkCircle, int& riQuantity, Vector2<Real> akPoint[2])
{
    Real afT[2];
    bool bIntersects = Find(rkLine.Origin(),rkLine.Direction(),
        rkCircle.Center(),rkCircle.Radius(),riQuantity,afT);

    if ( bIntersects )
    {
        // construct line-circle points of intersection
        for (int i = 0; i < riQuantity; i++)
            akPoint[i] = rkLine.Origin() + afT[i]*rkLine.Direction();
    }

    return riQuantity != 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Line2<Real>& rkLine,
    const Arc2<Real>& rkArc, int& riQuantity, Vector2<Real> akPoint[2])
{
    int iRootCount;
    Real afT[2];
    bool bIntersects = Find(rkLine.Origin(),rkLine.Direction(),
        rkArc.Center(),rkArc.Radius(),iRootCount,afT);

    riQuantity = 0;
    if ( bIntersects )
    {
        // Construct line-circle points of intersection and test if they are
        // on the arc.
        for (int i = 0; i < iRootCount; i++)
        {
            akPoint[riQuantity] = rkLine.Origin() + afT[i]*rkLine.Direction();
            if ( rkArc.Contains(akPoint[riQuantity]) )
                riQuantity++;
        }
    }

    return riQuantity != 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray2<Real>& rkRay,
    const Circle2<Real>& rkCircle, int& riQuantity, Vector2<Real> akPoint[2])
{
    Real afT[2];
    bool bIntersects = Find(rkRay.Origin(),rkRay.Direction(),
        rkCircle.Center(),rkCircle.Radius(),riQuantity,afT);

    if ( bIntersects )
    {
        // reduce root count if line-circle intersections are not on ray
        if ( riQuantity == 1 )
        {
            if ( afT[0] < (Real)0.0 )
                riQuantity = 0;
        }
        else
        {
            if ( afT[1] < (Real)0.0 )
            {
                riQuantity = 0;
            }
            else if ( afT[0] < (Real)0.0 )
            {
                riQuantity = 1;
                afT[0] = afT[1];
            }
        }

        // construct ray-circle points of intersection
        for (int i = 0; i < riQuantity; i++)
            akPoint[i] = rkRay.Origin() + afT[i]*rkRay.Direction();
    }

    return riQuantity != 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Ray2<Real>& rkRay, const Arc2<Real>& rkArc,
    int& riQuantity, Vector2<Real> akPoint[2])
{
    int iRootCount;
    Real afT[2];
    bool bIntersects = Find(rkRay.Origin(),rkRay.Direction(),rkArc.Center(),
        rkArc.Radius(),iRootCount,afT);

    riQuantity = 0;
    if ( bIntersects )
    {
        // reduce root count if line-circle intersections are not on ray
        if ( iRootCount == 1 )
        {
            if ( afT[0] < (Real)0.0 )
                iRootCount = 0;
        }
        else
        {
            if ( afT[1] < (Real)0.0 )
            {
                iRootCount = 0;
            }
            else if ( afT[0] < (Real)0.0 )
            {
                iRootCount = 1;
                afT[0] = afT[1];
            }
        }

        // Construct ray-circle points of intersection and test if they are
        // on the arc.
        for (int i = 0; i < iRootCount; i++)
        {
            akPoint[riQuantity] = rkRay.Origin() + afT[i]*rkRay.Direction();
            if ( rkArc.Contains(akPoint[riQuantity]) )
                riQuantity++;
        }
    }

    return riQuantity != 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment2<Real>& rkSegment,
    const Circle2<Real>& rkCircle, int& riQuantity, Vector2<Real> akPoint[2])
{
    Real afT[2];
    bool bIntersects = Find(rkSegment.Origin(),rkSegment.Direction(),
        rkCircle.Center(),rkCircle.Radius(),riQuantity,afT);

    if ( bIntersects )
    {
        // reduce root count if line-circle intersections are not on segment
        if ( riQuantity == 1 )
        {
            if ( afT[0] < (Real)0.0 || afT[0] > (Real)1.0 )
                riQuantity = 0;
        }
        else
        {
            if ( afT[1] < (Real)0.0 || afT[0] > (Real)1.0 )
            {
                riQuantity = 0;
            }
            else
            {
                if ( afT[1] <= (Real)1.0 )
                {
                    if ( afT[0] < (Real)0.0 )
                    {
                        riQuantity = 1;
                        afT[0] = afT[1];
                    }
                }
                else
                {
                    riQuantity = ( afT[0] >= (Real)0.0 ? 1 : 0 );
                }
            }
        }

        // construct segment-circle points of intersection
        for (int i = 0; i < riQuantity; i++)
            akPoint[i] = rkSegment.Origin() + afT[i]*rkSegment.Direction();
    }

    return riQuantity != 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Segment2<Real>& rkSegment,
    const Arc2<Real>& rkArc, int& riQuantity, Vector2<Real> akPoint[2])
{
    int iRootCount;
    Real afT[2];
    bool bIntersects = Find(rkSegment.Origin(),rkSegment.Direction(),
        rkArc.Center(),rkArc.Radius(),iRootCount,afT);

    riQuantity = 0;
    if ( bIntersects )
    {
        // reduce root count if line-circle intersections are not on segment
        if ( iRootCount == 1 )
        {
            if ( afT[0] < (Real)0.0 || afT[0] > (Real)1.0 )
                iRootCount = 0;
        }
        else
        {
            if ( afT[1] < (Real)0.0 || afT[0] > (Real)1.0 )
            {
                iRootCount = 0;
            }
            else
            {
                if ( afT[1] <= (Real)1.0 )
                {
                    if ( afT[0] < (Real)0.0 )
                    {
                        iRootCount = 1;
                        afT[0] = afT[1];
                    }
                }
                else
                {
                    iRootCount = ( afT[0] >= (Real)0.0 ? 1 : 0 );
                }
            }
        }

        // Construct segment-circle points of intersection and test if they
        // are on the arc.
        for (int i = 0; i < iRootCount; i++)
        {
            akPoint[riQuantity] = rkSegment.Origin() +
                afT[i]*rkSegment.Direction();
            if ( rkArc.Contains(akPoint[riQuantity]) )
                riQuantity++;
        }
    }

    return riQuantity != 0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool FindIntersection<float> (const Line2<float>&,
    const Circle2<float>&, int&, Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float> (const Line2<float>&,
    const Arc2<float>&, int&, Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float> (const Ray2<float>&,
    const Circle2<float>&, int&, Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float> (const Ray2<float>&,
    const Arc2<float>&, int&, Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float>
    (const Segment2<float>&, const Circle2<float>&, int&,
    Vector2<float>[2]);
template WML_ITEM bool FindIntersection<float>
    (const Segment2<float>&, const Arc2<float>&, int&,
    Vector2<float>[2]);

template WML_ITEM bool FindIntersection<double> (const Line2<double>&,
    const Circle2<double>&, int&, Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double> (const Line2<double>&,
    const Arc2<double>&, int&, Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double> (const Ray2<double>&,
    const Circle2<double>&, int&, Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double> (const Ray2<double>&,
    const Arc2<double>&, int&, Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double>
    (const Segment2<double>&, const Circle2<double>&, int&,
    Vector2<double>[2]);
template WML_ITEM bool FindIntersection<double>
    (const Segment2<double>&, const Arc2<double>&, int&,
    Vector2<double>[2]);
}
//----------------------------------------------------------------------------
