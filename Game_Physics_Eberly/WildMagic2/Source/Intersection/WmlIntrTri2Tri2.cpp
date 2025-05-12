// Magic Software, Inc.
// http://www.magic-software.com
// Copyright (c) 2000-2003.  All Rights Reserved
//
// Source code from Magic Software is supplied under the terms of a license
// agreement and may not be copied or disclosed except in accordance with the
// terms of that agreement.  The various license agreements may be found at
// the Magic Software web site.  This file is subject to the license
//
// FREE SOURCE CODE
// http://www.magic-software.com/License/free.pdf

#include "WmlIntrTri2Tri2.h"
#include "WmlIntrInterval.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary triangles
//----------------------------------------------------------------------------
template <class Real>
static int WhichSide (const Vector2<Real> akV[3], const Vector2<Real>& rkP,
    const Vector2<Real>& rkD)
{
    // Vertices are projected to the form P+t*D.  Return value is +1 if all
    // t > 0, -1 if all t < 0, 0 otherwise, in which case the line splits the
    // triangle.

    int iPositive = 0, iNegative = 0, iZero = 0;

    for (int i = 0; i < 3; i++)
    {
        Real fT = rkD.Dot(akV[i] - rkP);
        if ( fT > 0.0f )
            iPositive++;
        else if ( fT < 0.0f )
            iNegative++;
        else
            iZero++;

        if ( iPositive > 0 && iNegative > 0 )
            return 0;
    }

    return ( iZero == 0 ? ( iPositive > 0 ? 1 : -1 ) : 0 );
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle2<Real>& rkTri0,
    const Triangle2<Real>& rkTri1)
{
    Vector2<Real> akV0[3] =
    {
        rkTri0.Origin(),
        rkTri0.Origin() + rkTri0.Edge0(),
        rkTri0.Origin() + rkTri0.Edge1()
    };

    Vector2<Real> akV1[3] =
    {
        rkTri1.Origin(),
        rkTri1.Origin() + rkTri1.Edge0(),
        rkTri1.Origin() + rkTri1.Edge1()
    };

    int iI0, iI1;
    Vector2<Real> kD;

    // test edges of V0-triangle for separation
    for (iI0 = 0, iI1 = 2; iI0 < 3; iI1 = iI0, iI0++)
    {
        // test axis V0[i1] + t*perp(V0[i0]-V0[i1]), perp(x,y) = (y,-x)
        kD.X() = akV0[iI0].Y() - akV0[iI1].Y();
        kD.Y() = akV0[iI1].X() - akV0[iI0].X();
        if ( WhichSide(akV1,akV0[iI1],kD) > 0 )
        {
            // V1-triangle is entirely on positive side of V0-triangle
            return false;
        }
    }

    // test edges of V1-triangle for separation
    for (iI0 = 0, iI1 = 2; iI0 < 3; iI1 = iI0, iI0++)
    {
        // test axis V1[i1] + t*perp(V1[i0]-V1[i1]), perp(x,y) = (y,-x)
        kD.X() = akV1[iI0].Y() - akV1[iI1].Y();
        kD.Y() = akV1[iI1].X() - akV1[iI0].X();
        if ( WhichSide(akV0,akV1[iI1],kD) > 0 )
        {
            // V0-triangle is entirely on positive side of V1-triangle
            return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
static void ClipConvexPolygonAgainstLine (const Vector2<Real>& rkN, Real fC,
    int& riQuantity, Vector2<Real> akV[6])
{
    // The input vertices are assumed to be in counterclockwise order.  The
    // ordering is an invariant of this function.

    // test on which side of line are the vertices
    int iPositive = 0, iNegative = 0, iPIndex = -1;
    Real afTest[6];
    int i;
    for (i = 0; i < riQuantity; i++)
    {
        afTest[i] = rkN.Dot(akV[i]) - fC;
        if ( afTest[i] > (Real)0.0 )
        {
            iPositive++;
            if ( iPIndex < 0 )
                iPIndex = i;
        }
        else if ( afTest[i] < (Real)0.0 )
        {
            iNegative++;
        }
    }

    if ( iPositive > 0 )
    {
        if ( iNegative > 0 )
        {
            // line transversely intersects polygon
            Vector2<Real> akCV[6];
            int iCQuantity = 0, iCur, iPrv;
            Real fT;

            if ( iPIndex > 0 )
            {
                // first clip vertex on line
                iCur = iPIndex;
                iPrv = iCur-1;
                fT = afTest[iCur]/(afTest[iCur] - afTest[iPrv]);
                akCV[iCQuantity++] = akV[iCur]+fT*(akV[iPrv]-akV[iCur]);

                // vertices on positive side of line
                while ( iCur < riQuantity && afTest[iCur] > (Real)0.0 )
                    akCV[iCQuantity++] = akV[iCur++];

                // last clip vertex on line
                if ( iCur < riQuantity )
                {
                    iPrv = iCur-1;
                }
                else
                {
                    iCur = 0;
                    iPrv = riQuantity - 1;
                }
                fT = afTest[iCur]/(afTest[iCur] - afTest[iPrv]);
                akCV[iCQuantity++] = akV[iCur]+fT*(akV[iPrv]-akV[iCur]);
            }
            else  // iPIndex is 0
            {
                // vertices on positive side of line
                iCur = 0;
                while ( iCur < riQuantity && afTest[iCur] > (Real)0.0 )
                    akCV[iCQuantity++] = akV[iCur++];

                // last clip vertex on line
                iPrv = iCur-1;
                fT = afTest[iCur]/(afTest[iCur] - afTest[iPrv]);
                akCV[iCQuantity++] = akV[iCur]+fT*(akV[iPrv]-akV[iCur]);

                // skip vertices on negative side
                while ( iCur < riQuantity && afTest[iCur] <= (Real)0.0 )
                    iCur++;

                // first clip vertex on line
                if ( iCur < riQuantity )
                {
                    iPrv = iCur-1;
                    fT = afTest[iCur]/(afTest[iCur] - afTest[iPrv]);
                    akCV[iCQuantity++] = akV[iCur]+fT*(akV[iPrv]-akV[iCur]);

                    // vertices on positive side of line
                    while ( iCur < riQuantity && afTest[iCur] > (Real)0.0 )
                        akCV[iCQuantity++] = akV[iCur++];
                }
                else
                {
                    // iCur = 0
                    iPrv = riQuantity - 1;
                    fT = afTest[0]/(afTest[0] - afTest[iPrv]);
                    akCV[iCQuantity++] = akV[0]+fT*(akV[iPrv]-akV[0]);
                }
            }

            riQuantity = iCQuantity;
            memcpy(akV,akCV,iCQuantity*sizeof(Vector2<Real>));
        }
        // else polygon fully on positive side of line, nothing to do
    }
    else
    {
        // polygon does not intersect positive side of line, clip all
        riQuantity = 0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Triangle2<Real>& rkTri0,
    const Triangle2<Real>& rkTri1, int& riQuantity, Vector2<Real> akVertex[6])
{
    Vector2<Real> akV0[3] =
    {
        rkTri0.Origin(),
        rkTri0.Origin() + rkTri0.Edge0(),
        rkTri0.Origin() + rkTri0.Edge1()
    };

    // The potential intersection is initialized to the V1-triangle.  The
    // set of vertices is refined based on clipping against each edge of the
    // V0-triangle.
    riQuantity = 3;
    akVertex[0] = rkTri1.Origin();
    akVertex[1] = rkTri1.Origin() + rkTri1.Edge0();
    akVertex[2] = rkTri1.Origin() + rkTri1.Edge1();

    for (int iI1 = 2, iI2 = 0; iI2 < 3; iI1 = iI2, iI2++)
    {
        // clip against edge <V0[iI1],V0[iI2]>
        Vector2<Real> kN(akV0[iI1].Y()-akV0[iI2].Y(),
            akV0[iI2].X()-akV0[iI1].X());
        Real fC = kN.Dot(akV0[iI1]);
        ClipConvexPolygonAgainstLine(kN,fC,riQuantity,akVertex);
        if ( riQuantity == 0 )
        {
            // triangle completely clipped, no intersection occurs
            return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// moving triangles
//----------------------------------------------------------------------------
template <class Real>
class _Configuration
{
public:
    void ComputeTwo (const Vector2<Real> akV[3], const Vector2<Real>& rkD,
        int iI0, int iI1, int iI2);

    void ComputeThree (const Vector2<Real> akV[3], const Vector2<Real>& rkD,
        const Vector2<Real>& rkP);

    static bool NoIntersect (const _Configuration& rkCfg0,
        const _Configuration& rkCfg1, Real fTMax, Real fSpeed,
        int& riSide,  _Configuration& rkTCfg0, _Configuration& rkTCfg1,
        Real& rfTFirst, Real& rfTLast);

    static void GetIntersection (const _Configuration& rkCfg0,
        const _Configuration& rkCfg1, int iSide, const Vector2<Real> akV0[3],
        const Vector2<Real> akV1[3], int& riQuantity,
        Vector2<Real> akVertex[6]);

private:
    enum ProjectionMap
    {
        M21,  // 2 vertices map to min, 1 vertex maps to max
        M12,  // 1 vertex maps to min, 2 vertices map to max
        M11   // 1 vertex maps to min, 1 vertex maps to max
    };

    ProjectionMap m_eMap;    // how vertices map to the projection interval
    int m_aiIndex[3];        // the sorted indices of the vertices
    Real m_fMin, m_fMax;  // the interval is [min,max]
};
//----------------------------------------------------------------------------
template <class Real>
void _Configuration<Real>::ComputeTwo (const Vector2<Real> akV[3],
    const Vector2<Real>& rkD, int iI0, int iI1, int iI2)
{
    m_eMap = M12;
    m_aiIndex[0] = iI0;
    m_aiIndex[1] = iI1;
    m_aiIndex[2] = iI2;
    m_fMin = rkD.Dot(akV[iI0] - akV[iI1]);
    m_fMax = (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
void _Configuration<Real>::ComputeThree (const Vector2<Real> akV[3],
    const Vector2<Real>& rkD, const Vector2<Real>& rkP)
{
    Real fD0 = rkD.Dot(akV[0] - rkP);
    Real fD1 = rkD.Dot(akV[1] - rkP);
    Real fD2 = rkD.Dot(akV[2] - rkP);

    // Make sure that m_aiIndex[...] is an even permutation of (0,1,2)
    // whenever the map value is M12 or M21.  This is needed to guarantee
    // the intersection of overlapping edges is properly computed.

    if ( fD0 <= fD1 )
    {
        if ( fD1 <= fD2 )  // d0 <= d1 <= d2
        {
            if ( fD0 != fD1 )
                m_eMap = ( fD1 != fD2 ? M11 : M12 );
            else
                m_eMap = M21;

            m_aiIndex[0] = 0;
            m_aiIndex[1] = 1;
            m_aiIndex[2] = 2;
            m_fMin = fD0;
            m_fMax = fD2;
        }
        else if ( fD0 <= fD2 )  // d0 <= d2 < d1
        {
            if ( fD0 != fD2 )
            {
                m_eMap = M11;
                m_aiIndex[0] = 0;
                m_aiIndex[1] = 2;
                m_aiIndex[2] = 1;
            }
            else
            {
                m_eMap = M21;
                m_aiIndex[0] = 2;
                m_aiIndex[1] = 0;
                m_aiIndex[2] = 1;
            }

            m_fMin = fD0;
            m_fMax = fD1;
        }
        else  // d2 < d0 <= d1
        {
            m_eMap = ( fD0 != fD1 ? M12 : M11 );
            m_aiIndex[0] = 2;
            m_aiIndex[1] = 0;
            m_aiIndex[2] = 1;
            m_fMin = fD2;
            m_fMax = fD1;
        }
    }
    else
    {
        if ( fD2 <= fD1 )  // d2 <= d1 < d0
        {
            if ( fD2 != fD1 )
            {
                m_eMap = M11;
                m_aiIndex[0] = 2;
                m_aiIndex[1] = 1;
                m_aiIndex[2] = 0;
            }
            else
            {
                m_eMap = M21;
                m_aiIndex[0] = 1;
                m_aiIndex[1] = 2;
                m_aiIndex[2] = 0;
            }

            m_fMin = fD2;
            m_fMax = fD0;
        }
        else if ( fD2 <= fD0 )  // d1 < d2 <= d0
        {
            m_eMap = ( fD2 != fD0 ? M11 : M12 );
            m_aiIndex[0] = 1;
            m_aiIndex[1] = 2;
            m_aiIndex[2] = 0;
            m_fMin = fD1;
            m_fMax = fD0;
        }
        else  // d1 < d0 < d2
        {
            m_eMap = M11;
            m_aiIndex[0] = 1;
            m_aiIndex[1] = 0;
            m_aiIndex[2] = 2;
            m_fMin = fD1;
            m_fMax = fD2;
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool _Configuration<Real>::NoIntersect (const _Configuration& rkCfg0,
    const _Configuration& rkCfg1, Real fTMax, Real fSpeed, int& riSide,
    _Configuration& rkTCfg0, _Configuration& rkTCfg1, Real& rfTFirst,
    Real& rfTLast)
{
    Real fInvSpeed, fT;

    if ( rkCfg1.m_fMax < rkCfg0.m_fMin )
    {
        // V1-interval initially on left of V0-interval
        if ( fSpeed <= (Real)0.0 )
            return true;  // intervals moving apart

        // update first time
        fInvSpeed = ((Real)1.0)/fSpeed;
        fT = (rkCfg0.m_fMin - rkCfg1.m_fMax)*fInvSpeed;
        if ( fT > rfTFirst )
        {
            rfTFirst = fT;
            riSide = -1;
            rkTCfg0 = rkCfg0;
            rkTCfg1 = rkCfg1;
        }

        // test for exceedance of time interval
        if ( rfTFirst > fTMax )
            return true;

        // update last time
        fT = (rkCfg0.m_fMax - rkCfg1.m_fMin)*fInvSpeed;
        if ( fT < rfTLast )
            rfTLast = fT;

        // test for separation
        if ( rfTFirst > rfTLast )
            return true;
    }
    else if ( rkCfg0.m_fMax < rkCfg1.m_fMin )
    {
        // V1-interval initially on right of V0-interval
        if ( fSpeed >= (Real)0.0 )
            return true;  // intervals moving apart

        // update first time
        fInvSpeed = ((Real)1.0)/fSpeed;
        fT = (rkCfg0.m_fMax - rkCfg1.m_fMin)*fInvSpeed;
        if ( fT > rfTFirst )
        {
            rfTFirst = fT;
            riSide = 1;
            rkTCfg0 = rkCfg0;
            rkTCfg1 = rkCfg1;
        }

        // test for exceedance of time interval
        if ( rfTFirst > fTMax )
            return true;

        // update last time
        fT = (rkCfg0.m_fMin - rkCfg1.m_fMax)*fInvSpeed;
        if ( fT < rfTLast )
            rfTLast = fT;

        // test for separation
        if ( rfTFirst > rfTLast )
            return true;
    }
    else
    {
        // V0-interval and V1-interval initially overlap
        if ( fSpeed > (Real)0.0 )
        {
            // update last time
            fInvSpeed = ((Real)1.0)/fSpeed;
            fT = (rkCfg0.m_fMax - rkCfg1.m_fMin)*fInvSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;

            // test for separation
            if ( rfTFirst > rfTLast )
                return true;
        }
        else if ( fSpeed < (Real)0.0 )
        {
            // update last time
            fInvSpeed = ((Real)1.0)/fSpeed;
            fT = (rkCfg0.m_fMin - rkCfg1.m_fMax)*fInvSpeed;
            if ( fT < rfTLast )
                rfTLast = fT;

            // test for separation
            if ( rfTFirst > rfTLast )
                return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
void _Configuration<Real>::GetIntersection (const _Configuration& rkCfg0,
    const _Configuration& rkCfg1, int iSide, const Vector2<Real> akV0[3],
    const Vector2<Real> akV1[3], int& riQuantity, Vector2<Real> akVertex[6])
{
    Vector2<Real> kEdge, kDiff;
    const Vector2<Real>* pkOrigin;
    Real fInvEdE, fMin, fMax, afInterval[2];
    int i;

    if ( iSide == 1 )  // V1-interval contacts V0-interval on right
    {
        if ( rkCfg0.m_eMap == M21 || rkCfg0.m_eMap == M11 )
        {
            riQuantity = 1;
            akVertex[0] = akV0[rkCfg0.m_aiIndex[2]];
        }
        else if ( rkCfg1.m_eMap == M12 || rkCfg1.m_eMap == M11 )
        {
            riQuantity = 1;
            akVertex[0] = akV1[rkCfg1.m_aiIndex[0]];
        }
        else  // rkCfg0.m_eMap == M12 && rkCfg1.m_eMap == M21 (edge overlap)
        {
            pkOrigin = &akV0[rkCfg0.m_aiIndex[1]];
            kEdge = akV0[rkCfg0.m_aiIndex[2]] - *pkOrigin;
            fInvEdE = ((Real)1.0)/kEdge.Dot(kEdge);
            kDiff = akV1[rkCfg1.m_aiIndex[1]] - *pkOrigin;
            fMin = kEdge.Dot(kDiff)*fInvEdE;
            kDiff = akV1[rkCfg1.m_aiIndex[0]] - *pkOrigin;
            fMax = kEdge.Dot(kDiff)*fInvEdE;
            assert( fMin <= fMax );
            riQuantity = FindIntersection((Real)0.0,(Real)1.0,fMin,fMax,
                afInterval[0],afInterval[1]);
            assert( riQuantity > 0 );
            for (i = 0; i < riQuantity; i++)
                akVertex[i] = *pkOrigin + afInterval[i]*kEdge;
        }
    }
    else if ( iSide == -1 )  // V1-interval contacts V0-interval on left
    {
        if ( rkCfg1.m_eMap == M21 || rkCfg1.m_eMap == M11 )
        {
            riQuantity = 1;
            akVertex[0] = akV1[rkCfg1.m_aiIndex[2]];
        }
        else if ( rkCfg0.m_eMap == M12 || rkCfg0.m_eMap == M11 )
        {
            riQuantity = 1;
            akVertex[0] = akV0[rkCfg0.m_aiIndex[0]];
        }
        else  // rkCfg1.m_eMap == M12 && rkCfg0.m_eMap == M21 (edge overlap)
        {
            pkOrigin = &akV1[rkCfg1.m_aiIndex[1]];
            kEdge = akV1[rkCfg1.m_aiIndex[2]] - *pkOrigin;
            fInvEdE = 1.0f/kEdge.Dot(kEdge);
            kDiff = akV0[rkCfg0.m_aiIndex[1]] - *pkOrigin;
            fMin = kEdge.Dot(kDiff)*fInvEdE;
            kDiff = akV0[rkCfg0.m_aiIndex[0]] - *pkOrigin;
            fMax = kEdge.Dot(kDiff)*fInvEdE;
            assert( fMin <= fMax );
            riQuantity = FindIntersection((Real)0.0,(Real)1.0,fMin,fMax,
                afInterval[0],afInterval[1]);
            assert( riQuantity > 0 );
            for (i = 0; i < riQuantity; i++)
                akVertex[i] = *pkOrigin + afInterval[i]*kEdge;
        }
    }
    else  // triangles were initially intersecting
    {
        Triangle2<Real> kTri0, kTri1;
        kTri0.Origin() = akV0[0];
        kTri0.Edge0() = akV0[1] - akV0[0];
        kTri0.Edge1() = akV0[2] - akV0[0];
        kTri1.Origin() = akV1[0];
        kTri1.Edge0() = akV1[1] - akV1[0];
        kTri1.Edge1() = akV1[2] - akV1[0];
        FindIntersection(kTri0,kTri1,riQuantity,akVertex);
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (Real fTMax, const Triangle2<Real>& rkTri0,
    const Vector2<Real>& rkW0, const Triangle2<Real>& rkTri1,
    const Vector2<Real>& rkW1, Real& rfTFirst, Real& rfTLast)
{
    Vector2<Real> akV0[3] =
    {
        rkTri0.Origin(),
        rkTri0.Origin() + rkTri0.Edge0(),
        rkTri0.Origin() + rkTri0.Edge1()
    };

    Vector2<Real> akV1[3] =
    {
        rkTri1.Origin(),
        rkTri1.Origin() + rkTri1.Edge0(),
        rkTri1.Origin() + rkTri1.Edge1()
    };

    // process as if V0-triangle is stationary and V1-triangle is moving
    Vector2<Real> kW = rkW1 - rkW0;
    int iSide = 0;  // 0 = NONE, -1 = LEFT, +1 = RIGHT
    rfTFirst = (Real)0.0f;
    rfTLast = Math<Real>::MAX_REAL;

    _Configuration<Real> kCfg0, kCfg1, kTCfg0, kTCfg1;
    int iI0, iI1, iI2;
    Vector2<Real> kD;
    Real fSpeed;

    // process edges of V0-triangle
    for (iI0 = 1, iI1 = 2, iI2 = 0; iI2 < 3; iI0 = iI1, iI1 = iI2, iI2++)
    {
        // test axis V0[i1] + t*perp(V0[i2]-V0[i1]), perp(x,y) = (y,-x)
        kD.X() = akV0[iI2].Y() - akV0[iI1].Y();
        kD.Y() = akV0[iI1].X() - akV0[iI2].X();
        fSpeed = kD.Dot(kW);

        kCfg0.ComputeTwo(akV0,kD,iI0,iI1,iI2);
        kCfg1.ComputeThree(akV1,kD,akV0[iI1]);

        if ( _Configuration<Real>::NoIntersect(kCfg0,kCfg1,fTMax,fSpeed,iSide,
             kTCfg0,kTCfg1,rfTFirst,rfTLast) )
        {
            return false;
        }
    }

    // process edges of V1-triangle
    for (iI0 = 1, iI1 = 2, iI2 = 0; iI2 < 3; iI0 = iI1, iI1 = iI2, iI2++)
    {
        // test axis V1[i1] + t*perp(V1[i2]-V1[i1]), perp(x,y) = (y,-x)
        kD.X() = akV1[iI2].Y() - akV1[iI1].Y();
        kD.Y() = akV1[iI1].X() - akV1[iI2].X();
        fSpeed = kD.Dot(kW);

        kCfg1.ComputeTwo(akV1,kD,iI0,iI1,iI2);
        kCfg0.ComputeThree(akV0,kD,akV1[iI1]);

        if ( _Configuration<Real>::NoIntersect(kCfg0,kCfg1,fTMax,fSpeed,iSide,
             kTCfg0,kTCfg1,rfTFirst,rfTLast) )
        {
            return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (Real fTMax, const Triangle2<Real>& rkTri0,
    const Vector2<Real>& rkW0, const Triangle2<Real>& rkTri1,
    const Vector2<Real>& rkW1, Real& rfTFirst, Real& rfTLast,
    int& riQuantity, Vector2<Real> akVertex[6])
{
    Vector2<Real> akV0[3] =
    {
        rkTri0.Origin(),
        rkTri0.Origin() + rkTri0.Edge0(),
        rkTri0.Origin() + rkTri0.Edge1()
    };

    Vector2<Real> akV1[3] =
    {
        rkTri1.Origin(),
        rkTri1.Origin() + rkTri1.Edge0(),
        rkTri1.Origin() + rkTri1.Edge1()
    };

    // process as if V0-triangle is stationary and V1-triangle is moving
    Vector2<Real> kW = rkW1 - rkW0;
    int iSide = 0;  // 0 = NONE, -1 = LEFT, +1 = RIGHT
    rfTFirst = (Real)0.0f;
    rfTLast = Math<Real>::MAX_REAL;

    _Configuration<Real> kCfg0, kCfg1, kTCfg0, kTCfg1;
    int iI0, iI1, iI2;
    Vector2<Real> kD;
    Real fSpeed;

    // process edges of V0-triangle
    for (iI0 = 1, iI1 = 2, iI2 = 0; iI2 < 3; iI0 = iI1, iI1 = iI2, iI2++)
    {
        // test axis V0[i1] + t*perp(V0[i2]-V0[i1]), perp(x,y) = (y,-x)
        kD.X() = akV0[iI2].Y() - akV0[iI1].Y();
        kD.Y() = akV0[iI1].X() - akV0[iI2].X();
        fSpeed = kD.Dot(kW);

        kCfg0.ComputeTwo(akV0,kD,iI0,iI1,iI2);
        kCfg1.ComputeThree(akV1,kD,akV0[iI1]);

        if ( _Configuration<Real>::NoIntersect(kCfg0,kCfg1,fTMax,fSpeed,iSide,
             kTCfg0,kTCfg1,rfTFirst,rfTLast) )
        {
            return false;
        }
    }

    // process edges of V1-triangle
    for (iI0 = 1, iI1 = 2, iI2 = 0; iI2 < 3; iI0 = iI1, iI1 = iI2, iI2++)
    {
        // test axis V1[i1] + t*perp(V1[i2]-V1[i1]), perp(x,y) = (y,-x)
        kD.X() = akV1[iI2].Y() - akV1[iI1].Y();
        kD.Y() = akV1[iI1].X() - akV1[iI2].X();
        fSpeed = kD.Dot(kW);

        kCfg1.ComputeTwo(akV1,kD,iI0,iI1,iI2);
        kCfg0.ComputeThree(akV0,kD,akV1[iI1]);

        if ( _Configuration<Real>::NoIntersect(kCfg0,kCfg1,fTMax,fSpeed,iSide,
             kTCfg0,kTCfg1,rfTFirst,rfTLast) )
        {
            return false;
        }
    }

    // move triangles to first contact
    Vector2<Real> akMoveV0[3], akMoveV1[3];
    for (int i = 0; i < 3; i++)
    {
        akMoveV0[i] = akV0[i] + rfTFirst*rkW0;
        akMoveV1[i] = akV1[i] + rfTFirst*rkW1;
    };

    _Configuration<Real>::GetIntersection(kTCfg0,kTCfg1,iSide,akMoveV0,
        akMoveV1,riQuantity,akVertex);

    return riQuantity > 0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Triangle2<float>&, const Triangle2<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Triangle2<float>&, const Triangle2<float>&, int&,
    Vector2<float>[6]);
template WML_ITEM bool TestIntersection<float> (float,
    const Triangle2<float>&, const Vector2<float>&,
    const Triangle2<float>&, const Vector2<float>&, float&, float&);
template WML_ITEM bool FindIntersection<float> (float,
    const Triangle2<float>&, const Vector2<float>&,
    const Triangle2<float>&, const Vector2<float>&, float&, float&, int&,
    Vector2<float>[6]);

template WML_ITEM bool TestIntersection<double> (
    const Triangle2<double>&, const Triangle2<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Triangle2<double>&, const Triangle2<double>&, int&,
    Vector2<double>[6]);
template WML_ITEM bool TestIntersection<double> (double,
    const Triangle2<double>&, const Vector2<double>&,
    const Triangle2<double>&, const Vector2<double>&, double&, double&);
template WML_ITEM bool FindIntersection<double> (double,
    const Triangle2<double>&, const Vector2<double>&,
    const Triangle2<double>&, const Vector2<double>&, double&, double&, int&,
    Vector2<double>[6]);
}
//----------------------------------------------------------------------------
