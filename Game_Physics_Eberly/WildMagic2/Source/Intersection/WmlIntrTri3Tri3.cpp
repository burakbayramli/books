// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrTri3Tri3.h"
#include "WmlIntrUtilityLin3.h"
#include "WmlIntrUtilityTri3.h"
using namespace Wml;

//----------------------------------------------------------------------------
// stationary objects
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSetNonCoplanarTri (const Vector3<Real> akU[3], 
    const Vector3<Real> akV[3], int& riQuantity, Vector3<Real> akP[6])
{
    // This must necessarily return a line or a point, depending on the
    // orientation of the triangles.

    // On an infinite plane coplanar with U, it necessarily crosses
    // two segments of the triangle V (if not coplanar)

    // This routine picks those two segments and projects their
    // intersection onto that plane, so that it is the same case
    // as a colinear line-tri intersection.

    Vector3<Real> kEdge1 = akU[1] - akU[0];
    Vector3<Real> kEdge2 = akU[2] - akU[0];
    Vector3<Real> kNormU = kEdge1.Cross(kEdge2);

    Real afV[3], fU = kNormU.Dot(akU[0]);
    int iV[3] = { 0, 1, 2 };
    int iTemp;

    // Project V and sort those projections
    for (int i = 0; i < 3; i++)
    {
        afV[i] = kNormU.Dot( akV[i] );
        for (int i2 = i; i2 > 0; i2--)
        {
            // insertion sort vertex order in iV for values in fV
            if ( afV[iV[i2]] < afV[iV[i2-1]] )
            {
                // swap
                iTemp = iV[i2];
                iV[i2] = iV[i2-1];
                iV[i2-1] = iTemp;
            }
        }
    }

    // At this point, afV contains the projections and iV[0] is the 
    // index into afV of the smallest value, and iV[2] is the largest.

    // Four cases (for an infinite plane coplanar with U through V
    // 1) Plane through a line
    // 2) Plane through a vertex
    // 3) Plane through an edge and a vertex
    // 4) Plane through two edges

    // In all cases but the 2nd, the resulting contact set is a line
    // This will attempt to find two points on that line (to determine
    // endpoints) by picking two segments from the triangle to make
    // that line.  This is done by picking a single vertex that is
    // contained by both of those segments, and then getting the points.

    int iVertex;

    if ( afV[iV[0]] == fU )
    {
        if ( afV[iV[1]] == fU )
        {
            // plane through a line
            iVertex = 2;
        }
        else
        {
            // plane through a vertex (vertex iV[0])
            riQuantity = 1;
            akP[0] = akV[iV[0]];
            return;
        }
    }
    else if ( afV[iV[2]] == fU )
    {
        if ( afV[iV[1]] == fU )
        {
            // plane through a line
            iVertex = 0;
        }
        else
        {
            // plane through a vertex (vertex iV[2])
            riQuantity = 1;
            akP[0] = akV[iV[2]];
            return;
        }
    }
    else if ( afV[iV[1]] == fU )
    {
        // plane through an edge and a vertex
        // either vertex is fine to pick lines from (except for 1)
        iVertex = 0;
    }
    else
    {
        if ( afV[iV[1]] < fU )  // two points below the plane
            iVertex = 2;
        else  // two points above the plane
            iVertex = 0;
    }

    // Now, find the intersecting points on those two segments in U
    // segment i1-i2 and i2-i3

    int i1 = iV[(iVertex + 2) % 3];
    int i2 = iV[iVertex];
    int i3 = iV[(iVertex + 1) % 3];

    Vector3<Real> akProj[2];
    akProj[0] = akV[i1] + (fU-afV[i1])/(afV[i2]-afV[i1])*(akV[i2]-akV[i1]);
    akProj[1] = akV[i2] + (fU-afV[i2])/(afV[i3]-afV[i2])*(akV[i3]-akV[i2]);

    FindContactSetColinearLineTri(akProj,akU,riQuantity,akP);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSetCoplanarTri (const Vector3<Real> akU[3], 
    const Vector3<Real> akV[3], int& riQuantity, Vector3<Real> akP[6])
{
    // The potential intersection is initialized to the V-triangle.  The
    // set of vertices is refined based on clipping against each edge of the
    // U-triangle.

    riQuantity = 3;
    memcpy(akP,akV,3*sizeof(Vector3<Real>));

    Vector3<Real> kEdge1 = akU[1] - akU[0];
    Vector3<Real> kEdge2 = akU[2] - akU[0];
    Vector3<Real> kN = kEdge1.Cross(kEdge2);

    for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++)
    {
        // This points inward to U.
        Vector3<Real> kNxSeg = kN.Cross(akU[i1] - akU[i0]);
        Real fConstant = kNxSeg.Dot(akU[i0]);
        ClipConvexPolygonAgainstPlane(kNxSeg,fConstant,riQuantity,akP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle3<Real>& rkUTri,
    const Triangle3<Real>& rkVTri)
{
    Vector3<Real> akU[3] =
    {
        rkUTri.Origin(),
        rkUTri.Origin() + rkUTri.Edge0(),
        rkUTri.Origin() + rkUTri.Edge1()
    };

    Vector3<Real> akV[3] =
    {
        rkVTri.Origin(),
        rkVTri.Origin() + rkVTri.Edge0(),
        rkVTri.Origin() + rkVTri.Edge1()
    };

    Vector3<Real> kDir;
    Real fUMin, fUMax, fVMin, fVMax;
    int i0, i1;

    // direction N
    Vector3<Real> akE[3] = { akU[1]-akU[0], akU[2]-akU[1], akU[0]-akU[2] };
    Vector3<Real> kN = akE[0].Cross(akE[1]);
    Real fNdU0 = kN.Dot(akU[0]);
    TriProjection(kN,akV,fVMin,fVMax);

    if ( fNdU0 < fVMin || fNdU0 > fVMax )
        return false;

    // direction M
    Vector3<Real> akF[3] = { akV[1]-akV[0], akV[2]-akV[1], akV[0]-akV[2] };
    Vector3<Real> kM = akF[0].Cross(akF[1]);

    Vector3<Real> kNxM = kN.Cross(kM);
    if ( kNxM.Dot(kNxM) >= Math<Real>::EPSILON*(kN.Dot(kN)*kM.Dot(kM)) )
    {
        // triangles are not parallel
        Real fMdV0 = kM.Dot(akV[0]);
        TriProjection(kM,akU,fUMin,fUMax);

        if ( fMdV0 < fUMin || fMdV0 > fUMax )
            return false;

        // directions E[i0]xF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            for (i0 = 0; i0 < 3; i0++)
            {
                kDir = akE[i0].Cross(akF[i1]);
                TriProjection(kDir,akU,fUMin,fUMax);
                TriProjection(kDir,akV,fVMin,fVMax);
                if ( fUMax < fVMin || fVMax < fUMin )
                    return false;
            }
        }
    }
    else  // triangles are parallel (and, in fact, coplanar)
    {
        // directions NxE[i0]
        for (i0 = 0; i0 < 3; i0++)
        {
            kDir = kN.Cross(akE[i0]);
            TriProjection(kDir,akU,fUMin,fUMax);
            TriProjection(kDir,akV,fVMin,fVMax);
            if ( fUMax < fVMin || fVMax < fUMin )
                return false;
        }

        // directions NxF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            kDir = kM.Cross(akF[i1]);
            TriProjection(kDir,akU,fUMin,fUMax);
            TriProjection(kDir,akV,fVMin,fVMax);
            if ( fUMax < fVMin || fVMax < fUMin )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Triangle3<Real>& rkUTri,
    const Triangle3<Real>& rkVTri, int& riQuantity, Vector3<Real> akP[6])
{
    Vector3<Real> akU[3] =
    {
        rkUTri.Origin(),
        rkUTri.Origin() + rkUTri.Edge0(),
        rkUTri.Origin() + rkUTri.Edge1()
    };

    Vector3<Real> akV[3] =
    {
        rkVTri.Origin(),
        rkVTri.Origin() + rkVTri.Edge0(),
        rkVTri.Origin() + rkVTri.Edge1()
    };

    Vector3<Real> kDir;
    Real fUMin, fUMax, fVMin, fVMax;
    int i0, i1;

    // direction N
    Vector3<Real> akE[3] = { akU[1]-akU[0], akU[2]-akU[1], akU[0]-akU[2] };
    Vector3<Real> kN = akE[0].Cross(akE[1]);
    Real fNdU0 = kN.Dot(akU[0]);
    TriProjection(kN,akV,fVMin,fVMax);

    if ( fNdU0 < fVMin || fNdU0 > fVMax )
        return false;

    // direction M
    Vector3<Real> akF[3] = { akV[1]-akV[0], akV[2]-akV[1], akV[0]-akV[2] };
    Vector3<Real> kM = akF[0].Cross(akF[1]);

    Vector3<Real> kNxM = kN.Cross(kM);
    if ( kNxM.Dot(kNxM) >= Math<Real>::EPSILON*(kN.Dot(kN)*kM.Dot(kM)) )
    {
        // triangles are not parallel
        Real fMdV0 = kM.Dot(akV[0]);
        TriProjection(kM,akU,fUMin,fUMax);

        if ( fMdV0 < fUMin || fMdV0 > fUMax )
            return false;

        // directions E[i0]xF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            for (i0 = 0; i0 < 3; i0++)
            {
                kDir = akE[i0].Cross(akF[i1]);
                TriProjection(kDir,akU,fUMin,fUMax);
                TriProjection(kDir,akV,fVMin,fVMax);
                if ( fUMax < fVMin || fVMax < fUMin )
                    return false;
            }
        }

        FindContactSetNonCoplanarTri(akU,akV,riQuantity,akP);
    }
    else  // triangles are parallel (and, in fact, coplanar)
    {
        // directions NxE[i0]
        for (i0 = 0; i0 < 3; i0++)
        {
            kDir = kN.Cross(akE[i0]);
            TriProjection(kDir,akU,fUMin,fUMax);
            TriProjection(kDir,akV,fVMin,fVMax);
            if ( fUMax < fVMin || fVMax < fUMin )
                return false;
        }

        // directions NxF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            kDir = kM.Cross(akF[i1]);
            TriProjection(kDir,akU,fUMin,fUMax);
            TriProjection(kDir,akV,fVMin,fVMax);
            if ( fUMax < fVMin || fVMax < fUMin )
                return false;
        }

        FindContactSetCoplanarTri(akU,akV,riQuantity,akP);
    }

    return true;
}
//----------------------------------------------------------------------------
// moving objects
//----------------------------------------------------------------------------
template <class Real>
static bool TriTriAxisTest (const Vector3<Real>& rkVel,
    const Vector3<Real>& rkAxis, const Vector3<Real> akU[3],
    const Vector3<Real> akV[3], Real& rfTFirst, Real& rfTLast, Real fTMax)
{
    Real fUMin, fUMax;
    TriProjection(rkAxis,akU,fUMin,fUMax);

    Real fVMin, fVMax;
    TriProjection(rkAxis,akV,fVMin,fVMax);

    return AxisTest(rkVel,rkAxis,fUMin,fUMax,fVMin,fVMax,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static bool TriTriAxisFind (const Vector3<Real>& rkVel, 
    const Vector3<Real>& rkAxis, const Vector3<Real> akU[3], 
    const Vector3<Real> akV[3], ContactSide& reSide,
    ContactConfig<Real>& rkTUC, ContactConfig<Real>& rkTVC, Real& rfTFirst,
    Real& rfTLast, Real fTMax)
{
    ContactConfig<Real> kUC;
    GetTriConfiguration(rkAxis,akU,kUC);

    ContactConfig<Real> kVC;
    GetTriConfiguration(rkAxis,akV,kVC);

    return AxisFind(rkVel,rkAxis,kUC,kVC,reSide,rkTUC,rkTVC,rfTFirst,rfTLast,
        fTMax);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSetTriTri (Vector3<Real> akU[3], Vector3<Real> akV[3],
    int& riQuantity, Vector3<Real> akP[6])
{
    // This function is ignorant of whether the triangles are coplanar and
    // assumes they are intersecting.

    Vector3<Real> kEdge1 = akU[1] - akU[0];
    Vector3<Real> kEdge2 = akU[2] - akU[0];
    Vector3<Real> kN = kEdge1.Cross(kEdge2);

    kEdge1 = akV[1] - akV[0];
    kEdge2 = akV[2] - akV[0];
    Vector3<Real> kM = kEdge1.Cross(kEdge2);

    Vector3<Real> kNxM = kN.Cross(kM);

    if ( kNxM.Dot(kNxM) < Math<Real>::EPSILON*(kN.Dot(kN)*kM.Dot(kM)) )
        FindContactSetCoplanarTri(akU,akV,riQuantity,akP);
    else
        FindContactSetNonCoplanarTri(akU,akV,riQuantity,akP);
}
//----------------------------------------------------------------------------
template <class Real>
static void FindContactSet (Vector3<Real> akU[3], Vector3<Real> akV[3], 
    ContactSide& reSide, ContactConfig<Real>& rkUC,
    ContactConfig<Real>& rkVC, const Vector3<Real>& /* rkVel */,
    int& riQuantity, Vector3<Real> akP[6])
{
    // TO DO.  The parameter rkVel is not used here.  Is this an error?  Or
    // did the caller already make the adjustments to the positions to
    // incorporate the motion?

    if ( reSide == RIGHT ) // V right of U
    {
        if ( rkUC.m_kMap == m21 || rkUC.m_kMap == m111 )
        {
            // triangle U touching triangle V at a single point
            riQuantity = 1;
            akP[0] = akU[2];
        }
        else if ( rkVC.m_kMap == m12 || rkVC.m_kMap == m111 )
        {
            // triangle V touching triangle U at a single point
            riQuantity = 1;
            akP[0] = akV[0];
        }
        else if ( rkUC.m_kMap == m12 )
        {
            if ( rkVC.m_kMap == m21 )
            {
                // uedge-vedge intersection
                Vector3<Real> akUEdge[2] = { akU[1], akU[2] };
                Vector3<Real> akVEdge[2] = { akV[0], akV[1] };
                FindContactSetLinLin(akUEdge,akVEdge,riQuantity,akP);
            }
            else // ( rkVC.m_kMap == m3 )
            {
                // uedge-vface intersection
                Vector3<Real> akUEdge[2] = { akU[1], akU[2] };
                FindContactSetColinearLineTri(akUEdge,akV,riQuantity,akP);
            }
        }
        else // ( rkUC.m_kMap == m3 )
        {
            if ( rkVC.m_kMap == m21 )
            {

                // uface-vedge intersection
                Vector3<Real> akVEdge[2] = { akV[0], akV[1] };
                FindContactSetColinearLineTri(akVEdge,akU,riQuantity,akP);
            } 
            else // ( rkVC.m_kMap == m3 )
            {
                // uface-vface intersection
                FindContactSetCoplanarTri(akU,akV,riQuantity,akP);
            }
        }
    }
    else if ( reSide == LEFT ) // V left of U 
    {
        if ( rkVC.m_kMap == m21 || rkVC.m_kMap == m111 )
        {
            // triangle V touching triangle U at a single point
            riQuantity = 1;
            akP[0] = akV[2];
        }
        else if ( rkUC.m_kMap == m12 || rkUC.m_kMap == m111 )
        {
            // triangle U touching triangle V at a single point
            riQuantity = 1;
            akP[0] = akU[0];
        }
        else if ( rkVC.m_kMap == m12 )
        {
            if ( rkUC.m_kMap == m21 )
            {
                // uedge-vedge intersection
                Vector3<Real> akUEdge[2] = { akU[0], akU[1] };
                Vector3<Real> akVEdge[2] = { akV[1], akV[2] };
                FindContactSetLinLin(akUEdge,akVEdge,riQuantity,akP);
            }
            else // ( rkUC.m_kMap == m3 )
            {
                // vedge-uface intersection
                Vector3<Real> akVEdge[2] = { akV[1], akV[2] };
                FindContactSetColinearLineTri(akVEdge,akU,riQuantity,akP);
            }
        }
        else // ( rkVC.m_kMap == m3 )
        {
            if ( rkUC.m_kMap == m21 )
            {
                // uedge-vface intersection
                Vector3<Real> akUEdge[2] = { akU[0], akU[1] };
                FindContactSetColinearLineTri(akUEdge,akV,riQuantity,akP);
            } 
            else // ( rkUC.m_kMap == m3 )
            {
                // uface-vface intersection
                FindContactSetCoplanarTri(akU,akV,riQuantity,akP);
            }
        }
    }
    else // ( reSide == NONE )
    {
        FindContactSetTriTri(akU,akV,riQuantity,akP);
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Triangle3<Real>& rkUTri, 
    const Vector3<Real>& kUVelocity, const Triangle3<Real>& rkVTri,
    const Vector3<Real>& kVVelocity, Real& rfTFirst, Real fTMax)
{
    Vector3<Real> akU[3] =
    {
        rkUTri.Origin(),
        rkUTri.Origin() + rkUTri.Edge0(),
        rkUTri.Origin() + rkUTri.Edge1()
    };

    Vector3<Real> akV[3] =
    {
        rkVTri.Origin(),
        rkVTri.Origin() + rkVTri.Edge0(),
        rkVTri.Origin() + rkVTri.Edge1()
    };

    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    // velocity relative to the U triangle
    Vector3<Real> kVel = kVVelocity - kUVelocity;

    // direction N
    Vector3<Real> akE[3] = { akU[1]-akU[0], akU[2]-akU[1], akU[0]-akU[2] };
    Vector3<Real> kN = akE[0].Cross(akE[1]);
    if ( !TriTriAxisTest(kVel,kN,akU,akV,rfTFirst,fTLast,fTMax) )
        return false;

    // direction M
    Vector3<Real> akF[3] = { akV[1]-akV[0], akV[2]-akV[1], akV[0]-akV[2] };
    Vector3<Real> kM = akF[0].Cross(akF[1]);

    Vector3<Real> kNxM = kN.Cross(kM);
    Vector3<Real> kDir;
    int i0, i1;

    if ( kNxM.Dot(kNxM) >= Math<Real>::EPSILON*(kN.Dot(kN)*kM.Dot(kM)) )
    {
        // triangles are not parallel

        // direction M
        if ( !TriTriAxisTest(kVel,kM,akU,akV,rfTFirst,fTLast,fTMax) )
            return false;

        // directions E[i0]xF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            for (i0 = 0; i0 < 3; i0++)
            {
                kDir = akE[i0].Cross(akF[i1]);
                if ( !TriTriAxisTest(kVel,kDir,akU,akV,rfTFirst,fTLast,
                     fTMax) )
                {
                    return false;
                }
            }
        }
    }
    else  // triangles are parallel (and, in fact, coplanar)
    {
        // directions NxE[i0]
        for (i0 = 0; i0 < 3; i0++)
        {
            kDir = kN.Cross(akE[i0]);
            if ( !TriTriAxisTest(kVel,kDir,akU,akV,rfTFirst,fTLast,fTMax) )
                return false;
        }

        // directions NxF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            kDir = kM.Cross(akF[i1]);
            if ( !TriTriAxisTest(kVel,kDir,akU,akV,rfTFirst,fTLast,fTMax) )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::FindIntersection (const Triangle3<Real>& rkUTri,
    const Vector3<Real>& rkUVelocity, const Triangle3<Real>& rkVTri,
    const Vector3<Real>& rkVVelocity, Real& rfTFirst, Real fTMax,
    int& riQuantity, Vector3<Real> akP[6])
{
    Vector3<Real> akU[3] =
    {
        rkUTri.Origin(),
        rkUTri.Origin() + rkUTri.Edge0(),
        rkUTri.Origin() + rkUTri.Edge1()
    };

    Vector3<Real> akV[3] =
    {
        rkVTri.Origin(),
        rkVTri.Origin() + rkVTri.Edge0(),
        rkVTri.Origin() + rkVTri.Edge1()
    };

    rfTFirst = (Real)0.0;
    Real fTLast = Math<Real>::MAX_REAL;

    // velocity relative to the U triangle
    Vector3<Real> kVel = rkVVelocity - rkUVelocity;

    ContactSide eSide = NONE;
    ContactConfig<Real> kTUC, kTVC;

    // direction N
    Vector3<Real> akE[3] = { akU[1]-akU[0], akU[2]-akU[1], akU[0]-akU[2] };
    Vector3<Real> kN = akE[0].Cross(akE[1]);
    if ( !TriTriAxisFind(kVel,kN,akU,akV,eSide,kTUC,kTVC,rfTFirst,fTLast,
         fTMax) )
    {
        return false;
    }

    // direction M
    Vector3<Real> akF[3] = { akV[1]-akV[0], akV[2]-akV[1], akV[0]-akV[2] };
    Vector3<Real> kM = akF[0].Cross(akF[1]);

    Vector3<Real> kNxM = kN.Cross(kM);
    Vector3<Real> kDir;
    int i0, i1;

    if ( kNxM.Dot(kNxM) >= Math<Real>::EPSILON*(kN.Dot(kN)*kM.Dot(kM)) )
    {
        // triangles are not parallel

        // direction M
        if ( !TriTriAxisFind(kVel,kM,akU,akV,eSide,kTUC,kTVC,rfTFirst,fTLast,
             fTMax) )
        {
            return false;
        }

        // directions E[i0]xF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            for (i0 = 0; i0 < 3; i0++)
            {
                kDir = akE[i0].Cross(akF[i1]);
                if ( !TriTriAxisFind(kVel,kDir,akU,akV,eSide,kTUC,kTVC,
                     rfTFirst,fTLast,fTMax) )
                {
                    return false;
                }
            }
        }
    }
    else  // triangles are parallel (and, in fact, coplanar)
    {
        // directions NxE[i0]
        for (i0 = 0; i0 < 3; i0++)
        {
            kDir = kN.Cross(akE[i0]);
            if ( !TriTriAxisFind(kVel,kDir,akU,akV,eSide,kTUC,kTVC,rfTFirst,
                 fTLast,fTMax) )
            {
                return 0;
            }
        }

        // directions NxF[i1]
        for (i1 = 0; i1 < 3; i1++)
        {
            kDir = kM.Cross(akF[i1]);
            if ( !TriTriAxisFind(kVel,kDir,akU,akV,eSide,kTUC,kTVC,rfTFirst,
                 fTLast,fTMax) )
            {
                return 0;
            }
        }
    }
    
    if ( rfTFirst <= (Real)0.0 )
        return false;

    // adjust U and V for first time of contact before finding contact set
    Vector3<Real> akNewU[3], akNewV[3];
    for (i0 = 0; i0 < 3; i0++)
    {
        akNewU[i0] = akU[i0] + rfTFirst*rkUVelocity;
        akNewV[i0] = akV[i0] + rfTFirst*rkVVelocity;
    }

    FindContactSet(akNewU,akNewV,eSide,kTUC,kTVC,kVel,riQuantity,akP);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> 
    (const Triangle3<float>&, const Triangle3<float>&);
template WML_ITEM bool FindIntersection<float> (
    const Triangle3<float>&, const Triangle3<float>&, int&,
    Vector3<float>[6]);
template WML_ITEM bool TestIntersection<float> (
    const Triangle3<float>&, const Vector3<float>&,
    const Triangle3<float>&, const Vector3<float>&, float&, float);
template WML_ITEM bool FindIntersection<float> (
    const Triangle3<float>&, const Vector3<float>&,
    const Triangle3<float>&, const Vector3<float>&, float&, float, int&,
    Vector3<float>[6]);

template WML_ITEM bool TestIntersection<double>
    (const Triangle3<double>&, const Triangle3<double>&);
template WML_ITEM bool FindIntersection<double> (
    const Triangle3<double>&, const Triangle3<double>&, int&,
    Vector3<double>[6]);
template WML_ITEM bool TestIntersection<double> (
    const Triangle3<double>&, const Vector3<double>&,
    const Triangle3<double>&, const Vector3<double>&, double&, double);
template WML_ITEM bool FindIntersection<double> (
    const Triangle3<double>&, const Vector3<double>&,
    const Triangle3<double>&, const Vector3<double>&, double&, double, int&,
    Vector3<double>[6]);
}
//----------------------------------------------------------------------------
