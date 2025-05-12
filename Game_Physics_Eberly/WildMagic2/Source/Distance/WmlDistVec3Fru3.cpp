// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Fru3.h"
#include "WmlDistVec3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const Vector3<Real>& rkPoint,
    const Frustum3<Real>& rkFrustum, Vector3<Real>* pkClosest)
{
    // compute coordinates of point with respect to frustum coordinate system
    Vector3<Real> kDiff = rkPoint - rkFrustum.Origin();
    Vector3<Real> kTest = Vector3<Real>(
        kDiff.Dot(rkFrustum.LVector()),
        kDiff.Dot(rkFrustum.UVector()),
        kDiff.Dot(rkFrustum.DVector()));

    // perform calculations in octant with nonnegative L and U coordinates
    bool bLSignChange;
    if ( kTest.X() < (Real)0.0 )
    {
        bLSignChange = true;
        kTest.X() = -kTest.X();
    }
    else
    {
        bLSignChange = false;
    }

    bool bUSignChange;
    if ( kTest.Y() < (Real)0.0 )
    {
        bUSignChange = true;
        kTest.Y() = -kTest.Y();
    }
    else
    {
        bUSignChange = false;
    }

    // frustum derived parameters
    Real fLMin = rkFrustum.LBound(), fLMax = rkFrustum.GetDRatio()*fLMin;
    Real fUMin = rkFrustum.UBound(), fUMax = rkFrustum.GetDRatio()*fUMin;
    Real fDMin = rkFrustum.DMin(), fDMax = rkFrustum.DMax();
    Real fLMinSqr = fLMin*fLMin;
    Real fUMinSqr = fUMin*fUMin;
    Real fDMinSqr = fDMin*fDMin;
    Real fMinLDDot = fLMinSqr + fDMinSqr;
    Real fMinUDDot = fUMinSqr + fDMinSqr;
    Real fMinLUDDot = fLMinSqr + fMinUDDot;
    Real fMaxLDDot = rkFrustum.GetDRatio()*fMinLDDot;
    Real fMaxUDDot = rkFrustum.GetDRatio()*fMinUDDot;
    Real fMaxLUDDot = rkFrustum.GetDRatio()*fMinLUDDot;

    // Algorithm computes closest point in all cases by determining in which
    // Voronoi region of the vertices, edges, and faces of the frustum that
    // the test point lives.
    Vector3<Real> kClosest;
    Real fLDot, fUDot, fLDDot, fUDDot, fLUDDot, fLEdgeDot, fUEdgeDot, fT;

    if ( kTest.Z() >= fDMax )
    {
        if ( kTest.X() <= fLMax )
        {
            if ( kTest.Y() <= fUMax )
            {
                // F-face
                kClosest.X() = kTest.X();
                kClosest.Y() = kTest.Y();
                kClosest.Z() = fDMax;
            }
            else
            {
                // UF-edge
                kClosest.X() = kTest.X();
                kClosest.Y() = fUMax;
                kClosest.Z() = fDMax;
            }
        }
        else
        {
            if ( kTest.Y() <= fUMax )
            {
                // LF-edge
                kClosest.X() = fLMax;
                kClosest.Y() = kTest.Y();
                kClosest.Z() = fDMax;
            }
            else
            {
                // LUF-vertex
                kClosest.X() = fLMax;
                kClosest.Y() = fUMax;
                kClosest.Z() = fDMax;
            }
        }
    }
    else if ( kTest.Z() <= fDMin )
    {
        if ( kTest.X() <= fLMin )
        {
            if ( kTest.Y() <= fUMin )
            {
                // N-face
                kClosest.X() = kTest.X();
                kClosest.Y() = kTest.Y();
                kClosest.Z() = fDMin;
            }
            else
            {
                fUDDot = fUMin*kTest.Y() + fDMin*kTest.Z();
                if ( fUDDot >= fMaxUDDot )
                {
                    // UF-edge
                    kClosest.X() = kTest.X();
                    kClosest.Y() = fUMax;
                    kClosest.Z() = fDMax;
                }
                else if ( fUDDot >= fMinUDDot )
                {
                    // U-face
                    fUDot = fDMin*kTest.Y() - fUMin*kTest.Z();
                    fT = fUDot/fMinUDDot;
                    kClosest.X() = kTest.X();
                    kClosest.Y() = kTest.Y() - fT*fDMin;
                    kClosest.Z() = kTest.Z() + fT*fUMin;
                }
                else
                {
                    // UN-edge
                    kClosest.X() = kTest.X();
                    kClosest.Y() = fUMin;
                    kClosest.Z() = fDMin;
                }
            }
        }
        else
        {
            if ( kTest.Y() <= fUMin )
            {
                fLDDot = fLMin*kTest.X() + fDMin*kTest.Z();
                if ( fLDDot >= fMaxLDDot )
                {
                    // LF-edge
                    kClosest.X() = fLMax;
                    kClosest.Y() = kTest.Y();
                    kClosest.Z() = fDMax;
                }
                else if ( fLDDot >= fMinLDDot )
                {
                    // L-face
                    fLDot = fDMin*kTest.X() - fLMin*kTest.Z();
                    fT = fLDot/fMinLDDot;
                    kClosest.X() = kTest.X() - fT*fDMin;
                    kClosest.Y() = kTest.Y();
                    kClosest.Z() = kTest.Z() + fT*fLMin;
                }
                else
                {
                    // LN-edge
                    kClosest.X() = fLMin;
                    kClosest.Y() = kTest.Y();
                    kClosest.Z() = fDMin;
                }
            }
            else
            {
                fLUDDot = fLMin*kTest.X() + fUMin*kTest.Y() + fDMin*kTest.Z();
                fLEdgeDot = fUMin*fLUDDot - fMinLUDDot*kTest.Y();
                if ( fLEdgeDot >= (Real)0.0 )
                {
                    fLDDot = fLMin*kTest.X() + fDMin*kTest.Z();
                    if ( fLDDot >= fMaxLDDot )
                    {
                        // LF-edge
                        kClosest.X() = fLMax;
                        kClosest.Y() = kTest.Y();
                        kClosest.Z() = fDMax;
                    }
                    else if ( fLDDot >= fMinLDDot )
                    {
                        // L-face
                        fLDot = fDMin*kTest.X() - fLMin*kTest.Z();
                        fT = fLDot/fMinLDDot;
                        kClosest.X() = kTest.X() - fT*fDMin;
                        kClosest.Y() = kTest.Y();
                        kClosest.Z() = kTest.Z() + fT*fLMin;
                    }
                    else
                    {
                        // LN-edge
                        kClosest.X() = fLMin;
                        kClosest.Y() = kTest.Y();
                        kClosest.Z() = fDMin;
                    }
                }
                else
                {
                    fUEdgeDot = fLMin*fLUDDot - fMinLUDDot*kTest.X();
                    if ( fUEdgeDot >= (Real)0.0 )
                    {
                        fUDDot = fUMin*kTest.Y() + fDMin*kTest.Z();
                        if ( fUDDot >= fMaxUDDot )
                        {
                            // UF-edge
                            kClosest.X() = kTest.X();
                            kClosest.Y() = fUMax;
                            kClosest.Z() = fDMax;
                        }
                        else if ( fUDDot >= fMinUDDot )
                        {
                            // U-face
                            fUDot = fDMin*kTest.Y() - fUMin*kTest.Z();
                            fT = fUDot/fMinUDDot;
                            kClosest.X() = kTest.X();
                            kClosest.Y() = kTest.Y() - fT*fDMin;
                            kClosest.Z() = kTest.Z() + fT*fUMin;
                        }
                        else
                        {
                            // UN-edge
                            kClosest.X() = kTest.X();
                            kClosest.Y() = fUMin;
                            kClosest.Z() = fDMin;
                        }
                    }
                    else
                    {
                        if ( fLUDDot >= fMaxLUDDot )
                        {
                            // LUF-vertex
                            kClosest.X() = fLMax;
                            kClosest.Y() = fUMax;
                            kClosest.Z() = fDMax;
                        }
                        else if ( fLUDDot >= fMinLUDDot )
                        {
                            // LU-edge
                            fT = fLUDDot/fMinLUDDot;
                            kClosest.X() = fT*fLMin;
                            kClosest.Y() = fT*fUMin;
                            kClosest.Z() = fT*fDMin;
                        }
                        else
                        {
                            // LUN-vertex
                            kClosest.X() = fLMin;
                            kClosest.Y() = fUMin;
                            kClosest.Z() = fDMin;
                        }
                    }
                }
            }
        }
    }
    else
    {
        fLDot = fDMin*kTest.X() - fLMin*kTest.Z();
        fUDot = fDMin*kTest.Y() - fUMin*kTest.Z();
        if ( fLDot <= (Real)0.0 )
        {
            if ( fUDot <= (Real)0.0 )
            {
                // point inside frustum
                kClosest = kTest;
            }
            else
            {
                fUDDot = fUMin*kTest.Y() + fDMin*kTest.Z();
                if ( fUDDot >= fMaxUDDot )
                {
                    // UF-edge
                    kClosest.X() = kTest.X();
                    kClosest.Y() = fUMax;
                    kClosest.Z() = fDMax;
                }
                else
                {
                    // U-face
                    fUDot = fDMin*kTest.Y() - fUMin*kTest.Z();
                    fT = fUDot/fMinUDDot;
                    kClosest.X() = kTest.X();
                    kClosest.Y() = kTest.Y() - fT*fDMin;
                    kClosest.Z() = kTest.Z() + fT*fUMin;
                }
            }
        }
        else
        {
            if ( fUDot <= (Real)0.0 )
            {
                fLDDot = fLMin*kTest.X() + fDMin*kTest.Z();
                if ( fLDDot >= fMaxLDDot )
                {
                    // LF-edge
                    kClosest.X() = fLMax;
                    kClosest.Y() = kTest.Y();
                    kClosest.Z() = fDMax;
                }
                else
                {
                    // L-face
                    fLDot = fDMin*kTest.X() - fLMin*kTest.Z();
                    fT = fLDot/fMinLDDot;
                    kClosest.X() = kTest.X() - fT*fDMin;
                    kClosest.Y() = kTest.Y();
                    kClosest.Z() = kTest.Z() + fT*fLMin;
                }
            }
            else
            {
                fLUDDot = fLMin*kTest.X() + fUMin*kTest.Y() + fDMin*kTest.Z();
                fLEdgeDot = fUMin*fLUDDot - fMinLUDDot*kTest.Y();
                if ( fLEdgeDot >= (Real)0.0 )
                {
                    fLDDot = fLMin*kTest.X() + fDMin*kTest.Z();
                    if ( fLDDot >= fMaxLDDot )
                    {
                        // LF-edge
                        kClosest.X() = fLMax;
                        kClosest.Y() = kTest.Y();
                        kClosest.Z() = fDMax;
                    }
                    else // assert( fLDDot >= fMinLDDot ) from geometry
                    {
                        // L-face
                        fLDot = fDMin*kTest.X() - fLMin*kTest.Z();
                        fT = fLDot/fMinLDDot;
                        kClosest.X() = kTest.X() - fT*fDMin;
                        kClosest.Y() = kTest.Y();
                        kClosest.Z() = kTest.Z() + fT*fLMin;
                    }
                }
                else
                {
                    fUEdgeDot = fLMin*fLUDDot - fMinLUDDot*kTest.X();
                    if ( fUEdgeDot >= (Real)0.0 )
                    {
                        fUDDot = fUMin*kTest.Y() + fDMin*kTest.Z();
                        if ( fUDDot >= fMaxUDDot )
                        {
                            // UF-edge
                            kClosest.X() = kTest.X();
                            kClosest.Y() = fUMax;
                            kClosest.Z() = fDMax;
                        }
                        else // assert( fUDDot >= fMinUDDot ) from geometry
                        {
                            // U-face
                            fUDot = fDMin*kTest.Y() - fUMin*kTest.Z();
                            fT = fUDot/fMinUDDot;
                            kClosest.X() = kTest.X();
                            kClosest.Y() = kTest.Y() - fT*fDMin;
                            kClosest.Z() = kTest.Z() + fT*fUMin;
                        }
                    }
                    else
                    {
                        if ( fLUDDot >= fMaxLUDDot )
                        {
                            // LUF-vertex
                            kClosest.X() = fLMax;
                            kClosest.Y() = fUMax;
                            kClosest.Z() = fDMax;
                        }
                        else // assert( fLUDDot >= fMinLUDDot ) from geometry
                        {
                            // LU-edge
                            fT = fLUDDot/fMinLUDDot;
                            kClosest.X() = fT*fLMin;
                            kClosest.Y() = fT*fUMin;
                            kClosest.Z() = fT*fDMin;
                        }
                    }
                }
            }
        }
    }

    kDiff = kTest - kClosest;

    // convert back to original quadrant
    if ( bLSignChange )
        kClosest.X() = -kClosest.X();

    if ( bUSignChange )
        kClosest.Y() = -kClosest.Y();

    if ( pkClosest )
    {
        // caller wants closest point, convert back to world coordinates
        *pkClosest = rkFrustum.Origin() +
            kClosest.X()*rkFrustum.LVector() +
            kClosest.Y()*rkFrustum.UVector() +
            kClosest.Z()*rkFrustum.DVector();
    }

    // compute and return squared distance
    return kDiff.SquaredLength();
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const Vector3<Real>& rkPoint,
    const Frustum3<Real>& rkFrustum, Vector3<Real>* pkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkPoint,rkFrustum,pkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float> (const Vector3<float>&,
    const Frustum3<float>&, Vector3<float>*);
template WML_ITEM float Distance<float> (const Vector3<float>&,
    const Frustum3<float>&, Vector3<float>*);

template WML_ITEM double SqrDistance<double> (const Vector3<double>&,
    const Frustum3<double>&, Vector3<double>*);
template WML_ITEM double Distance<double> (const Vector3<double>&,
    const Frustum3<double>&, Vector3<double>*);
}
//----------------------------------------------------------------------------
