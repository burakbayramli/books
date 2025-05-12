// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONVEXHULL2_H
#define WMLCONVEXHULL2_H

#include "WmlVector2.h"
#include <vector>

namespace Wml
{

template <class Real>
class WML_ITEM ConvexHull2
{
public:
    // Construction and destruction.  ConvexHull2 does not take ownership
    // of the input array.  The application is responsible for deleting it.
    // The method for computing the convex hull is either "incremental" or
    // "divide and conqer", the choice made by selecting 'true' or 'false',
    // respectively, for bIncremental.
    ConvexHull2 (int iVQuantity, const Vector2<Real>* akVertex,
        bool bIncremental);
    ~ConvexHull2 ();

    // hull stored in counterclockwise order
    int GetQuantity () const;
    const int* GetIndices () const;

    // test if point is contained by hull
    bool ContainsPoint (const Vector2<Real>& rkP) const;

    // The 'collinear epsilon' is used to test if three points P0, P1, and P2
    // are collinear.  If A = P1-P0 and B = P2-P0, the points are collinear
    // in theory if d = A.x*B.y-A.y*B.x = 0.  For numerical robustness, the
    // test is implemented as |d|^2 <= e*|A|^2*|B|^2 where e is the collinear
    // epsilon.  The idea is that d = |Cross((A,0),(B,0))| = |A|*|B|*|sin(t)|
    // where t is the angle between A and B.  Therefore, the comparison is
    // really |sin(t)|^2 <= e, a relative error test.  The default e = 1e-06.
    static Real COLLINEAR_EPSILON;

protected:
    // two different methods to compute convex hull
    void ByDivideAndConquer ();
    void ByIncremental ();

    // remove collinear points on hull
    void RemoveCollinear ();

    // for sorting
    class SortedVertex
    {
    public:
        bool operator== (const SortedVertex& rkSV) const
        {
            return m_kV == rkSV.m_kV;
        }

        // Added to satisfy the SGI Mips Pro CC compiler that appears to be
        // instantiating this, but never using it.
        bool operator!= (const SortedVertex& rkSV) const
        {
            return !operator==(rkSV);
        }

        bool operator<  (const SortedVertex& rkSV) const
        {
            if ( m_kV.X() < rkSV.m_kV.X() )
                return true;
            if ( m_kV.X() > rkSV.m_kV.X() )
                return false;
            return m_kV.Y() < rkSV.m_kV.Y();
        }

        Vector2<Real> m_kV;
        int m_iIndex;
    };

    typedef typename std::vector<SortedVertex> SVArray;

    // hull dimensions
    enum
    {
        HULL_POINT,
        HULL_LINEAR,
        HULL_PLANAR
    };

    // for collinearity tests
    enum
    {
        ORDER_POSITIVE,
        ORDER_NEGATIVE,
        ORDER_COLLINEAR_LEFT,
        ORDER_COLLINEAR_RIGHT,
        ORDER_COLLINEAR_CONTAIN
    };

    int CollinearTest (const Vector2<Real>& rkP, const Vector2<Real>& rkQ0,
        const Vector2<Real>& rkQ1) const;

    // construct convex hull using divide-and-conquer
    void GetHull (int i0, int i1, const SVArray& rkSVArray, SVArray& rkHull);
    void Merge (SVArray& rkLHull, SVArray& rkRHull, SVArray& rkHull);
    void MergeLinear (const SortedVertex& rkP, SVArray& rkHull);
    void GetTangent (const SVArray& rkLHull, const SVArray& rkRHull,
        int& riL, int& riR);

    // construct convex hull incrementally
    void MergeLinear (const SortedVertex& rkP);
    void MergePlanar (const SortedVertex& rkP);

    // vertex information
    int m_iVQuantity;
    const Vector2<Real>* m_akVertex;

    // hull information
    int m_iHullType;
    SVArray m_kHull;

    // indices for ordered vertices of hull
    int m_iHQuantity;
    int* m_aiHIndex;
};

typedef ConvexHull2<float> ConvexHull2f;
typedef ConvexHull2<double> ConvexHull2d;

}

#endif
