// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONVEXHULL3_H
#define WMLCONVEXHULL3_H

#include "WmlVector3.h"
#include "WmlETManifoldMesh.h"
#include <map>
#include <set>
#include <vector>

namespace Wml
{

template <class Real>
class WML_ITEM ConvexHull3
{
public:
    // Construction and destruction.  ConvexHull3 does not take ownership
    // of the input array.  The application is responsible for deleting it.
    ConvexHull3 (int iVQuantity, const Vector3<Real>* akVertex);
    virtual ~ConvexHull3 ();

    // hull types
    enum
    {
        // Hull is a single point.  Quantity is 1, index array has one
        // element (index 0).
        HULL_POINT,

        // Hull is a line segment.  Quantity is 2, index array has two
        // elements that are indices to the end points of the segment.
        HULL_LINEAR,

        // Hull is a planar convex polygon.  Quantity is number of vertices,
        // index array has the indices to that number of vertices.  The
        // indices represent an ordered polygon, but since there is no
        // associated normal vector, you need to supply your own and determine
        // if the ordering is clockwise or counterclockwise relative to that
        // normal.  If you want a triangle connectivity array, you will have
        // to triangulate the polygon yourself.
        HULL_PLANAR,

        // The hull is a convex polyhedron (positive volume).  Quantity is
        // number of triangles, index array has 3 times that many elements.
        // Each triple of indices represents a triangle in the hull.  All the
        // triangles are counterclockwise ordered as you view the polyhedron
        // from the outside.
        HULL_SPATIAL
    };

    int GetType () const;
    const std::vector<int>& GetConnectivity () const;

protected:
    // construct convex hull incrementally
    void MergePoint (int iP);
    void MergeLinear (int iP);
    void MergePlanar (int iP);
    void MergeSpatial (int iP);

    // hull information
    int m_iHullType;
    std::vector<int> m_kHull;

private:
    class WML_ITEM Triangle : public ETManifoldMesh::Triangle
    {
    public:
        Triangle (int iV0, int iV1, int iV2)
            :
            ETManifoldMesh::Triangle(iV0,iV1,iV2)
        {
        }

        Vector3<Real> Normal;  // outward normal to triangle
    };

    static ETManifoldMesh::Triangle* CreateTriangle (int iV0, int iV1,
        int iV2);
    void InsertTriangle (int iV0, int iV1, int iV2);

    // input vertices (scaled to [-1,1]^3)
    int m_iVQuantity;
    Vector3<Real>* m_akVertex;

    // point, linear, or planar hull
    std::vector<int> m_kHullP;
    Vector3<Real> m_kOrigin, m_kNormal;

    // spatial hull
    ETManifoldMesh m_kHullS;
};

typedef ConvexHull3<float> ConvexHull3f;
typedef ConvexHull3<double> ConvexHull3d;

}

#endif
