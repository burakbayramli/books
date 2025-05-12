// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDELAUNAY2_H
#define WMLDELAUNAY2_H

// The Delaunay triangulation method is a modification of code written by 
// Dave Watson.  It uses an algorithm described in
//
//     Watson, D.F., 1981, Computing the n-dimensional Delaunay 
//     tessellation with application to Voronoi polytopes: 
//     The Computer J., 24(2), p. 167-172.

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Delaunay2
{
public:
    // Construction and destruction.  In the first constructor,
    // Delaunay2 accepts ownership of the input array and will delete
    // it during destruction.  The second constructor is designed to allow
    // sharing of the network (note that the reference argument is not passed
    // as 'const').  Any other network that shares this object will not delete
    // the data in this object.
    Delaunay2 (int iVertexQuantity, Vector2<Real>* akVertex);
    Delaunay2 (Delaunay2& rkNet);
    virtual ~Delaunay2 ();
    bool IsOwner () const;

    // vertices
    int GetVertexQuantity () const;
    const Vector2<Real>& GetVertex (int i) const;
    const Vector2<Real>* GetVertices () const;
    Real GetXMin () const;
    Real GetXMax () const;
    Real GetXRange () const;
    Real GetYMin () const;
    Real GetYMax () const;
    Real GetYRange () const;


    // edges
    class WML_ITEM Edge
    {
    public:
        // vertices forming edge
        int m_aiVertex[2];

        // triangles sharing edge
        int m_aiTriangle[2];
    };

    int GetEdgeQuantity () const;
    const Edge& GetEdge (int i) const;
    const Edge* GetEdges () const;


    // triangles
    class WML_ITEM Triangle
    {
    public:
        // vertices, listed in counterclockwise order
        int m_aiVertex[3];

        // adjacent triangles,
        //   adj[0] points to triangle sharing edge (ver[0],ver[1])
        //   adj[1] points to triangle sharing edge (ver[1],ver[2])
        //   adj[2] points to triangle sharing edge (ver[2],ver[0])
        int m_aiAdjacent[3];
    };

    int GetTriangleQuantity () const;
    Triangle& GetTriangle (int i);
    const Triangle& GetTriangle (int i) const;
    Triangle* GetTriangles ();
    const Triangle* GetTriangles () const;

    // extra triangles (added to boundary)
    int GetExtraTriangleQuantity () const;
    Triangle& GetExtraTriangle (int i);
    const Triangle& GetExtraTriangle (int i) const;
    Triangle* GetExtraTriangles ();
    const Triangle* GetExtraTriangles () const;

    // helper functions
    static void ComputeBarycenter (const Vector2<Real>& rkV0,
        const Vector2<Real>& rkV1, const Vector2<Real>& rkV2,
        const Vector2<Real>& rkP, Real afBary[3]);

    static bool InTriangle (const Vector2<Real>& rkV0,
        const Vector2<Real>& rkV1, const Vector2<Real>& rkV2,
        const Vector2<Real>& rkTest);

    static void ComputeInscribedCenter (const Vector2<Real>& rkV0,
        const Vector2<Real>& rkV1, const Vector2<Real>& rkV2,
        Vector2<Real>& rkCenter);

    // tweaking parameters
    static Real& Epsilon ();  // default = 0.00001
    static Real& Range ();    // default = 10.0
    static int& TSize ();        // default = 75

protected:
    // for sharing
    bool m_bOwner;

    // vertices
    int m_iVertexQuantity;
    Vector2<Real>* m_akVertex;
    Real m_fXMin, m_fXMax, m_fXRange;
    Real m_fYMin, m_fYMax, m_fYRange;

    // edges
    int m_iEdgeQuantity;
    Edge* m_akEdge;

    // triangles
    int m_iTriangleQuantity;
    Triangle* m_akTriangle;

    // extra triangles to support interpolation on convex hull of vertices
    int m_iExtraTriangleQuantity;
    Triangle* m_akExtraTriangle;

    // tweaking parameters
    static Real ms_fEpsilon;
    static Real ms_fRange;
    static int ms_iTSize;
};

typedef Delaunay2<float> Delaunay2f;
typedef Delaunay2<double> Delaunay2d;

}

#endif
