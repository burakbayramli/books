// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLQUADRICSURFACE_H
#define WMLQUADRICSURFACE_H

#include "WmlMatrix3.h"
#include "WmlSurface.h"

namespace Wml
{

template <class Real>
class WML_ITEM QuadricSurface : public Surface<Real>
{
public:
    // Quadric surfaces are defined implicitly by x^T A x + b^T x + c = 0
    // where A is symmetric 3x3, b and x are 3x1, and c is a scalar.
    QuadricSurface (const Matrix3<Real>& rkA, const Vector3<Real>& rkB,
        Real fC);

    enum Type
    {
        QST_NONE,  // the implicit equation has no solution or is a tautology
        QST_POINT,
        QST_LINE,
        QST_PLANE,
        QST_TWO_PLANES,
        QST_PARABOLIC_CYLINDER,
        QST_ELLIPTIC_CYLINDER,
        QST_HYPERBOLIC_CYLINDER,
        QST_ELLIPTIC_PARABOLOID,
        QST_HYPERBOLIC_PARABOLOID,
        QST_ELLIPTIC_CONE,
        QST_HYPERBOLOID_ONE_SHEET,
        QST_HYPERBOLOID_TWO_SHEETS,
        QST_ELLIPSOID,
        QST_MAX_TYPE
    };

    // The returned array contains the eigenvalues of A, the entries of the
    // diagonal matrix D in y^T D y + e^T x + c = 0.
    void GetCharacterization (Type& eType, Real afD[3]) const;


    // Tessellation of a sphere using a 'seed' inscribed convex polyhedron.
    class Edge;
    class Triangle;
    class WML_ITEM Vertex
    {
    public:
        Vector3<Real>* m_pkPoint;
        int m_iNumEdges;
        class Edge** m_apkEdge;
    };

    class WML_ITEM Edge
    {
    public:
        Vertex* m_apkVertex[2];
        Triangle* m_apkTriangle[2];

        // For testing purposes, but not necessary for the algorithm.  This
        // allows the display program to show the subdivision structure.
        int m_iStep;
    };

    class WML_ITEM Triangle
    {
    public:
        // pointers to triangle vertices (counterclockwise order)
        Vertex* m_apkVertex[3];

        // triangle edges: E0 = <V0,V1>, E1 = <V1,V2>, E2 = <V2,V0>
        Edge* m_apkEdge[3];

        // adjacent triangles:  adjacent[i] shares edge[i]
        Triangle* m_apkAdjacent[3];
    };

    class WML_ITEM ConvexPolyhedron
    {
    public:
        int m_iNumVertices;
        Vertex* m_apkVertex;

        int m_iNumEdges;
        Edge* m_apkEdge;

        int m_iNumTriangles;
        Triangle* m_apkTriangle;

        // temporary storage, average of polyhedron vertices
        Vector3<Real> m_kCentroid;
    };

    static void TessellateSphere (int iSteps, ConvexPolyhedron& rkPoly);
    static void DeletePolyhedron (ConvexPolyhedron& rkPoly);

protected:
    Matrix3<Real> m_kA;
    Vector3<Real> m_kB;
    Real m_fC;

    // support for sphere tessellation
    static int VertexIndex (const ConvexPolyhedron& rkPoly,
        const Vertex* pkV);
    static int EdgeIndex (const ConvexPolyhedron& rkPoly, const Edge* pkE);
    static int TriangleIndex (const ConvexPolyhedron& rkPoly,
        const Triangle* pkT);
    static int AdjacentOrient (const Triangle* pkT, const Triangle* pkA);
    static void ComputeCentroid (ConvexPolyhedron& rkPoly, int iNumVertices);
    static void RayIntersectSphere (const Vector3<Real>& rkCen,
        const Vector3<Real>& rkMid, Vector3<Real>* pkIntersect);
    static void Expand (int iSteps, ConvexPolyhedron& rkPoly);
};

typedef QuadricSurface<float> QuadricSurfacef;
typedef QuadricSurface<double> QuadricSurfaced;

}

#endif
