// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONVEXPOLYHEDRON3_H
#define WMLCONVEXPOLYHEDRON3_H

#include "WmlMTMesh.h"
#include "WmlPlane3.h"
#include "WmlVector2.h"
#include <set>
#include <vector>

namespace Wml
{

template <class Real>
class WML_ITEM ConvexPolyhedron3 : public MTMesh
{
public:
    typedef typename std::vector<Vector2<Real> > V2Array;
    typedef typename std::vector<Vector3<Real> > V3Array;
    typedef typename std::vector<Plane3<Real> > PArray;
    typedef typename std::vector<int> IArray;

    // construction
    ConvexPolyhedron3 ();
    ConvexPolyhedron3 (const V3Array& rakPoint, const IArray& raiConnect);
    ConvexPolyhedron3 (const V3Array& rakPoint, const IArray& raiConnect,
        const PArray& rakPlane);
    ConvexPolyhedron3 (const ConvexPolyhedron3& rkPoly);

    void Create (const V3Array& rakPoint, const IArray& raiConnect);
    void Create (const V3Array& rakPoint, const IArray& raiConnect,
        const PArray& rakPlane);

    ConvexPolyhedron3& operator= (const ConvexPolyhedron3& rkPoly);

    // read points and planes
    const V3Array& GetPoints () const;
    const Vector3<Real>& GetPoint (int iV) const;
    const PArray& GetPlanes () const;
    const Plane3<Real>& GetPlane (int iT) const;

    // Allow vertex modification.  The caller is responsible for preserving
    // the convexity.  After modifying the vertices, call UpdatePlanes to
    // recompute the planes of the polyhedron faces.
    int AddPoint (const Vector3<Real>& rkPoint);
    V3Array& Points ();
    Vector3<Real>& Point (int iV);
    void UpdatePlanes ();

    // Test for convexity:  Assuming the application has guaranteed that the
    // mesh is manifold and closed, this function will iterate over the faces
    // of the polyhedron and verify for each that the polyhedron vertices are
    // all on the nonnegative side of the plane.  The threshold is the value
    // that the plane distance d is compared to, d < 0.  In theory the
    // distances should all be nonegative.  Floating point round-off errors
    // can cause some small distances, so you might set fThreshold to a small
    // negative number.
    bool ValidateHalfSpaceProperty (Real fThreshold = (Real)0.0) const;
    void ComputeCentroid ();
    const Vector3<Real>& GetCentroid () const;

    // discard the portion of the mesh on the negative side of the plane
    bool Clip (const Plane3<Real>& rkPlane, ConvexPolyhedron3& rkIntr) const;

    // compute the polyhedron of intersection
    bool FindIntersection (const ConvexPolyhedron3& rkPoly,
        ConvexPolyhedron3& rkIntr) const;

    static void FindAllIntersections (int iQuantity,
        ConvexPolyhedron3* akPoly, int& riCombos,
        ConvexPolyhedron3**& rapkIntr);

    Real GetSurfaceArea () const;
    Real GetVolume () const;
    bool ContainsPoint (const Vector3<Real>& rkP) const;

    // The eye point must be outside the polyhedron.  The output is the
    // terminator, an ordered list of vertices forming a simple closed
    // polyline that separates the visible from invisible faces of the
    // polyhedron.
    void ComputeTerminator (const Vector3<Real>& rkEye,
        V3Array& rkTerminator);

    // If projection plane is Dot(N,X) = c where N is unit length, then the
    // application must ensure that Dot(N,eye) > c.  That is, the eye point is
    // on the side of the plane to which N points.  The application must also
    // specify two vectors U and V in the projection plane so that {U,V,N} is
    // a right-handed and orthonormal set (the matrix [U V N] is orthonormal
    // with determinant 1).  The origin of the plane is computed internally as
    // the closest point to the eye point (an orthogonal pyramid for the
    // perspective projection).  If all vertices P on the terminator satisfy
    // Dot(N,P) < Dot(N,eye), then the polyhedron is completely visible (in
    // the sense of perspective projection onto the viewing plane).  In this
    // case the silhouette is computed by projecting the terminator points
    // onto the viewing plane.  The return value of the function is 'true'
    // when this happens.  However, if at least one terminator point P
    // satisfies Dot(N,P) >= Dot(N,eye), then the silhouette is unbounded in
    // the view plane.  It is not computed and the function returns 'false'.
    // A silhouette point (x,y) is extracted from the point Q that is the
    // intersection of the ray whose origin is the eye point and that contains
    // a terminator point, Q = K+x*U+y*V+z*N where K is the origin of the
    // plane.
    bool ComputeSilhouette (const Vector3<Real>& rkEye,
        const Plane3<Real>& rkPlane, const Vector3<Real>& rkU,
        const Vector3<Real>& rkV, V2Array& rkSilhouette);

    bool ComputeSilhouette (V3Array& rkTerminator,
        const Vector3<Real>& rkEye, const Plane3<Real>& rkPlane,
        const Vector3<Real>& rkU, const Vector3<Real>& rkV,
        V2Array& rkSilhouette);

    // Create an egg-shaped object that is axis-aligned and centered at
    // (xc,yc,zc).  The input bounds are all positive and represent the
    // distances from the center to the six extreme points on the egg.
    static void CreateEggShape (const Vector3<Real>& rkCenter, Real fX0,
        Real fX1, Real fY0, Real fY1, Real fZ0, Real fZ1, int iMaxSteps,
        ConvexPolyhedron3& rkEgg);

    // debugging support
    virtual void Print (std::ofstream& rkOStr) const;
    virtual bool Print (const char* acFilename) const;

protected:
    // support for intersection testing
    static ConvexPolyhedron3* FindSolidIntersection (
        const ConvexPolyhedron3& rkPoly0, const ConvexPolyhedron3& rkPoly1);
    static int GetHighBit (int i);

    // support for computing surface area
    Real GetTriangleArea (const Vector3<Real>& rkN, const Vector3<Real>& rkV0,
        const Vector3<Real>& rkV1, const Vector3<Real>& rkV2) const;

    // support for computing the terminator and silhouette
    Real GetDistance (const Vector3<Real>& rkEye, int iT,
        std::vector<Real>& rafDistance) const;
    static bool IsNegativeProduct (Real fDist0, Real fDist1);

    V3Array m_akPoint;
    PArray m_akPlane;
    Vector3<Real> m_kCentroid;
};

typedef ConvexPolyhedron3<float> ConvexPolyhedron3f;
typedef ConvexPolyhedron3<double> ConvexPolyhedron3d;

}

#endif
