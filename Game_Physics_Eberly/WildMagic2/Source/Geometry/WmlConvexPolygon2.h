// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONVEXPOLYGON2_H
#define WMLCONVEXPOLYGON2_H

#include "WmlLine2.h"
#include <vector>

namespace Wml
{

template <class Real>
class WML_ITEM ConvexPolygon2
{
public:
    typedef typename std::vector<Vector2<Real> > VArray;
    typedef typename std::vector<Line2<Real> > LArray;

    // construction
    ConvexPolygon2 ();
    ConvexPolygon2 (const VArray& rakPoint);
    ConvexPolygon2 (const VArray& rakPoint, const LArray& rakLine);
    ConvexPolygon2 (const ConvexPolygon2& rkPoly);
    void Create (const VArray& rakPoint);
    void Create (const VArray& rakPoint, const LArray& rakLine);

    ConvexPolygon2& operator= (const ConvexPolygon2& rkPoly);

    // read points and lines
    const VArray& GetPoints () const;
    const Vector2<Real>& GetPoint (int iV) const;
    const LArray& GetLines () const;
    const Line2<Real>& GetLine (int iT) const;

    // Allow vertex modification.  The caller is responsible for preserving
    // the convexity.  After modifying the vertices, call UpdateLines to
    // recompute the lines of the polygon edges.
    int AddPoint (const Vector2<Real>& rkPoint);
    VArray& Points ();
    Vector2<Real>& Point (int iV);
    void UpdateLines ();

    // Test for convexity:  This function will iterate over the edges of the
    // polygon and verify for each that the polygon vertices are all on the
    // nonnegative side of the line.  The threshold is the value that the line
    // distance d is compared to, d < 0.  In theory the distances should all
    // be nonegative.  Floating point round-off errors can cause some small
    // distances, so you might set fThreshold to a small negative number.
    bool ValidateHalfSpaceProperty (Real fThreshold = (Real)0.0) const;
    void ComputeCentroid ();
    const Vector2<Real>& GetCentroid () const;

    // discard the portion of the mesh on the negative side of the line
    bool Clip (const Line2<Real>& rkLine, ConvexPolygon2& rkIntr) const;

    // compute the polygon of intersection
    bool FindIntersection (const ConvexPolygon2& rkPoly,
        ConvexPolygon2& rkIntr) const;

    static void FindAllIntersections (int iQuantity, ConvexPolygon2* akPoly,
        int& riCombos, ConvexPolygon2**& rapkIntr);

    Real GetPerimeterLength () const;
    Real GetArea () const;
    bool ContainsPoint (const Vector2<Real>& rkP) const;

    // Create an egg-shaped object that is axis-aligned and centered at
    // (xc,yc).  The input bounds are all positive and represent the
    // distances from the center to the four extreme points on the egg.
    static void CreateEggShape (const Vector2<Real>& rkCenter, Real fX0,
        Real fX1, Real fY0, Real fY1, int iMaxSteps, ConvexPolygon2& rkEgg);

    // debugging support
    void Print (std::ofstream& rkOStr) const;
    bool Print (const char* acFilename) const;

protected:
    // support for intersection testing
    static ConvexPolygon2* FindSolidIntersection (
        const ConvexPolygon2& rkPoly0, const ConvexPolygon2& rkPoly1);
    static int GetHighBit (int i);

    VArray m_akPoint;
    LArray m_akLine;
    Vector2<Real> m_kCentroid;
};

typedef ConvexPolygon2<float> ConvexPolygon2f;
typedef ConvexPolygon2<double> ConvexPolygon2d;

}

#endif
