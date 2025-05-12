// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDELAUNAY3_H
#define WMLDELAUNAY3_H

// The Delaunay triangulation method is a modification of code written by 
// Dave Watson.  It uses an algorithm described in
//
//     Watson, D.F., 1981, Computing the n-dimensional Delaunay 
//     tessellation with application to Voronoi polytopes: 
//     The Computer J., 24(2), p. 167-172.

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Delaunay3
{
public:
    // Construction and destruction.  In the first constructor,
    // Delaunay3D accepts ownership of the input array and will
    // delete it during destruction.  The second constructor is designed to
    // allow sharing of the network (note that the reference argument is not
    // passed as 'const').  Any other network that shares this object will
    // not delete the data in this object.
    Delaunay3 (int iVertexQuantity, Vector3<Real>* akVertex);
    Delaunay3 (Delaunay3& rkNet);
    virtual ~Delaunay3 ();
    bool IsOwner () const;

    // vertices
    int GetVertexQuantity () const;
    const Vector3<Real>& GetVertex (int i) const;
    const Vector3<Real>* GetVertices () const;
    Real GetXMin () const;
    Real GetXMax () const;
    Real GetXRange () const;
    Real GetYMin () const;
    Real GetYMax () const;
    Real GetYRange () const;
    Real GetZMin () const;
    Real GetZMax () const;
    Real GetZRange () const;

    // tetrahedra
    class WML_ITEM Tetrahedron
    {
    public:
        int m_aiVertex[4];
    };

    int GetTetrahedronQuantity () const;
    Tetrahedron& GetTetrahedron (int i);
    const Tetrahedron& GetTetrahedron (int i) const;
    Tetrahedron* GetTetrahedrons ();
    const Tetrahedron* GetTetrahedrons () const;

    // Barycentric coordinates of P are P = b0*V0+b1*V1+b2*V2+b3*V3 where
    // b0+b1+b2+b3 = 1.  The coordinates are b[i] = n[i]/d.  This function
    // computes the numerators and denominator.  The application must handle
    // the division (if it actually needs to divide).  InTetrahedron uses this
    // function, but does not require the division.
    static void ComputeBarycenter (const Vector3<Real>& rkV0,
        const Vector3<Real>& rkV1, const Vector3<Real>& rkV2,
        const Vector3<Real>& rkV3, const Vector3<Real>& rkP, Real afNumer[4],
        Real& rfDenom);

    // Test if P is in the tetrahedron formed by V0, V1, V2, and V3.
    static bool InTetrahedron (const Vector3<Real>& rkV0,
        const Vector3<Real>& rkV1,  const Vector3<Real>& rkV2,
        const Vector3<Real>& rkV3, const Vector3<Real>& rkP);

    // Test if P is in the tetrahedron, but ComputeBarycenter was called first
    // to determine the coordinates of P with respect to the tetrahedron.
    static bool InTetrahedron (const Real afNumer[4], Real fDenom);

    // tweaking parameters
    static Real& Epsilon ();     // default = 0.00001
    static Real& Range ();       // default = 10.0
    static int& TSize ();           // default = 75
    static int& QuantityFactor ();  // default = 16

protected:
    // for sharing
    bool m_bOwner;

    // vertices
    int m_iVertexQuantity;
    Vector3<Real>* m_akVertex;
    Real m_fXMin, m_fXMax, m_fXRange;
    Real m_fYMin, m_fYMax, m_fYRange;
    Real m_fZMin, m_fZMax, m_fZRange;

    // tetrahedra
    int m_iTetrahedronQuantity;
    Tetrahedron* m_akTetrahedron;

    // tweaking parameters
    static Real ms_fEpsilon;
    static Real ms_fRange;
    static int ms_iTSize;
    static int ms_iQuantityFactor;
};

typedef Delaunay3<float> Delaunay3f;
typedef Delaunay3<double> Delaunay3d;

}

#endif
