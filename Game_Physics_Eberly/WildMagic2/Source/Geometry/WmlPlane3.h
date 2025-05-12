// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPLANE3_H
#define WMLPLANE3_H

#include "WmlVector3.h"
#include "WmlPoint4.h"

namespace Wml
{

template <class Real>
class WML_ITEM Plane3
{
public:
    // The plane is represented as Dot(N,X) = c where N is the plane normal
    // vector, not necessarily unit length, c is the plane constant, and X is
    // any point on the plane.

    // N and c are uninitialized
    Plane3 ();

    // N and c are specified
    Plane3 (const Vector3<Real>& rkNormal, Real fConstant);

    // N is specified, c = Dot(N,P) where P is the input point
    Plane3 (const Vector3<Real>& rkNormal, const Vector3<Real>& rkPoint);

    // N = Cross(P1-P0,P2-P0), c = Dot(N,P0) where P0, P1, P2 are input points
    Plane3 (const Vector3<Real>& rkPoint0, const Vector3<Real>& rkPoint1,
        const Vector3<Real>& rkPoint2);

    // copy constructor
    Plane3 (const Plane3& rkPlane);

    // member access
    void SetNormal (const Vector3<Real>& rkNormal);
    Vector3<Real> GetNormal () const;
    void SetConstant (Real fConstant);
    Real GetConstant () const;
    void Set (const Vector3<Real>& rkNormal, Real fConstant);
    void Get (Vector3<Real>& rkNormal, Real& rfConstant) const;

    // access plane P as P[0] = N.x, P[1] = N.y, P[2] = N.z, P[3] = c
    operator const Real* () const;
    operator Real* ();
    Real operator[] (int i) const;
    Real& operator[] (int i);

    // Access plane as homogeneous vector (N.x,N.y,N.z,-c) for use in
    // multiplication by 4-by-4 homogeneous matrix.
    operator Point4<Real> ();

    // assignment
    Plane3& operator= (const Plane3& rkPlane);

    // The "positive side" of the plane is the half space to which the plane
    // normal points.  The "negative side" is the other half space.  The flag
    // "no side" indicates the plane itself.
    enum Side
    {
        NO_SIDE,
        POSITIVE_SIDE,
        NEGATIVE_SIDE
    };

    Side WhichSide (const Vector3<Real>& rkPoint) const;

    // This is a pseudodistance.  The sign of the return value is positive if
    // the point is on the positive side of the plane, negative if the point
    // is on the negative side, and zero if the point is on the plane.  The
    // absolute value of the return value is the true distance only when the
    // plane normal is a unit length vector.
    Real DistanceTo (const Vector3<Real>& rkPoint) const;

    // If the plane is Dot(N,X) = c, arrange for the normal vector to be
    // unit length.  The new equation is Dot(N/|N|,X) = c/|N|.
    void Normalize ();

protected:
    // Normal N = (nx,ny,nz), constant c, plane is Dot(N,X) = c.  The tuple
    // stores tuple[0] = nx, tuple[1] = ny, tuple[2] = nz, tuple[3] = c.
    Real m_afTuple[4];
};

typedef Plane3<float> Plane3f;
typedef Plane3<double> Plane3d;

}

#endif
