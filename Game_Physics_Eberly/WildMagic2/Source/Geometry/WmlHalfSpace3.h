// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLHALFSPACE3_H
#define WMLHALFSPACE3_H

#include "WmlVector3.h"
#include "WmlPoint4.h"

namespace Wml
{

template <class Real>
class WML_ITEM HalfSpace3
{
public:
    // The halfspace is represented as Dot(N,X) >= c and has a boundary plane
    // Dot(N,X) = c where N is the plane normal vector, not necessarily unit
    // length, c is the plane constant, and X is any point on the normal side
    // of the plane.

    // N and c are uninitialized
    HalfSpace3 ();

    // N and c are specified
    HalfSpace3 (const Vector3<Real>& rkNormal, Real fConstant);

    // N is specified, c = Dot(N,P) where P is the input point
    HalfSpace3 (const Vector3<Real>& rkNormal, const Vector3<Real>& rkPoint);

    // N = Cross(P1-P0,P2-P0), c = Dot(N,P0) where P0, P1, P2 are input points
    HalfSpace3 (const Vector3<Real>& rkPoint0, const Vector3<Real>& rkPoint1,
        const Vector3<Real>& rkPoint2);

    // copy constructor
    HalfSpace3 (const HalfSpace3& rkHalfSpace);

    // member access
    void SetNormal (const Vector3<Real>& rkNormal);
    Vector3<Real> GetNormal () const;
    void SetConstant (Real fConstant);
    Real GetConstant () const;
    void Set (const Vector3<Real>& rkNormal, Real fConstant);
    void Get (Vector3<Real>& rkNormal, Real& rfConstant) const;

    // assignment
    HalfSpace3& operator= (const HalfSpace3& rkHalfSpace);

    // If the plane is Dot(N,X) = c, arrange for the normal vector to be
    // unit length.  The new equation is Dot(N/|N|,X) = c/|N|.
    void Normalize ();

protected:
    // Normal N = (nx,ny,nz), constant c, plane is Dot(N,X) = c.  The tuple
    // stores tuple[0] = nx, tuple[1] = ny, tuple[2] = nz, tuple[3] = c.
    Real m_afTuple[4];
};

typedef HalfSpace3<float> HalfSpace3f;
typedef HalfSpace3<double> HalfSpace3d;

}

#endif
