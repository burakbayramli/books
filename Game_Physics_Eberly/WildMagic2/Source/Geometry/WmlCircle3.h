// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCIRCLE3_H
#define WMLCIRCLE3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Circle3
{
public:
    // Plane containing circle is Dot(N,X-C) = 0 where X is any point in the
    // plane.  Vectors U, V, and N form an orthonormal right-handed set
    // (matrix [U V N] is orthonormal and has determinant 1).  Circle within
    // the plane is parameterized by X = C + R*(cos(A)*U + sin(A)*V) where
    // A is an angle in [0,2*pi).

    Circle3 ();

    Vector3<Real>& U ();
    const Vector3<Real>& U () const;

    Vector3<Real>& V ();
    const Vector3<Real>& V () const;

    Vector3<Real>& N ();
    const Vector3<Real>& N () const;

    Vector3<Real>& Center ();
    const Vector3<Real>& Center () const;

    Real& Radius ();
    const Real& Radius () const;

protected:
    Vector3<Real> m_kU, m_kV, m_kN;
    Vector3<Real> m_kCenter;
    Real m_fRadius;
};

typedef Circle3<float> Circle3f;
typedef Circle3<double> Circle3d;

}

#endif
