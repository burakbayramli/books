// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3ELD3_H
#define WMLDISTVEC3ELD3_H

// Input:   Ellipsoid (x/a)^2+(y/b)^2+(z/c)^2 = 1, point (u,v,w).
//          rkExtent = (a,b,x)
// Output:  Closest point (x,y,z) on ellipsoid to (u,v,w), function returns
//          the distance sqrt((x-u)^2+(y-v)^2+(z-w)^2).
//          rkPoint = (u,v,w), rkClosest = (x,y,z)
//
// Method sets up the distance as the maximum root to a sixth degree
// polynomial.  The root is found by Newton's method.  If the return value
// is -1, then the iterates failed to converge.

#include "WmlEllipsoid3.h"

namespace Wml
{

template <class Real>
WML_ITEM Real SqrDistance (const EllipsoidStandard3<Real>& rkEllipsoid,
    const Vector3<Real>& rkPoint, Vector3<Real>& rkClosest);

template <class Real>
WML_ITEM Real Distance (const EllipsoidStandard3<Real>& rkEllipsoid,
    const Vector3<Real>& rkPoint, Vector3<Real>& rkClosest);

}

#endif
