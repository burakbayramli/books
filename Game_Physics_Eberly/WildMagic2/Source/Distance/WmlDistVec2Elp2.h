// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC2ELP2_H
#define WMLDISTVEC2ELP2_H

// Input:   Ellipse (x/a)^2+(y/b)^2 = 1, point (u,v).
//          rkExtent = (a,b)
// Output:  Closest point (x,y) on ellipse to (u,v), function returns
//          the distance sqrt((x-u)^2+(y-v)^2).
//          rkPoint = (u,v), rkClosest = (x,y)
//
// Method sets up the distance as the maximum root to a fourth degree
// polynomial.  The root is found by Newton's method.  If the return value
// is -1, then the iterates failed to converge.

#include "WmlEllipse2.h"

namespace Wml
{

template <class Real>
WML_ITEM Real SqrDistance (const EllipseStandard2<Real>& rkEllipse,
    const Vector2<Real>& rkPoint, Vector2<Real>& rkClosest);

template <class Real>
WML_ITEM Real Distance (const EllipseStandard2<Real>& rkEllipse,
    const Vector2<Real>& rkPoint, Vector2<Real>& rkClosest);

}

#endif
