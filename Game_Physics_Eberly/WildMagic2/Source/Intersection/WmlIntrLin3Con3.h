// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3CON3_H
#define WMLINTRLIN3CON3_H

#include "WmlCone3.h"
#include "WmlLine3.h"

namespace Wml
{

// The cone is assumed to have an acute angle between cone axis and cone edge.
// The return value is 'true' if and only if there is an intersection.  If
// there is an intersection, the number of intersections is stored in
// riQuantity.

// It is possible half the line is entirely on the cone surface.  In this
// case, the riQuantity is set to -1 and akPoint[] values are unassigned (the
// ray of intersection is V+(t*Dot(D,A))*D where V is the cone vertex, D is
// the line direction, and t >= 0.

template <class Real>
WML_ITEM bool FindIntersection (const Line3<Real>& rkLine,
    const Cone3<Real>& rkCone, int& riQuantity, Vector3<Real> akPoint[2]);

}

#endif
