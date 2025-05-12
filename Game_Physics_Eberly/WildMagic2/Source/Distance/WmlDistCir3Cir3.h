// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTCIR3CIR3_H
#define WMLDISTCIR3CIR3_H

#include "WmlCircle3.h"

// Compute the distance between two circles.  In many configurations, there
// is exactly one pair of points that attains minimal distance.  However,
// there are configurations for which two pair of points attain minimal
// distance, for example when the circles are not coplanar and share the same
// center or when the circles are not coplanar yet parallel and their
// projections onto a single parallel plane intersect in two points.  Finally,
// when the two circles are coplanar and share the same center, there are
// infinitely many pairs of points that are equidistant with minimum distance.

namespace Wml
{

template <class Real>
WML_ITEM Real SqrDistance (const Circle3<Real>& rkCircle0,
    const Circle3<Real>& rkCircle1, Vector3<Real>* pkClosest0 = NULL,
    Vector3<Real>* pkClosest1 = NULL);

template <class Real>
WML_ITEM Real Distance (const Circle3<Real>& rkCircle0,
    const Circle3<Real>& rkCircle1, Vector3<Real>* pkClosest0 = NULL,
    Vector3<Real>* pkClosest1 = NULL);

}

#endif
