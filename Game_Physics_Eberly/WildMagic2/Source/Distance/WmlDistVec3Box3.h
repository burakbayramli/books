// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3BOX3_H
#define WMLDISTVEC3BOX3_H

#include "WmlBox3.h"

namespace Wml
{

// Use these for oriented bounding boxes.  If you provide three non-null
// pointers pfBParam0, pfBParam1, pfBParam2, both functions also return the
// closest point in box coordinates.  That is, if the box has center C and
// axes U0, U1, and U2, the closest point to the input point is
//   K = C + (*pfBParam0)*U0 + (*pfBParam1)*U1 + (*pfBParam2)*U2;

template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint,
    const Box3<Real>& rkBox, Real* pfBParam0 = NULL, Real* pfBParam1 = NULL,
    Real* pfBParam2 = NULL);

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Box3<Real>& rkBox, Real* pfBParam0 = NULL, Real* pfBParam1 = NULL,
    Real* pfBParam2 = NULL);

// Use these for axis-aligned bounding boxes.  A point is in the box whenever
// xmin <= x <= xmax and ymin <= y <= ymax and zmin <= z <= zmax.  If you
// provide a non-null pointer pkClosest, both functions also return the
// closest point.

template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint, Real fXMin,
    Real fXMax, Real fYMin, Real fYMax, Real fZMin, Real fZMax,
    Vector3<Real>* pkClosest = NULL);

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint, Real fXMin,
    Real fXMax, Real fYMin, Real fYMax, Real fZMin, Real fZMax,
    Vector3<Real>* pkClosest = NULL);

}

#endif
