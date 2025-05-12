// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3CIR3_H
#define WMLDISTVEC3CIR3_H

#include "WmlCircle3.h"
#include "WmlDisk3.h"

namespace Wml
{

// Compute the distance from 'p' to 'circle'.  Returned value is distance.
// The closest circle point is returned as 'closest' if it is unique.  When
// 'p' is on the normal line C+t*N where C is circle center and N is the
// normal to plane containing circle, then all circle points are equidistant
// from 'p'.  In this case the returned point is (infinity,infinity,infinity).

template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkClosest = NULL);

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkClosest = NULL);

// Compute the distance from 'p' to 'disk' (circle plus its interior).
// Returned value is distance.  The closest disk point is returned as
// 'closest' (always unique).

template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint,
    const Disk3<Real>& rkDisk, Vector3<Real>* pkClosest = NULL);

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Disk3<Real>& rkDisk, Vector3<Real>* pkClosest = NULL);

}

#endif
