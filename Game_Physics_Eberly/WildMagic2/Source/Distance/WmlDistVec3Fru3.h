// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3FRU3_H
#define WMLDISTVEC3FRU3_H

#include "WmlFrustum3.h"

namespace Wml
{

// The calculations assume that the frustum is a solid, so any point inside
// the frustum has distance zero.

// squared distance measurements
template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint,
    const Frustum3<Real>& rkFrustum, Vector3<Real>* pkClosest = NULL);

// distance measurements
template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Frustum3<Real>& rkFrustum, Vector3<Real>* pkClosest = NULL);

}

#endif
