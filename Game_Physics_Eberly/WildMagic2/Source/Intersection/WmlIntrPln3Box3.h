// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRPLN3BOX3_H
#define WMLINTRPLN3BOX3_H

#include "WmlBox3.h"
#include "WmlPlane3.h"

namespace Wml
{

template <class Real>
WML_ITEM bool TestIntersection (const Plane3<Real>& rkPlane,
    const Box3<Real>& rkBox);

// Culling support.  View frustum is assumed to be on the positive side of
// the plane.  Box is culled if it is on the negative side.
template <class Real>
WML_ITEM bool Culled (const Plane3<Real>& rkPlane, const Box3<Real>& rkBox);

}

#endif
