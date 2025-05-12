// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRPLN3LOZ3_H
#define WMLINTRPLN3LOZ3_H

#include "WmlLozenge3.h"
#include "WmlPlane3.h"

namespace Wml
{

// The boolean bUnitNormal is a hint about whether or not the plane normal
// is unit length.  If it is not, the length must be calculated by these
// routines.  For batch calls, the plane normal should be unitized in advance
// to avoid the expensive length calculation.

template <class Real>
WML_ITEM bool TestIntersection (const Plane3<Real>& rkPlane,
    const Lozenge3<Real>& rkLozenge, bool bUnitNormal);

// Culling support.  View frustum is assumed to be on the positive side of
// the plane.  Lozenge is culled if it is on the negative side.

template <class Real>
WML_ITEM bool Culled (const Plane3<Real>& rkPlane,
    const Lozenge3<Real>& rkLozenge, bool bUnitNormal);

}

#endif
