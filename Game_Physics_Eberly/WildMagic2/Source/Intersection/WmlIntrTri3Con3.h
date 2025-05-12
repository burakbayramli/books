// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRTRI3CON3_H
#define WMLINTRTRI3CON3_H

#include "WmlCone3.h"
#include "WmlTriangle3.h"

namespace Wml
{

// The cone is assumed to have an acute angle between cone axis and cone edge.
// The return value is 'true' if and only if there is an intersection.

template <class Real>
WML_ITEM bool TestIntersection (const Triangle3<Real>& rkTri,
    const Cone3<Real>& rkCone);

}

#endif
