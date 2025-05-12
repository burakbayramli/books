// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRSPH3FRU3_H
#define WMLINTRSPH3FRU3_H

#include "WmlSphere3.h"
#include "WmlFrustum3.h"

namespace Wml
{

template <class Real>
WML_ITEM bool TestIntersection (const Sphere3<Real>& rkSphere,
    const Frustum3<Real>& rkFrustum);

}

#endif
