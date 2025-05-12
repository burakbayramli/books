// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRSPH3CON3_H
#define WMLINTRSPH3CON3_H

#include "WmlCone3.h"
#include "WmlSphere3.h"

namespace Wml
{

template <class Real>
WML_ITEM bool TestIntersection (const Sphere3<Real>& rkSphere,
    const Cone3<Real>& rkCone);

// If an intersection occurs, the point of intersection closest to the cone
// vertex is returned.

template <class Real>
WML_ITEM bool FindIntersection (const Sphere3<Real>& rkSphere,
    const Cone3<Real>& rkCone, Vector3<Real>& rkClosest);

}

#endif
