// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRPLN3PLN3_H
#define WMLINTRPLN3PLN3_H

#include "WmlLine3.h"
#include "WmlPlane3.h"

namespace Wml
{

template <class Real>
WML_ITEM bool TestIntersection (const Plane3<Real>& rkPlane0,
    const Plane3<Real>& rkPlane1);

template <class Real>
WML_ITEM bool FindIntersection (const Plane3<Real>& rkPlane0,
    const Plane3<Real>& rkPlane1, Line3<Real>& rkLine);

}

#endif
