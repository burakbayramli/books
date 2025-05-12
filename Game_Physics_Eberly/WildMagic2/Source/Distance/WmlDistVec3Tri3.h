// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3TRI3_H
#define WMLDISTVEC3TRI3_H

#include "WmlTriangle3.h"

namespace Wml
{

// squared distance measurements
template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint,
    const Triangle3<Real>& rkTri, Real* pfSParam = NULL,
    Real* pfTParam = NULL);

// distance measurements
template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint,
    const Triangle3<Real>& rkTri, Real* pfSParam = NULL,
    Real* pfTParam = NULL);

}

#endif
