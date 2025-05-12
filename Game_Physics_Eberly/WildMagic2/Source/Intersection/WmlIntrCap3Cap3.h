// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRCAP3CAP3_H
#define WMLINTRCAP3CAP3_H

#include "WmlCapsule3.h"

namespace Wml
{

// Test for intersection of static capsules.
template <class Real>
WML_ITEM bool TestIntersection (const Capsule3<Real>& rkC0,
    const Capsule3<Real>& rkC1);

}

#endif
