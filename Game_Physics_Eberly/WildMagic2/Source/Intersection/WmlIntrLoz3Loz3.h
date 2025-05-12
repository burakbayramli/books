// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLOZ3LOZ3_H
#define WMLINTRLOZ3LOZ3_H

#include "WmlLozenge3.h"

namespace Wml
{

// Test for intersection of static lozenges.
template <class Real>
WML_ITEM bool TestIntersection (const Lozenge3<Real>& rkL0,
    const Lozenge3<Real>& rkL1, Real fEpsilon = (Real)0.0);

}

#endif
