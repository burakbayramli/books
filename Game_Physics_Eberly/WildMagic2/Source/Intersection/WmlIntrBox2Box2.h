// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRBOX2BOX2_H
#define WMLINTRBOX2BOX2_H

#include "WmlBox2.h"

namespace Wml
{

// boxes are stationary

template <class Real>
WML_ITEM bool TestIntersection (const Box2<Real>& rkBox0,
    const Box2<Real>& rkBox1);

}

#endif
