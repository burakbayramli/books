// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTMINBOX2_H
#define WMLCONTMINBOX2_H

#include "WmlBox2.h"

namespace Wml
{

// Compute minimum area oriented box containing the specified points.  The
// algorithm uses the rotating calipers method.  NOTE.  The input points must
// form a convex polygon and be in counterclockwise order.

template <class Real>
WML_ITEM Box2<Real> MinBox (int iQuantity, const Vector2<Real>* akPoint);

// The slower method for computing the minimum area oriented box that does not
// maintain the extremal points supporting the box (like rotating calipers
// does).  The input points must also form a convex polygon, but the order may
// be counterclockwise or clockwise.

template <class Real>
WML_ITEM Box2<Real> MinBoxOrderNSqr (int iQuantity,
    const Vector2<Real>* akPoint);

}

#endif
