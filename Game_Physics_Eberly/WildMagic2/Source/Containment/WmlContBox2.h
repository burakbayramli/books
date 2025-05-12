// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTBOX2_H
#define WMLCONTBOX2_H

#include "WmlBox2.h"

namespace Wml
{

template <class Real>
WML_ITEM void ContAlignedBox (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkMin, Vector2<Real>& rkMax);

template <class Real>
WML_ITEM Box2<Real> ContOrientedBox (int iQuantity,
    const Vector2<Real>* akPoint);


// This function allows for selection of vertices from a pool.  The return
// value is 'true' if and only if at least one vertex is valid.

template <class Real>
WML_ITEM bool ContOrientedBox (int iQuantity, const Vector2<Real>* akPoint,
    const bool* abValid, Box2<Real>& rkBox);

}

#endif
