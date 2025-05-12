// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRAY2_H
#define WMLRAY2_H

#include "WmlVector2.h"

namespace Wml 
{

template <class Real>
class WML_ITEM Ray2
{
public:
    // Ray is R(t) = P+t*D for t >= 0.  D is not necessarily unit length.
    Ray2 ();

    Vector2<Real>& Origin ();
    const Vector2<Real>& Origin () const;

    Vector2<Real>& Direction ();
    const Vector2<Real>& Direction () const;

protected:
    Vector2<Real> m_kOrigin;  // P
    Vector2<Real> m_kDirection;  // D
};

typedef Ray2<float> Ray2f;
typedef Ray2<double> Ray2d;

}

#endif
