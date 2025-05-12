// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTRIANGLE2_H
#define WMLTRIANGLE2_H

// Two different representations of the triangle are allowed.  If one form is
// used, the other is *not* automatically set.

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Triangle2
{
public:
    Triangle2 ();

    // Triangle points are tri(s,t) = b+s*e0+t*e1 where 0 <= s <= 1,
    // 0 <= t <= 1, and 0 <= s+t <= 1.
    Vector2<Real>& Origin ();
    const Vector2<Real>& Origin () const;
    Vector2<Real>& Edge0 ();
    const Vector2<Real>& Edge0 () const;
    Vector2<Real>& Edge1 ();
    const Vector2<Real>& Edge1 () const;

    // Triangle points are V0, V1, V2.
    Vector2<Real>& Vertex (int i);
    const Vector2<Real>& Vertex (int i) const;

protected:
    // parametric form
    Vector2<Real> m_kOrigin;
    Vector2<Real> m_kEdge0;
    Vector2<Real> m_kEdge1;

    // vertex form
    Vector2<Real> m_akV[3];
};

typedef Triangle2<float> Triangle2f;
typedef Triangle2<double> Triangle2d;

}

#endif
