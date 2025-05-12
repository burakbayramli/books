// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLINE2_H
#define WMLLINE2_H

// Two different representations of the line are allowed.  If one form is
// used, the other is *not* automatically set.

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Line2
{
public:
    Line2 ();

    // parametric form:
    //   origin = V0, direction = V1-V0
    //
    // normal form:
    //   normal = Perp(V1-V0) where Perp(x,y)=(y,-x),
    //   constant = Dot(normal,V0)
    Line2 (const Vector2<Real>& rkV0, const Vector2<Real>& rkV1,
        bool bParametricForm);

    // Line is L(t) = P+t*D for any real-valued t.  D is not necessarily
    // unit length.
    Vector2<Real>& Origin ();
    const Vector2<Real>& Origin () const;
    Vector2<Real>& Direction ();
    const Vector2<Real>& Direction () const;

    // Line is Dot(N,X) = c.  N is not necessarily unit length.
    Vector2<Real>& Normal ();
    const Vector2<Real>& Normal () const;
    Real& Constant ();
    const Real& Constant () const;

    // Compute d = Dot(N,V)-c.  The value |d| is the true distance from V
    // to the line whenever |N| = 1.
    Real GetPseudodistance (const Vector2<Real>& rkV) const;

protected:
    // parametric form
    Vector2<Real> m_kOrigin;
    Vector2<Real> m_kDirection;

    // normal form
    Vector2<Real> m_kNormal;
    Real m_fConstant;
};

typedef Line2<float> Line2f;
typedef Line2<double> Line2d;

}

#endif
