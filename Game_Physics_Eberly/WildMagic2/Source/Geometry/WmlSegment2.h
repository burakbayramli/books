// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSEGMENT2_H
#define WMLSEGMENT2_H

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Segment2
{
public:
    // Segment is S(t) = P+t*D for 0 <= t <= 1.  D is not necessarily unit
    // length.  The end points are P and P+D.
    Segment2 ();

    Vector2<Real>& Origin ();
    const Vector2<Real>& Origin () const;

    Vector2<Real>& Direction ();
    const Vector2<Real>& Direction () const;

protected:
    Vector2<Real> m_kOrigin;  // P
    Vector2<Real> m_kDirection;  // D
};

typedef Segment2<float> Segment2f;
typedef Segment2<double> Segment2d;

}

#endif
