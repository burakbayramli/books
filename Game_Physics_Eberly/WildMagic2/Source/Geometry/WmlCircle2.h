// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCIRCLE2_H
#define WMLCIRCLE2_H

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Circle2
{
public:
    Circle2 ();

    Vector2<Real>& Center ();
    const Vector2<Real>& Center () const;

    Real& Radius ();
    const Real& Radius () const;

protected:
    Vector2<Real> m_kCenter;
    Real m_fRadius;
};

typedef Circle2<float> Circle2f;
typedef Circle2<double> Circle2d;

}

#endif
