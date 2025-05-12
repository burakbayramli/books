// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISK2_H
#define WMLDISK2_H

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Disk2
{
public:
    // solid disk |X-C| <= R

    Disk2 ();

    Vector2<Real>& Center ();
    const Vector2<Real>& Center () const;

    Real& Radius ();
    const Real& Radius () const;

protected:
    Vector2<Real> m_kCenter;
    Real m_fRadius;
};

typedef Disk2<float> Disk2f;
typedef Disk2<double> Disk2d;

}

#endif
