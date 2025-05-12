// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLARC2_H
#define WMLARC2_H

#include "WmlCircle2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Arc2 : public Circle2<Real>
{
public:
    // The arc is defined by two points End0 and End1 on the circle so that
    // End1 is obtained from End0 by traversing counterclockwise.  The
    // application is responsible for ensuring that End0 and End1 are on the
    // circle and that they are properly ordered.

    Arc2 ();

    Vector2<Real>& End0 ();
    const Vector2<Real>& End0 () const;

    Vector2<Real>& End1 ();
    const Vector2<Real>& End1 () const;

    // Test if P is on the arc.  The application must ensure that P is on the
    // circle; that is, |P-C| = R.  This test works regardless of the angle
    // between B-C and A-C.
    bool Contains (const Vector2<Real>& rkP) const;

protected:
    Vector2<Real> m_kEnd0, m_kEnd1;
};

typedef Arc2<float> Arc2f;
typedef Arc2<double> Arc2d;

}

#endif
