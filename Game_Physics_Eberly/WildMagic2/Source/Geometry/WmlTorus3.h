// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTORUS_H
#define WMLTORUS_H

// Implementation of torus centered at (0,0,0) with z-axis as the
// axis of symmetry (axis about which circle is rotated to generate
// the torus).
//
// algebraic form
//      Ro > 0 is radius from center of torus
//      Ri > 0 is radius of tube of torus
//      p^2 = x^2+y^2+z^2
//      p^4-2*(Ro^2+Ri^2)*p^2+4*Ro^2*z^2+(Ro^2-Ri^2)^2 = 0
//
// parametric form
//      0 <= s <= 1, 0 <= t <= 1
//      Rc = Ro+Ri*cos(2*PI*t)
//      x = Rc*cos(2*PI*s)
//      y = Rc*sin(2*PI*s)
//      z = Ri*sin(2*PI*t)

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Torus3
{
public:
    Torus3 (Real fRo, Real fRi);

    const Real& Ro () const;
    const Real& Ri () const;

    Vector3<Real> Position (Real fS, Real fT);
    Vector3<Real> Normal (Real fS, Real fT);

    // for use in intersection testing
    void GetParameters (const Vector3<Real>& rkPos, Real& rfS,
        Real& rfT) const;

protected:
    Real m_fRo, m_fRi;
};

typedef Torus3<float> Torus3f;
typedef Torus3<double> Torus3d;

}

#endif
