// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONE3_H
#define WMLCONE3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Cone3
{
public:
    // An acute cone is Dot(A,X-V) = |X-V| cos(T) where V is the vertex, A
    // is the unit-length direction of the axis of the cone, and T is the
    // cone angle with 0 < T < PI/2.  The cone interior is defined by the
    // inequality Dot(A,X-V) >= |X-V| cos(T).  Since cos(T) > 0, we can avoid
    // computing square roots.  The solid cone is defined by the inequality
    // Dot(A,X-V)^2 >= Dot(X-V,X-V) cos(T)^2.

    Cone3 ();

    Vector3<Real>& Vertex ();
    const Vector3<Real>& Vertex () const;
    Vector3<Real>& Axis ();
    const Vector3<Real>& Axis () const;
    Real& CosAngle ();
    const Real& CosAngle () const;
    Real& SinAngle ();
    const Real& SinAngle () const;

protected:
    Vector3<Real> m_kVertex;
    Vector3<Real> m_kAxis;
    Real m_fCos, m_fSin;  // cos(T), sin(T)
};

typedef Cone3<float> Cone3f;
typedef Cone3<double> Cone3d;

}

#endif
