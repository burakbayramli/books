// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBOX2_H
#define WMLBOX2_H

#include "WmlVector2.h"

namespace Wml
{

template <class Real>
class WML_ITEM Box2
{
public:
    Box2 ();

    Vector2<Real>& Center ();
    const Vector2<Real>& Center () const;

    Vector2<Real>& Axis (int i);
    const Vector2<Real>& Axis (int i) const;
    Vector2<Real>* Axes ();
    const Vector2<Real>* Axes () const;

    Real& Extent (int i);
    const Real& Extent (int i) const;
    Real* Extents ();
    const Real* Extents () const;

    void ComputeVertices (Vector2<Real> akVertex[4]) const;

protected:
    Vector2<Real> m_kCenter;
    Vector2<Real> m_akAxis[2];
    Real m_afExtent[2];
};

typedef Box2<float> Box2f;
typedef Box2<double> Box2d;

}

#endif
