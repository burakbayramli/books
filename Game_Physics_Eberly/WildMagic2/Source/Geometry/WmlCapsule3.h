// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCAPSULE3_H
#define WMLCAPSULE3_H

#include "WmlSegment3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Capsule3
{
public:
    Capsule3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& Direction ();
    const Vector3<Real>& Direction () const;

    Real& Radius ();
    const Real& Radius () const;

    Segment3<Real>& Segment ();
    const Segment3<Real>& Segment () const;

protected:
    Segment3<Real> m_kSegment;
    Real m_fRadius;
};

typedef Capsule3<float> Capsule3f;
typedef Capsule3<double> Capsule3d;

}

#endif
