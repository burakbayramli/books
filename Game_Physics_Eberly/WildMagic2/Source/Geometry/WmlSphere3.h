// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSPHERE3_H
#define WMLSPHERE3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Sphere3
{
public:
    Sphere3 ();

    Vector3<Real>& Center ();
    const Vector3<Real>& Center () const;

    Real& Radius ();
    const Real& Radius () const;

protected:
    Vector3<Real> m_kCenter;
    Real m_fRadius;
};

typedef Sphere3<float> Sphere3f;
typedef Sphere3<double> Sphere3d;

}

#endif
