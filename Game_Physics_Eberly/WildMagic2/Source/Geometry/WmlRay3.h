// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRAY3_H
#define WMLRAY3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Ray3
{
public:
    // Ray is R(t) = P+t*D for t >= 0.  D is not necessarily unit length.
    Ray3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& Direction ();
    const Vector3<Real>& Direction () const;

protected:
    Vector3<Real> m_kOrigin;  // P
    Vector3<Real> m_kDirection;  // D
};

typedef Ray3<float> Ray3f;
typedef Ray3<double> Ray3d;

}

#endif
