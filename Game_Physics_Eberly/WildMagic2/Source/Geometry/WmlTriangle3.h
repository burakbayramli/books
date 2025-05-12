// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTRIANGLE3_H
#define WMLTRIANGLE3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Triangle3
{
public:
    // Triangle points are tri(s,t) = b+s*e0+t*e1 where 0 <= s <= 1,
    // 0 <= t <= 1, and 0 <= s+t <= 1.

    Triangle3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& Edge0 ();
    const Vector3<Real>& Edge0 () const;

    Vector3<Real>& Edge1 ();
    const Vector3<Real>& Edge1 () const;

protected:
    Vector3<Real> m_kOrigin;
    Vector3<Real> m_kEdge0;
    Vector3<Real> m_kEdge1;
};

typedef Triangle3<float> Triangle3f;
typedef Triangle3<double> Triangle3d;

}

#endif
