// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLINE3_H
#define WMLLINE3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Line3
{
public:
    // Line is L(t) = P+t*D for any real-valued t.  D is not necessarily
    // unit length.
    Line3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& Direction ();
    const Vector3<Real>& Direction () const;

protected:
    Vector3<Real> m_kOrigin;  // P
    Vector3<Real> m_kDirection;  // D
};

typedef Line3<float> Line3f;
typedef Line3<double> Line3d;

}

#endif



