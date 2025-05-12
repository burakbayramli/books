// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSEGMENT3_H
#define WMLSEGMENT3_H

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
class WML_ITEM Segment3
{
public:
    // Segment is S(t) = P+t*D for 0 <= t <= 1.  D is not necessarily unit
    // length.  The end points are P and P+D.
    Segment3 ();

    Vector3<Real>& Origin ();
    const Vector3<Real>& Origin () const;

    Vector3<Real>& Direction ();
    const Vector3<Real>& Direction () const;

protected:
    Vector3<Real> m_kOrigin;  // P
    Vector3<Real> m_kDirection;  // D
};

typedef Segment3<float> Segment3f;
typedef Segment3<double> Segment3d;

}

#endif
