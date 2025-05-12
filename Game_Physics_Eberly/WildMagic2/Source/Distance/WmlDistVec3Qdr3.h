// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTVEC3QDR3_H
#define WMLDISTVEC3QDR3_H

// Given a quadric surface defined implicitly by
//   0 = C[0] + C[1]*X + C[2]*Y + C[3]*Z + C[4]*X^2 + C[5]*Y^2
//       + C[6]*Z^2 + C[7]*X*Y + C[8]*X*Z + C[9]*Y*Z
// find distance from point to the surface.

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
WML_ITEM Real SqrDistance (const Vector3<Real>& rkPoint, const Real afQuad[10],
    Vector3<Real>& rkClosest);

template <class Real>
WML_ITEM Real Distance (const Vector3<Real>& rkPoint, const Real afQuad[10],
    Vector3<Real>& rkClosest);

}

#endif
