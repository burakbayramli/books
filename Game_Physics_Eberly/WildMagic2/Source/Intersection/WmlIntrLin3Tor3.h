// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRLIN3TOR3_H
#define WMLINTRLIN3TOR3_H

#include "WmlRay3.h"
#include "WmlTorus3.h"

namespace Wml
{

// The ray is p*dir+eye, p >= 0.  The intersection is determined by
// p^4+c3*p^3+c2*p^2+c1*p+c0 = 0.  The direction is assumed to be towards
// the object from the observer so that the minimum p solution gives the
// nearest intersection.

template <class Real>
WML_ITEM bool FindIntersection (const Ray3<Real>& rkRay,
     const Torus3<Real>& rkTorus, Real& rfS, Real& rfT);

}

#endif
