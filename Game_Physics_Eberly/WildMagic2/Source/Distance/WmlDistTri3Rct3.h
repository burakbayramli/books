// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTTRI3RCT3_H
#define WMLDISTTRI3RCT3_H

#include "WmlRectangle3.h"
#include "WmlTriangle3.h"

namespace Wml
{

// squared distance measurements

template <class Real>
WML_ITEM Real SqrDistance (const Triangle3<Real>& rkTri,
    const Rectangle3<Real>& rkRct, Real* pfTriP0 = NULL, Real* pfTriP1 = NULL,
    Real* pfRctP0 = NULL, Real* pfRctP1 = NULL);

// distance measurements

template <class Real>
WML_ITEM Real Distance (const Triangle3<Real>& rkTri,
    const Rectangle3<Real>& rkRct, Real* pfTriP0 = NULL, Real* pfTriP1 = NULL,
    Real* pfRctP0 = NULL, Real* pfRctP1 = NULL);

}

#endif
