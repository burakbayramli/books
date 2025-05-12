// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPRSPHEREFIT3_H
#define WMLAPPRSPHEREFIT3_H

// Least-squares fit of a sphere to a set of points.  Successful fit is
// indicated by return value of 'true'.  If return value is false, number of
// iterations was exceeded.  Try increasing the maximum number of iterations.

#include "WmlVector3.h"

namespace Wml
{

template <class Real>
WML_ITEM bool SphereFit (int iQuantity, const Vector3<Real>* akPoint,
    int iMaxIterations, Vector3<Real>& rkCenter, Real& rfRadius);

}

#endif
