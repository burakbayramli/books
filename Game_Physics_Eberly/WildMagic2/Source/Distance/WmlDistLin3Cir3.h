// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTLIN3CIR3_H
#define WMLDISTLIN3CIR3_H

#include "WmlCircle3.h"
#include "WmlLine3.h"

namespace Wml
{

template <class Real>
WML_ITEM Real SqrDistance (const Line3<Real>& rkLine,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkLineClosest = NULL,
    Vector3<Real>* pkCircleClosest = NULL);

template <class Real>
WML_ITEM Real Distance (const Line3<Real>& rkLine,
    const Circle3<Real>& rkCircle, Vector3<Real>* pkLineClosest = NULL,
    Vector3<Real>* pkCircleClosest = NULL);

}

#endif
