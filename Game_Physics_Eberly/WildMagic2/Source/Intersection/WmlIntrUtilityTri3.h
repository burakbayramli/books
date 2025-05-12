// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRUTILITYTRI3_H
#define WMLINTRUTILITYTRI3_H

#include "WmlIntrUtility3.h"

namespace Wml
{

template <class Real>
WML_ITEM void TriProjection (const Vector3<Real>& rkD,
    const Vector3<Real> akV[3], Real& rfMin, Real& rfMax);

template <class Real>
WML_ITEM void GetTriConfiguration (const Vector3<Real>& rkAxis, 
    const Vector3<Real> akU[3], ContactConfig<Real>& rkConfig);

template <class Real>
WML_ITEM void FindContactSetColinearLineTri (const Vector3<Real> akU[2],
    const Vector3<Real> akV[3], int& riQuantity, Vector3<Real>* akP);

}

#endif
