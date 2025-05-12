// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRUTILITYLIN3_H
#define WMLINTRUTILITYLIN3_H

#include "WmlIntrUtility3.h"

namespace Wml
{

template <class Real>
WML_ITEM void LineProjection (const Vector3<Real>& rkD,
    const Vector3<Real> akV[2], Real& rfMin, Real& rfMax);

template <class Real>
WML_ITEM void GetLineConfiguration (const Vector3<Real>& rkAxis, 
    const Vector3<Real> akU[2], ContactConfig<Real>& rkConfig);

// This is the case for a static/dynamic contact set (currently touching and
// colinear).
template <class Real>
WML_ITEM void FindContactSetColinearLines (const Vector3<Real> akU[2],
    const Vector3<Real> akV[2], int& riQuantity, Vector3<Real>* akP);

// This is the case for a static/dynamic contact set (currently touching and
// non-colinear).
//   kNormU is the normal of U in the plane of UxV ((UxV)xU)
//   U here is any point on the plane whose normal is NormU
//   V here is a line
//   returns the point where the line crosses that plane
template <class Real>
WML_ITEM void FindContactSetLineThroughPlane (const Vector3<Real> akV[2], 
    const Vector3<Real>& rkU, const Vector3<Real>& rkNormU,
    int& riQuantity, Vector3<Real>* akP);

// for two statically intersecting lines
template <class Real>
WML_ITEM void FindContactSetLinLin (const Vector3<Real> akU[2], 
    const Vector3<Real> akV[2], int& riQuantity, Vector3<Real>* akP);

}

#endif
