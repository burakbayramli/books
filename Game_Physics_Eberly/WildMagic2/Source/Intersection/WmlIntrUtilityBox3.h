// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTRUTILITYBOX3_H
#define WMLINTRUTILITYBOX3_H

#include "WmlIntrUtility3.h"
#include "WmlBox3.h"

namespace Wml
{

template <class Real>
WML_ITEM void BoxProjection (const Vector3<Real>& rkAxis,
    const Box3<Real>& rkBox, Real& rfMin, Real& rfMax);

template <class Real>
WML_ITEM void GetBoxConfiguration (const Vector3<Real>& rkAxis, 
    const Box3<Real>& rkBox, ContactConfig<Real>& rkConfig);

template <class Real>
WML_ITEM void FindContactSetCoplanarLineRect (const Vector3<Real> akSeg[2],
    const Vector3<Real> akFace[4], int& riQuantity, Vector3<Real>* akP);

// translates an index into the box back into real coordinates
template <class Real>
WML_ITEM Vector3<Real> GetPoint (int iIndex, const Box3<Real>& rkBox);

}

#endif
