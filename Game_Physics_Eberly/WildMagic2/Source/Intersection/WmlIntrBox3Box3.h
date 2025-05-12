// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLINTR3DBOXBOX_H
#define WMLINTR3DBOXBOX_H

#include "WmlBox3.h"

namespace Wml
{

// boxes are stationary
template <class Real>
WML_ITEM bool TestIntersection (const Box3<Real>& rkBox0,
    const Box3<Real>& rkBox1);

// boxes have constant linear velocity
template <class Real>
WML_ITEM bool TestIntersection (Real fTime, const Box3<Real>& rkBox0,
    const Vector3<Real>& rkVel0, const Box3<Real>& rkBox1,
    const Vector3<Real>& rkVel1);

// boxes have constant linear velocities and angular velocities
template <class Real>
WML_ITEM bool TestIntersection (Real fTime, int iNumSteps,
    const Box3<Real>& rkBox0,  const Vector3<Real>& rkVel0,
    const Vector3<Real>& rkRotCen0, const Vector3<Real>& rkRotAxis0,
    const Box3<Real>& rkBox1, const Vector3<Real>& rkVel1,
    const Vector3<Real>& rkRotCen1, const Vector3<Real>& rkRotAxis1);

// boxes are have constant linear velocities
template <class Real>
WML_ITEM bool FindIntersection (const Box3<Real>& rkBox0,
    const Vector3<Real>& rkVel0, const Box3<Real>& rkBox1,
    const Vector3<Real>& rkVel1, Real& rfTFirst, Real fTMax, int& riQuantity,
    Vector3<Real>* akP);

}

#endif
