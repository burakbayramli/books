// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONT3DCAPSULE_H
#define WMLCONT3DCAPSULE_H

#include "WmlCapsule3.h"
#include "WmlSphere3.h"

namespace Wml
{

// Compute axis of capsule segment using least-squares fit.  Radius is
// maximum distance from points to axis.  Hemispherical caps are chosen
// as close together as possible.
template <class Real>
WML_ITEM Capsule3<Real> ContCapsule (int iQuantity,
    const Vector3<Real>* akPoint);

// This function allows for selection of vertices from a pool.  The
// return value is 'true' if and only if at least one vertex is valid.
template <class Real>
WML_ITEM bool ContCapsule (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Capsule3<Real>& rkCapsule);

// Test for containment, point x is in capsule if
//    Sqr(Distance(x,capsule.segment)) <= Sqr(capsule.radius)
// If an epsilon e is supplied, the test becomes
//    Sqr(Distance(x,capsule.segment)) <= Sqr(capsule.radius) + e
template <class Real>
WML_ITEM bool InCapsule (const Vector3<Real>& rkPoint,
    const Capsule3<Real>& rkCapsule, Real fEpsilon = (Real)0.0);

template <class Real>
WML_ITEM bool InCapsule (const Sphere3<Real>& rkSphere,
    const Capsule3<Real>& rkCapsule);

template <class Real>
WML_ITEM bool InCapsule (const Capsule3<Real>& rkTestCapsule,
    const Capsule3<Real>& rkCapsule);

template <class Real>
WML_ITEM Capsule3<Real> MergeCapsules (const Capsule3<Real>& rkCapsule0,
    const Capsule3<Real>& rkCapsule1);

}

#endif
