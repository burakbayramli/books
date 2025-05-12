// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONTSPHERE3_H
#define WMLCONTSPHERE3_H

#include "WmlSphere3.h"

namespace Wml
{

template <class Real>
WML_ITEM Sphere3<Real> ContSphereOfAABB (int iQuantity,
    const Vector3<Real>* akPoint);

template <class Real>
WML_ITEM Sphere3<Real> ContSphereAverage (int iQuantity,
    const Vector3<Real>* akPoint);

// These two functions allow for selection of vertices from a pool.  The
// return value is 'true' if and only if at least one vertex is valid.

template <class Real>
WML_ITEM bool ContSphereOfAABB (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Sphere3<Real>& rkSphere);

template <class Real>
WML_ITEM bool ContSphereAverage (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Sphere3<Real>& rkSphere);

// Test for containment, point x is in sphere if
//    Sqr(Distance(x,sphere.center)) <= Sqr(sphere.radius)
// If an epsilon e is supplied, the test becomes
//    Sqr(Distance(x,sphere.center)) <= Sqr(sphere.radius) + e

template <class Real>
WML_ITEM bool InSphere (const Vector3<Real>& rkPoint,
    const Sphere3<Real>& rkSphere, Real fEpsilon = (Real)0.0);

template <class Real>
WML_ITEM Sphere3<Real> MergeSpheres (const Sphere3<Real>& rkSphere0,
    const Sphere3<Real>& rkSphere1);

}

#endif
