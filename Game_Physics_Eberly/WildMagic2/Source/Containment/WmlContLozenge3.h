// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCONT3DLOZENGE_H
#define WMLCONT3DLOZENGE_H

#include "WmlLozenge3.h"

namespace Wml
{

// Compute plane of lozenge rectangle using least-squares fit.  Parallel
// planes are chosen close enough together so that all the data points lie
// between them.  The radius is half the distance between the two planes.
// The half-cylinder and quarter-cylinder side pieces are chosen using a
// method similar to that used for fitting by capsules.
template <class Real>
WML_ITEM Lozenge3<Real> ContLozenge (int iQuantity,
    const Vector3<Real>* akPoint);

// This function allows for selection of vertices from a pool.  The
// return value is 'true' if and only if at least one vertex is valid.
template <class Real>
WML_ITEM bool ContLozenge (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Lozenge3<Real>& rkLozenge);

// Test for containment, point x is in lozenge if
//    Sqr(Distance(x,lozenge.rectangle)) <= Sqr(lozenge.radius)
// If an epsilon e is supplied, the test becomes
//    Sqr(Distance(x,lozenge.rectangle)) <= Sqr(lozenge.radius) + e
template <class Real>
WML_ITEM bool InLozenge (const Vector3<Real>& rkPoint,
    const Lozenge3<Real>& rkLozenge, Real fEpsilon = (Real)0.0);

}

#endif
