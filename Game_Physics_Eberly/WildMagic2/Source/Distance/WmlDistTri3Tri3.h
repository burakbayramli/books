// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTTRI3TRI3_H
#define WMLDISTTRI3TRI3_H

#include "WmlTriangle3.h"

namespace Wml
{

// squared distance measurements

template <class Real>
WML_ITEM Real SqrDistance (const Triangle3<Real>& rkTri0,
    const Triangle3<Real>& rkTri1, Real* pfTri0P0 = NULL,
    Real* pfTri0P1 = NULL, Real* pfTri1P0 = NULL, Real* pfTri1P1 = NULL);

// distance measurements

template <class Real>
WML_ITEM Real Distance (const Triangle3<Real>& rkTri0,
    const Triangle3<Real>& rkTri1, Real* pfTri0P0 = NULL,
    Real* pfTri0P1 = NULL, Real* pfTri1P0 = NULL, Real* pfTri1P1 = NULL);

}

// Sample usage for SqrDistance or Distance.
//
// Triangle3f tri0, tri1;
// tri0.Origin() = Vector3f(*,*,*);
// tri0.Edge0()  = Vector3f(*,*,*);
// tri0.Edge1()  = Vector3f(*,*,*);
// tri1.Origin() = Vector3f(*,*,*);
// tri1.Edge0()  = Vector3f(*,*,*);
// tri1.Edge1()  = Vector3f(*,*,*);
// 
// If you only want the squared distance:
// float sqrDist = SqrDistance<float>(tri0,tri1);
//
// If you want the two closest points, one from each triangle:
// float p0, p1, q0, q1;
// float sqrDist = SqrDistance<float>(tri0,tri1,&p0,&p1,&q0,&q1);
// Vector3f closest0 = tri0.Origin()+p0*tri0.Edge0()+p1*tri0.Edge1();
// Vector3f closest1 = tri1.Origin()+q0*tri1.Edge0()+q1*tri1.Edge1();

#endif
