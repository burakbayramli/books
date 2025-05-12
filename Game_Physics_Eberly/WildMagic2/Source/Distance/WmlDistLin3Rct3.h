// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTLIN3RCT3_H
#define WMLDISTLIN3RCT3_H

#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlRectangle3.h"
#include "WmlSegment3.h"

namespace Wml
{

// squared distance measurements

template <class Real>
WML_ITEM Real SqrDistance (const Line3<Real>& rkLine,
    const Rectangle3<Real>& rkRct, Real* pfLinP = NULL, Real* pfRctP0 = NULL,
    Real* pfRctP1 = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Ray3<Real>& rkRay,
    const Rectangle3<Real>& rkRct, Real* pfRayP = NULL, Real* pfRctP0 = NULL,
    Real* pfRctP1 = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Segment3<Real>& rkSeg,
    const Rectangle3<Real>& rkRct, Real* pfSegP = NULL, Real* pfRctP0 = NULL,
    Real* pfRctP1 = NULL);

// distance measurements

template <class Real>
WML_ITEM Real Distance (const Line3<Real>& rkLine,
    const Rectangle3<Real>& rkRct, Real* pfLinP = NULL, Real* pfRctP0 = NULL,
    Real* pfRctP1 = NULL);

template <class Real>
WML_ITEM Real Distance (const Ray3<Real>& rkRay,
    const Rectangle3<Real>& rkRct, Real* pfRayP = NULL, Real* pfRctP0 = NULL,
    Real* pfRctP1 = NULL);

template <class Real>
WML_ITEM Real Distance (const Segment3<Real>& rkSeg,
    const Rectangle3<Real>& rkRct, Real* pfSegP = NULL, Real* pfRctP0 = NULL,
    Real* pfRctP1 = NULL);

}

#endif
