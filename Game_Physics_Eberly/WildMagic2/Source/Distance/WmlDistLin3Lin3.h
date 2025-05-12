// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTLIN3LIN3_H
#define WMLDISTLIN3LIN3_H

#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// squared distance measurements

template <class Real>
WML_ITEM Real SqrDistance (const Line3<Real>& rkLine0,
    const Line3<Real>& rkLine1, Real* pfLinP0 = NULL, Real* pfLinP1 = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Line3<Real>& rkLine,
    const Ray3<Real>& rkRay, Real* pfLinP = NULL, Real* pfRayP = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Line3<Real>& rkLine,
    const Segment3<Real>& rkSeg, Real* pfLinP = NULL, Real* pfSegP = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Ray3<Real>& rkRay0,
    const Ray3<Real>& rkRay1, Real* pfRayP0 = NULL, Real* pfRayP1 = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Ray3<Real>& rkRay,
    const Segment3<Real>& rkSeg, Real* pfRayP = NULL, Real* pfSegP = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Segment3<Real>& rkSeg0,
    const Segment3<Real>& rkSeg1, Real* pfSegP0 = NULL, Real* pfSegP1 = NULL);

// distance measurements

template <class Real>
WML_ITEM Real Distance (const Line3<Real>& rkLine0,
    const Line3<Real>& rkLine1, Real* pfLinP0 = NULL, Real* pfLinP1 = NULL);

template <class Real>
WML_ITEM Real Distance (const Line3<Real>& rkLine,
    const Ray3<Real>& rkRay, Real* pfLinP = NULL, Real* pfRayP = NULL);

template <class Real>
WML_ITEM Real Distance (const Line3<Real>& rkLine,
    const Segment3<Real>& rkSeg, Real* pfLinP = NULL, Real* pfSegP = NULL);

template <class Real>
WML_ITEM Real Distance (const Ray3<Real>& rkRay0, const Ray3<Real>& rkRay1,
    Real* pfRayP0 = NULL, Real* pfRayP1 = NULL);

template <class Real>
WML_ITEM Real Distance (const Ray3<Real>& rkRay,
    const Segment3<Real>& rkSeg, Real* pfRayP = NULL, Real* pfSegP = NULL);

template <class Real>
WML_ITEM Real Distance (const Segment3<Real>& rkSeg0,
    const Segment3<Real>& rkSeg1, Real* pfSegP0 = NULL, Real* pfSegP1 = NULL);

}

#endif
