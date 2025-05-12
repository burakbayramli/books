// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDISTLIN3BOX3_H
#define WMLDISTLIN3BOX3_H

#include "WmlBox3.h"
#include "WmlLine3.h"
#include "WmlRay3.h"
#include "WmlSegment3.h"

namespace Wml
{

// squared distance measurements

template <class Real>
WML_ITEM Real SqrDistance (const Line3<Real>& rkLine,
    const Box3<Real>& rkBox, Real* pfLParam = NULL, Real* pfBParam0 = NULL,
    Real* pfBParam1 = NULL, Real* pfBParam2 = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Ray3<Real>& rkRay,
    const Box3<Real>& rkBox, Real* pfLParam = NULL, Real* pfBParam0 = NULL,
    Real* pfBParam1 = NULL, Real* pfBParam2 = NULL);

template <class Real>
WML_ITEM Real SqrDistance (const Segment3<Real>& rkSeg,
    const Box3<Real>& rkBox, Real* pfLParam = NULL, Real* pfBParam0 = NULL,
    Real* pfBParam1 = NULL, Real* pfBParam2 = NULL);


// distance measurements

template <class Real>
WML_ITEM Real Distance (const Line3<Real>& rkLine, const Box3<Real>& rkBox,
    Real* pfLParam = NULL, Real* pfBParam0 = NULL, Real* pfBParam1 = NULL,
    Real* pfBParam2 = NULL);

template <class Real>
WML_ITEM Real Distance (const Ray3<Real>& rkRay, const Box3<Real>& rkBox,
    Real* pfLParam = NULL, Real* pfBParam0 = NULL, Real* pfBParam1 = NULL,
    Real* pfBParam2 = NULL);

template <class Real>
WML_ITEM Real Distance (const Segment3<Real>& rkSeg, const Box3<Real>& rkBox,
    Real* pfLParam = NULL, Real* pfBParam0 = NULL, Real* pfBParam1 = NULL,
    Real* pfBParam2 = NULL);

}

#endif
