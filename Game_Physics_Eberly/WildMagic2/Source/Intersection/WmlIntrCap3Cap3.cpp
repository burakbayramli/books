// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistLin3Lin3.h"
#include "WmlIntrCap3Cap3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Capsule3<Real>& rkC0,
    const Capsule3<Real>& rkC1)
{
    Real fSqrDist = SqrDistance<Real>(rkC0.Segment(),rkC1.Segment());
    Real fRSum = rkC0.Radius() + rkC1.Radius();
    Real fRSumSqr = fRSum*fRSum;

    return fSqrDist <= fRSumSqr;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Capsule3<float>&,
    const Capsule3<float>&);

template WML_ITEM bool TestIntersection<double> (const Capsule3<double>&,
    const Capsule3<double>&);
}
//----------------------------------------------------------------------------
