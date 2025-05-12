// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistRct3Rct3.h"
#include "WmlIntrLoz3Loz3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Lozenge3<Real>& rkL0,
    const Lozenge3<Real>& rkL1, Real fEpsilon)
{
    Real fSqrDist = SqrDistance(rkL0.Rectangle(),rkL1.Rectangle());
    Real fRSum = rkL0.Radius() + rkL1.Radius();
    Real fRSumSqr = fRSum*fRSum;
    return fSqrDist <= fRSumSqr + fEpsilon;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Lozenge3<float>&,
    const Lozenge3<float>&, float);

template WML_ITEM bool TestIntersection<double> (const Lozenge3<double>&,
    const Lozenge3<double>&, double);
}
//----------------------------------------------------------------------------
