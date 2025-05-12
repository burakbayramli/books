// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrLin3Loz3.h"
#include "WmlDistLin3Rct3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Segment3<Real>& rkSegment,
    const Lozenge3<Real>& rkLozenge)
{
    Real fSqrDist = SqrDistance(rkSegment,rkLozenge.Rectangle());
    return fSqrDist <= rkLozenge.Radius()*rkLozenge.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Ray3<Real>& rkRay,
    const Lozenge3<Real>& rkLozenge)
{
    Real fSqrDist = SqrDistance(rkRay,rkLozenge.Rectangle());
    return fSqrDist <= rkLozenge.Radius()*rkLozenge.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Line3<Real>& rkLine,
    const Lozenge3<Real>& rkLozenge)
{
    Real fSqrDist = SqrDistance(rkLine,rkLozenge.Rectangle());
    return fSqrDist <= rkLozenge.Radius()*rkLozenge.Radius();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (
    const Segment3<float>&, const Lozenge3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Ray3<float>&, const Lozenge3<float>&);
template WML_ITEM bool TestIntersection<float> (
    const Line3<float>&, const Lozenge3<float>&);

template WML_ITEM bool TestIntersection<double> (
    const Segment3<double>&, const Lozenge3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Ray3<double>&, const Lozenge3<double>&);
template WML_ITEM bool TestIntersection<double> (
    const Line3<double>&, const Lozenge3<double>&);
}
//----------------------------------------------------------------------------
