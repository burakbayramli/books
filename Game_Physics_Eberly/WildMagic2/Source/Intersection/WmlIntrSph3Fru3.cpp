// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrSph3Fru3.h"
#include "WmlDistVec3Fru3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Sphere3<Real>& rkSphere,
    const Frustum3<Real>& rkFrustum)
{
    Real fSqrDist = SqrDistance(rkSphere.Center(),rkFrustum);
    Real fSqrRadius = rkSphere.Radius()*rkSphere.Radius();
    return fSqrDist <= fSqrRadius;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Sphere3<float>&,
    const Frustum3<float>&);

template WML_ITEM bool TestIntersection<double> (const Sphere3<double>&,
    const Frustum3<double>&);
}
//----------------------------------------------------------------------------
