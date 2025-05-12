// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntrPln3Loz3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::TestIntersection (const Plane3<Real>& rkPlane,
    const Lozenge3<Real>& rkLozenge, bool bUnitNormal)
{
    Vector3<Real> kNormal = rkPlane.GetNormal();
    Real fConstant = rkPlane.GetConstant();
    if ( !bUnitNormal )
    {
        Real fLength = kNormal.Normalize();
        fConstant /= fLength;
    }

    Vector3<Real> kC10 = rkLozenge.Origin() + rkLozenge.Edge0();
    Vector3<Real> kC01 = rkLozenge.Origin() + rkLozenge.Edge1();
    Vector3<Real> kC11 = kC10 + rkLozenge.Edge1();

    Real fTmp00 = kNormal.Dot(rkLozenge.Origin()) - fConstant;
    Real fTmp10 = kNormal.Dot(kC10) - fConstant;
    if ( fTmp00*fTmp10 <= (Real)0.0 )
    {
        // two lozenge ends on opposite sides of the plane
        return true;
    }

    Real fTmp01 = kNormal.Dot(kC01) - fConstant;
    if ( fTmp00*fTmp01 <= (Real)0.0 )
    {
        // two lozenge ends on opposite sides of the plane
        return true;
    }

    Real fTmp11 = kNormal.Dot(kC11) - fConstant;
    if ( fTmp10*fTmp11 <= (Real)0.0 )
    {
        // two lozenge ends on opposite sides of the plane
        return true;
    }

    return Math<Real>::FAbs(fTmp00) <= rkLozenge.Radius()
        || Math<Real>::FAbs(fTmp10) <= rkLozenge.Radius()
        || Math<Real>::FAbs(fTmp01) <= rkLozenge.Radius()
        || Math<Real>::FAbs(fTmp11) <= rkLozenge.Radius();
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::Culled (const Plane3<Real>& rkPlane,
    const Lozenge3<Real>& rkLozenge, bool bUnitNormal)
{
    Vector3<Real> kNormal = rkPlane.GetNormal();
    Real fConstant = rkPlane.GetConstant();
    if ( !bUnitNormal )
    {
        Real fLength = kNormal.Normalize();
        fConstant /= fLength;
    }

    Real fTmp00 = kNormal.Dot(rkLozenge.Origin()) - fConstant;
    if ( fTmp00 < (Real)0.0 )
    {
        Real fDotNE0 = kNormal.Dot(rkLozenge.Edge0());
        Real fTmp10 = fTmp00 + fDotNE0;
        if ( fTmp10 < (Real)0.0 )
        {
            Real fDotNE1 = kNormal.Dot(rkLozenge.Edge1());
            Real fTmp01 = fTmp00 + fDotNE1;
            if ( fTmp01 < (Real)0.0 )
            {
                Real fTmp11 = fTmp10 + fDotNE1;
                if ( fTmp11 < (Real)0.0 )
                {
                    // all four lozenge corners on negative side of plane
                    if ( fTmp00 <= fTmp10 )
                    {
                        if ( fTmp00 <= fTmp01 )
                            return fTmp00 <= -rkLozenge.Radius();
                        else
                            return fTmp01 <= -rkLozenge.Radius();
                    }
                    else
                    {
                        if ( fTmp10 <= fTmp11 )
                            return fTmp10 <= -rkLozenge.Radius();
                        else
                            return fTmp11 <= -rkLozenge.Radius();
                    }
                }
            }
        }
    }

    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool TestIntersection<float> (const Plane3<float>&,
    const Lozenge3<float>&, bool);
template WML_ITEM bool Culled<float> (const Plane3<float>&,
    const Lozenge3<float>&, bool);

template WML_ITEM bool TestIntersection<double> (const Plane3<double>&,
    const Lozenge3<double>&, bool);
template WML_ITEM bool Culled<double> (const Plane3<double>&,
    const Lozenge3<double>&, bool);
}
//----------------------------------------------------------------------------
