// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContCylinder3.h"
#include "WmlApprLineFit3.h"
#include "WmlDistVec3Lin3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Cylinder3<Real> Wml::ContCylinder (int iQuantity,
    const Vector3<Real>* akPoint)
{
    Cylinder3<Real> kCylinder;

    Line3<Real> kLine;
    OrthogonalLineFit(iQuantity,akPoint,kLine.Origin(),kLine.Direction());

    Real fMaxRadiusSqr = (Real)0.0;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        Real fRadiusSqr = SqrDistance(akPoint[i],kLine);
        if ( fRadiusSqr > fMaxRadiusSqr )
            fMaxRadiusSqr = fRadiusSqr;
    }

    Vector3<Real> kDiff = akPoint[0] - kLine.Origin();
    Real fWMin = kLine.Direction().Dot(kDiff), fWMax = fWMin;
    for (i = 1; i < iQuantity; i++)
    {
        kDiff = akPoint[i] - kLine.Origin();
        Real fW = kLine.Direction().Dot(kDiff);
        if ( fW < fWMin )
            fWMin = fW;
        else if ( fW > fWMax )
            fWMax = fW;
    }

    kCylinder.Center() = kLine.Origin() +
        (((Real)0.5)*(fWMax+fWMin))*kLine.Direction();
    kCylinder.Direction() = kLine.Direction();
    kCylinder.Radius() = Math<Real>::Sqrt(fMaxRadiusSqr);
    kCylinder.Height() = fWMax - fWMin;

    return kCylinder;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Cylinder3<float> ContCylinder<float> (int,
    const Vector3<float>*);

template WML_ITEM Cylinder3<double> ContCylinder<double> (int,
    const Vector3<double>*);
}
//----------------------------------------------------------------------------
