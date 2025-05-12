// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContBox2.h"
#include "WmlApprGaussPointsFit2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::ContAlignedBox (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkMin, Vector2<Real>& rkMax)
{
    rkMin = akPoint[0];
    rkMax = rkMin;

    for (int i = 1; i < iQuantity; i++)
    {
        if ( akPoint[i].X() < rkMin.X() )
            rkMin.X() = akPoint[i].X();
        else if ( akPoint[i].X() > rkMax.X() )
            rkMax.X() = akPoint[i].X();

        if ( akPoint[i].Y() < rkMin.Y() )
            rkMin.Y() = akPoint[i].Y();
        else if ( akPoint[i].Y() > rkMax.Y() )
            rkMax.Y() = akPoint[i].Y();
    }
}
//----------------------------------------------------------------------------
template <class Real>
Box2<Real> Wml::ContOrientedBox (int iQuantity, const Vector2<Real>* akPoint)
{
    Box2<Real> kBox;

    GaussPointsFit(iQuantity,akPoint,kBox.Center(),kBox.Axes(),
        kBox.Extents());

    // Let C be the box center and let U0 and U1 be the box axes.  Each input
    // point is of the form X = C + y0*U0 + y1*U1.  The following code
    // computes min(y0), max(y0), min(y1), and max(y1).  The box center is
    // then adjusted to be
    //   C' = C + 0.5*(min(y0)+max(y0))*U0 + 0.5*(min(y1)+max(y1))*U1

    Vector2<Real> kDiff = akPoint[0] - kBox.Center();
    Real fY0Min = kDiff.Dot(kBox.Axis(0)), fY0Max = fY0Min;
    Real fY1Min = kDiff.Dot(kBox.Axis(1)), fY1Max = fY1Min;

    for (int i = 1; i < iQuantity; i++)
    {
        kDiff = akPoint[i] - kBox.Center();

        Real fY0 = kDiff.Dot(kBox.Axis(0));
        if ( fY0 < fY0Min )
            fY0Min = fY0;
        else if ( fY0 > fY0Max )
            fY0Max = fY0;

        Real fY1 = kDiff.Dot(kBox.Axis(1));
        if ( fY1 < fY1Min )
            fY1Min = fY1;
        else if ( fY1 > fY1Max )
            fY1Max = fY1;
    }

    kBox.Center() += (((Real)0.5)*(fY0Min+fY0Max))*kBox.Axis(0) +
        (((Real)0.5)*(fY1Min+fY1Max))*kBox.Axis(1);

    kBox.Extent(0) = ((Real)0.5)*(fY0Max - fY0Min);
    kBox.Extent(1) = ((Real)0.5)*(fY1Max - fY1Min);

    return kBox;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::ContOrientedBox (int iQuantity, const Vector2<Real>* akPoint,
    const bool* abValid, Box2<Real>& rkBox)
{
    if ( !GaussPointsFit(iQuantity,akPoint,abValid,rkBox.Center(),
         rkBox.Axes(),rkBox.Extents()) )
    {
        return false;
    }

    // Let C be the box center and let U0 and U1 be the box axes.  Each input
    // point is of the form X = C + y0*U0 + y1*U1.  The following code
    // computes min(y0), max(y0), min(y1), and max(y1).  The box center is
    // then adjusted to be
    //   C' = C + 0.5*(min(y0)+max(y0))*U0 + 0.5*(min(y1)+max(y1))*U1

    // get first valid vertex
    Vector2<Real> kDiff;
    Real fY0Min, fY0Max, fY1Min, fY1Max;
    int i;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kDiff = akPoint[i] - rkBox.Center();
            fY0Min = kDiff.Dot(rkBox.Axis(0));
            fY0Max = fY0Min;
            fY1Min = kDiff.Dot(rkBox.Axis(1));
            fY1Max = fY1Min;
            break;
        }
    }

    for (i++; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            kDiff = akPoint[i] - rkBox.Center();

            Real fY0 = kDiff.Dot(rkBox.Axis(0));
            if ( fY0 < fY0Min )
                fY0Min = fY0;
            else if ( fY0 > fY0Max )
                fY0Max = fY0;

            Real fY1 = kDiff.Dot(rkBox.Axis(1));
            if ( fY1 < fY1Min )
                fY1Min = fY1;
            else if ( fY1 > fY1Max )
                fY1Max = fY1;
        }
    }

    rkBox.Center() += (((Real)0.5)*(fY0Min+fY0Max))*rkBox.Axis(0)
        + (((Real)0.5)*(fY1Min+fY1Max))*rkBox.Axis(1);

    rkBox.Extent(0) = ((Real)0.5)*(fY0Max - fY0Min);
    rkBox.Extent(1) = ((Real)0.5)*(fY1Max - fY1Min);

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void ContAlignedBox<float> (int,
    const Vector2<float>*, Vector2<float>&, Vector2<float>&);
template WML_ITEM Box2<float> ContOrientedBox<float> (int,
    const Vector2<float>*);
template WML_ITEM bool ContOrientedBox<float> (int,
    const Vector2<float>*, const bool* abValid, Box2<float>&);

template WML_ITEM void ContAlignedBox<double> (int,
    const Vector2<double>*, Vector2<double>&, Vector2<double>&);
template WML_ITEM Box2<double> Wml::ContOrientedBox<double> (int,
    const Vector2<double>*);
template WML_ITEM bool ContOrientedBox<double> (int,
    const Vector2<double>*, const bool* abValid, Box2<double>&);
}
//----------------------------------------------------------------------------
