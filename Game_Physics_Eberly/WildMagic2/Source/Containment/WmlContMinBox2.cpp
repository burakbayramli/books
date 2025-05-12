// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContMinBox2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Box2<Real> Wml::MinBox (int iQuantity, const Vector2<Real>* akPoint)
{
    // The input points are V[0] through V[N-1] and are assumed to be the
    // vertices of a convex polygon that are counterclockwise ordered.  The
    // input points must not contain three consecutive collinear points.

    // Unit-length edge directions of convex polygon.  These could be
    // precomputed and passed to this routine if the application requires it.
    int iQuantityM1 = iQuantity -1;
    Vector2<Real>* akEdge = new Vector2<Real>[iQuantity];
    bool* abVisited = new bool[iQuantity];
    int i;
    for (i = 0; i < iQuantityM1; i++)
    {
        akEdge[i] = akPoint[i+1] - akPoint[i];
        akEdge[i].Normalize();
        abVisited[i] = false;
    }
    akEdge[iQuantityM1] = akPoint[0] - akPoint[iQuantityM1];
    akEdge[iQuantityM1].Normalize();
    abVisited[iQuantityM1] = false;

    // Find the smallest axis-aligned box containing the points.  Keep track
    // of the extremum indices, L (left), R (right), B (bottom), and T (top)
    // so that the following constraints are met:
    //   V[L].X() <= V[i].X() for all i and V[(L+1)%N].X() > V[L].X()
    //   V[R].X() >= V[i].X() for all i and V[(R+1)%N].X() < V[R].X()
    //   V[B].Y() <= V[i].Y() for all i and V[(B+1)%N].Y() > V[B].Y()
    //   V[T].Y() >= V[i].Y() for all i and V[(T+1)%N].Y() < V[R].Y()
    Real fXMin = akPoint[0].X(), fXMax = fXMin;
    Real fYMin = akPoint[0].Y(), fYMax = fYMin;
    int iLIndex = 0, iRIndex = 0, iBIndex = 0, iTIndex = 0;
    for (i = 1; i < iQuantity; i++)
    {
        if ( akPoint[i].X() <= fXMin )
        {
            fXMin = akPoint[i].X();
            iLIndex = i;
        }
        else if ( akPoint[i].X() >= fXMax )
        {
            fXMax = akPoint[i].X();
            iRIndex = i;
        }

        if ( akPoint[i].Y() <= fYMin )
        {
            fYMin = akPoint[i].Y();
            iBIndex = i;
        }
        else if ( akPoint[i].Y() >= fYMax )
        {
            fYMax = akPoint[i].Y();
            iTIndex = i;
        }
    }

    // wrap-around tests to ensure the constraints mentioned above
    if ( akPoint[0].X() <= fXMin )
    {
        fXMin = akPoint[0].X();
        iLIndex = 0;
    }
    else if ( akPoint[0].X() >= fXMax )
    {
        fXMax = akPoint[0].X();
        iRIndex = 0;
    }

    if ( akPoint[0].Y() <= fYMin )
    {
        fYMin = akPoint[0].Y();
        iBIndex = 0;
    }
    else if ( akPoint[0].Y() >= fYMax )
    {
        fYMax = akPoint[0].Y();
        iTIndex = 0;
    }

    // dimensions of axis-aligned box (extents store width and height for now)
    Box2<Real> kBox;
    kBox.Center().X() = ((Real)0.5)*(fXMin + fXMax);
    kBox.Center().Y() = ((Real)0.5)*(fYMin + fYMax);
    kBox.Axis(0) = Vector2<Real>::UNIT_X;
    kBox.Axis(1) = Vector2<Real>::UNIT_Y;
    kBox.Extent(0) = ((Real)0.5)*(fXMax - fXMin);
    kBox.Extent(1) = ((Real)0.5)*(fYMax - fYMin);
    Real fMinAreaDiv4 = kBox.Extent(0)*kBox.Extent(1);

    // rotating calipers algorithm
    enum { F_NONE, F_LEFT, F_RIGHT, F_BOTTOM, F_TOP };
    Vector2<Real> kU = Vector2<Real>::UNIT_X, kV = Vector2<Real>::UNIT_Y;

    bool bDone = false;
    while ( !bDone )
    {
        // determine edge that forms smallest angle with current box edges
        int iFlag = F_NONE;
        Real fMaxDot = (Real)0.0;

        Real fDot = kU.Dot(akEdge[iBIndex]);
        if ( fDot > fMaxDot )
        {
            fMaxDot = fDot;
            iFlag = F_BOTTOM;
        }

        fDot = kV.Dot(akEdge[iRIndex]);
        if ( fDot > fMaxDot )
        {
            fMaxDot = fDot;
            iFlag = F_RIGHT;
        }

        fDot = -kU.Dot(akEdge[iTIndex]);
        if ( fDot > fMaxDot )
        {
            fMaxDot = fDot;
            iFlag = F_TOP;
        }

        fDot = -kV.Dot(akEdge[iLIndex]);
        if ( fDot > fMaxDot )
        {
            fMaxDot = fDot;
            iFlag = F_LEFT;
        }

        switch ( iFlag )
        {
        case F_BOTTOM:
            if ( abVisited[iBIndex] )
            {
                bDone = true;
            }
            else
            {
                // compute box axes with E[B] as an edge
                kU = akEdge[iBIndex];
                kV = -kU.Perp();

                // mark edge visited and rotate the calipers
                abVisited[iBIndex] = true;
                if ( ++iBIndex == iQuantity )
                    iBIndex = 0;
            }
            break;
        case F_RIGHT:
            if ( abVisited[iRIndex] )
            {
                bDone = true;
            }
            else
            {
                // compute dimensions of box with E[R] as an edge
                kV = akEdge[iRIndex];
                kU = kV.Perp();

                // mark edge visited and rotate the calipers
                abVisited[iRIndex] = true;
                if ( ++iRIndex == iQuantity )
                    iRIndex = 0;
            }
            break;
        case F_TOP:
            if ( abVisited[iTIndex] )
            {
                bDone = true;
            }
            else
            {
                // compute dimensions of box with E[T] as an edge
                kU = -akEdge[iTIndex];
                kV = -kU.Perp();

                // mark edge visited and rotate the calipers
                abVisited[iTIndex] = true;
                if ( ++iTIndex == iQuantity )
                    iTIndex = 0;
            }
            break;
        case F_LEFT:
            if ( abVisited[iLIndex] )
            {
                bDone = true;
            }
            else
            {
                // compute dimensions of box with E[L] as an edge
                kV = -akEdge[iLIndex];
                kU = kV.Perp();

                // mark edge visited and rotate the calipers
                abVisited[iLIndex] = true;
                if ( ++iLIndex == iQuantity )
                    iLIndex = 0;
            }
            break;
        case F_NONE:
            // polygon is a rectangle
            bDone = true;
            break;
        }

        Real fExtent0 = ((Real)0.5)*(kU.Dot(akPoint[iRIndex] -
            akPoint[iLIndex]));
        Real fExtent1 = ((Real)0.5)*(kV.Dot(akPoint[iTIndex] -
            akPoint[iBIndex]));
        Real fAreaDiv4 = fExtent0*fExtent1;
        if ( fAreaDiv4 < fMinAreaDiv4 )
        {
            fMinAreaDiv4 = fAreaDiv4;
            kBox.Axis(0) = kU;
            kBox.Axis(1) = kV;
            kBox.Extent(0) = fExtent0;
            kBox.Extent(1) = fExtent1;

            // compute box center
            Vector2<Real> kTmp = ((Real)0.5)*(akPoint[iTIndex] +
                akPoint[iBIndex]) - akPoint[iLIndex];
            kBox.Center() = akPoint[iLIndex] + fExtent0*kBox.Axis(0) +
                (kBox.Axis(1).Dot(kTmp))*kBox.Axis(1);
        }
    }

    delete[] abVisited;
    delete[] akEdge;
    return kBox;
}
//----------------------------------------------------------------------------
template <class Real>
Box2<Real> Wml::MinBoxOrderNSqr (int iQuantity, const Vector2<Real>* akPoint)
{
    Real fMinAreaDiv4 = Math<Real>::MAX_REAL;
    Box2<Real> kBox;

    for (int i1 = 0, i0 = iQuantity-1; i1 < iQuantity; i0 = i1, i1++)
    {
        Vector2<Real> kU0 = akPoint[i1] - akPoint[i0];
        kU0.Normalize();
        Vector2<Real> kU1 = -kU0.Perp();
        Real fS0 = (Real)0.0, fT0 = (Real)0.0;
        Real fS1 = (Real)0.0, fT1 = (Real)0.0;
        for (int j = 1; j < iQuantity; j++)
        {
            Vector2<Real> kDiff = akPoint[j] - akPoint[0];
            Real fTest = kU0.Dot(kDiff);
            if ( fTest < fS0 )
                fS0 = fTest;
            else if ( fTest > fS1 )
                fS1 = fTest;

            fTest = kU1.Dot(kDiff);
            if ( fTest < fT0 )
                fT0 = fTest;
            else if ( fTest > fT1 )
                fT1 = fTest;
        }

        Real fExtent0 = ((Real)0.5)*(fS1 - fS0);
        Real fExtent1 = ((Real)0.5)*(fT1 - fT0);
        Real fAreaDiv4 = fExtent0*fExtent1;
        if ( fAreaDiv4 < fMinAreaDiv4 )
        {
            fMinAreaDiv4 = fAreaDiv4;
            kBox.Axis(0) = kU0;
            kBox.Axis(1) = kU1;
            kBox.Extent(0) = fExtent0;
            kBox.Extent(1) = fExtent1;
            kBox.Center() = akPoint[0] + ((Real)0.5)*(fS0+fS1)*kU0 +
                ((Real)0.5)*(fT0+fT1)*kU1;
        }
    }

    return kBox;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Box2<float> MinBox<float> (int,
    const Vector2<float>*);
template WML_ITEM Box2<float> MinBoxOrderNSqr<float> (int,
    const Vector2<float>*);

template WML_ITEM Box2<double> MinBox<double> (int,
    const Vector2<double>*);
template WML_ITEM Box2<double> MinBoxOrderNSqr<double> (int,
    const Vector2<double>*);
}
//----------------------------------------------------------------------------
