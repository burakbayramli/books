// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContSeparatePoints2.h"
#include "WmlConvexHull2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
static int OnSameSide (const Line2<Real>& rkLine, int iEdgeQuantity,
    const int* aiEdge, const Vector2<Real>* akPoint)
{
    // test if all points on same side of line Dot(N,X) = c
    Real fC0;
    int iPosSide = 0, iNegSide = 0;

    for (int i1 = 0, i0 = iEdgeQuantity-1; i1 < iEdgeQuantity; i0 = i1++)
    {
        fC0 = rkLine.Normal().Dot(akPoint[aiEdge[i0]]);
        if ( fC0 > rkLine.Constant() + Math<Real>::EPSILON )
            iPosSide++;
        else if ( fC0 < rkLine.Constant() - Math<Real>::EPSILON )
            iNegSide++;
        
        if ( iPosSide && iNegSide )
        {
            // line splits point set
            return 0;
        }

        fC0 = rkLine.Normal().Dot(akPoint[aiEdge[i1]]);
        if ( fC0 > rkLine.Constant() + Math<Real>::EPSILON )
            iPosSide++;
        else if ( fC0 < rkLine.Constant() - Math<Real>::EPSILON )
            iNegSide++;
        
        if ( iPosSide && iNegSide )
        {
            // line splits point set
            return 0;
        }
    }

    return iPosSide ? +1 : -1;
}
//----------------------------------------------------------------------------
template <class Real>
static int WhichSide (const Line2<Real>& rkLine, int iEdgeQuantity,
    const int* aiEdge, const Vector2<Real>* akPoint)
{
    // establish which side of line hull is on
    Real fC0;
    for (int i1 = 0, i0 = iEdgeQuantity-1; i1 < iEdgeQuantity; i0 = i1++)
    {
        fC0 = rkLine.Normal().Dot(akPoint[aiEdge[i0]]);
        if ( fC0 > rkLine.Constant() + Math<Real>::EPSILON )
        {
            // hull on positive side
            return +1;
        }
        if ( fC0 < rkLine.Constant() - Math<Real>::EPSILON )
        {
            // hull on negative side
            return -1;
        }

        fC0 = rkLine.Normal().Dot(akPoint[aiEdge[i1]]);
        if ( fC0 > rkLine.Constant() +Math<Real>::EPSILON )
        {
            // hull on positive side
            return +1;
        }
        if ( fC0 < rkLine.Constant() - Math<Real>::EPSILON )
        {
            // hull on negative side
            return -1;
        }
    }

    // hull is effectively collinear
    return 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::SeparatePoints2 (int iQuantity0, const Vector2<Real>* akVertex0,
    int iQuantity1, const Vector2<Real>* akVertex1, Line2<Real>& rkSeprLine)
{
    // construct convex hull of point set 0
    ConvexHull2<Real> kHull0(iQuantity0,akVertex0,false);
    int iEdgeQuantity0 = kHull0.GetQuantity();
    const int* aiEdge0 = kHull0.GetIndices();

    // construct convex hull of point set 1
    ConvexHull2<Real> kHull1(iQuantity1,akVertex1,false);
    int iEdgeQuantity1 = kHull1.GetQuantity();
    const int* aiEdge1 = kHull1.GetIndices();

    // test edges of hull 0 for possible separation of points
    int j0, j1, iI0, iI1, iSide0, iSide1;
    Vector2<Real> kDiff;
    for (j1 = 0, j0 = iEdgeQuantity0-1; j1 < iEdgeQuantity0; j0 = j1++)
    {
        // lookup edge (assert: iI0 != iI1 )
        iI0 = aiEdge0[j0];
        iI1 = aiEdge0[j1];

        // compute potential separating line (assert: (xNor,yNor) != (0,0))
        kDiff = akVertex0[iI1] - akVertex0[iI0];
        rkSeprLine.Normal() = kDiff.Perp();
        rkSeprLine.Constant() = rkSeprLine.Normal().Dot(akVertex0[iI0]);

        // determine if hull 1 is on same side of line
        iSide1 = OnSameSide(rkSeprLine,iEdgeQuantity1,aiEdge1,akVertex1);
        if ( iSide1 )
        {
            // determine which side of line hull 0 lies
            iSide0 = WhichSide(rkSeprLine,iEdgeQuantity0,aiEdge0,akVertex0);

            if ( iSide0*iSide1 <= 0 )  // line separates hulls
                return true;
        }
    }

    // test edges of hull 1 for possible separation of points
    for (j1 = 0, j0 = iEdgeQuantity1-1; j1 < iEdgeQuantity1; j0 = j1++)
    {
        // lookup edge (assert: iI0 != iI1 )
        iI0 = aiEdge1[j0];
        iI1 = aiEdge1[j1];

        // compute perpendicular to edge (assert: (xNor,yNor) != (0,0))
        kDiff = akVertex1[iI1] - akVertex1[iI0];
        rkSeprLine.Normal() = kDiff.Perp();
        rkSeprLine.Constant() = rkSeprLine.Normal().Dot(akVertex1[iI0]);

        // determine if hull 0 is on same side of line
        iSide0 = OnSameSide(rkSeprLine,iEdgeQuantity0,aiEdge0,akVertex0);
        if ( iSide0 )
        {
            // determine which side of line hull 1 lies
            iSide1 = WhichSide(rkSeprLine,iEdgeQuantity1,aiEdge1,akVertex1);

            if ( iSide0*iSide1 <= 0 )  // line separates hulls
                return true;
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
template WML_ITEM bool SeparatePoints2<float> (int,
    const Vector2<float>*, int, const Vector2<float>*, Line2<float>&);

template WML_ITEM bool SeparatePoints2<double> (int,
    const Vector2<double>*, int, const Vector2<double>*, Line2<double>&);
}
//----------------------------------------------------------------------------
