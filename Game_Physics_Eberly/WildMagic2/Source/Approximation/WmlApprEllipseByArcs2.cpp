// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprEllipseByArcs2.h"
#include "WmlContScribeCir2.h"

//----------------------------------------------------------------------------
template <class Real>
void Wml::ApproximateEllipseByArcs (Real fA, Real fB, int iNumArcs,
    Vector2<Real>*& rakPoint, Vector2<Real>*& rakCenter, Real*& rafRadius)
{
    // allocate arrays
    assert( iNumArcs >= 2 );
    if ( iNumArcs < 2 )
    {
        rakPoint = NULL;
        rakCenter = NULL;
        rafRadius = NULL;
        return;
    }

    rakPoint = new Vector2<Real>[iNumArcs+1];
    rakCenter = new Vector2<Real>[iNumArcs];
    rafRadius = new Real[iNumArcs];

    // intermediate ellipse quantities
    Real fA2 = fA*fA, fB2 = fB*fB, fAB = fA*fB;
    Real fInvB2mA2 = ((Real)1.0)/(fB2-fA2);

    // End points of ellipse in first quadrant.  Points are generated in
    // counterclockwise order.
    rakPoint[0] = Vector2<Real>(fA,(Real)0.0);
    rakPoint[iNumArcs] = Vector2<Real>((Real)0.0,fB);

    // curvature at end points, store curvature for computing arcs
    Real fK0 = fA/fB2;
    Real fK1 = fB/fA2;

    // select ellipse points based on curvature properties
    Real fInvNumArcs = ((Real)1.0)/iNumArcs;
    int i;
    for (i = 1; i < iNumArcs; i++)
    {
        // curvature at new point is weighted average of curvature at ends
        Real fW1 = i*fInvNumArcs, fW0 = (Real)1.0 - fW1;
        Real fK = fW0*fK0 + fW1*fK1;

        // compute point having this curvature
        Real fTmp = Math<Real>::Pow(fAB/fK,(Real)(2.0/3.0));
        rakPoint[i].X() = fA*Math<Real>::Sqrt(
            Math<Real>::FAbs((fTmp-fA2)*fInvB2mA2));
        rakPoint[i].Y() = fB*Math<Real>::Sqrt(
            Math<Real>::FAbs((fTmp-fB2)*fInvB2mA2));
    }

    // compute arc at (a,0)
    Circle2<Real> kCircle;
    Circumscribe<Real>(Vector2<Real>(rakPoint[1].X(),-rakPoint[1].Y()),
        rakPoint[0],rakPoint[1],kCircle);
    rakCenter[0] = kCircle.Center();
    rafRadius[0] = kCircle.Radius();

    // compute arc at (0,b)
    int iLast = iNumArcs-1;
    Circumscribe(Vector2<Real>(-rakPoint[iLast].X(),rakPoint[iLast].Y()),
        rakPoint[iNumArcs],rakPoint[iLast],kCircle);
    rakCenter[iLast] = kCircle.Center();
    rafRadius[iLast] = kCircle.Radius();

    // compute arcs at intermediate points between (a,0) and (0,b)
    int iM, iP;
    for (iM = 0, i = 1, iP = 2; i < iLast; iM++, i++, iP++)
    {
        Circumscribe<Real>(rakPoint[iM],rakPoint[i],rakPoint[iP],kCircle);
        rakCenter[i] = kCircle.Center();
        rafRadius[i] = kCircle.Radius();
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void ApproximateEllipseByArcs<float> (float,
    float, int, Vector2<float>*&, Vector2<float>*&, float*&);
template WML_ITEM void ApproximateEllipseByArcs<double> (double,
    double, int, Vector2<double>*&, Vector2<double>*&, double*&);
}
//----------------------------------------------------------------------------
