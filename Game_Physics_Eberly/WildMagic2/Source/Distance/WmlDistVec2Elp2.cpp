// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec2Elp2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const EllipseStandard2<Real>& rkEllipse,
    const Vector2<Real>& rkPoint, Vector2<Real>& rkClosest)
{
    const Real* afExtent = rkEllipse.Extents();

    Real fA2 = afExtent[0]*afExtent[0];
    Real fB2 = afExtent[1]*afExtent[1];
    Real fU2 = rkPoint.X()*rkPoint.X();
    Real fV2 = rkPoint.Y()*rkPoint.Y();
    Real fA2U2 = fA2*fU2, fB2V2 = fB2*fV2;
    Real fDx, fDy, fXDivA, fYDivB;

    // handle points near the coordinate axes
    const Real fThreshold = (Real)1e-12;
    if ( Math<Real>::FAbs(rkPoint.X()) <= fThreshold )  // rkPoint.X() == 0
    {
        if ( afExtent[0] >= afExtent[1] 
        ||   Math<Real>::FAbs(rkPoint.Y()) >= afExtent[1]-fA2/afExtent[1] )
        {
            rkClosest.X() = (Real)0.0;
            rkClosest.Y() = ( rkPoint.Y() >= (Real)0.0 ? afExtent[1] :
                -afExtent[1] );
            fDy = rkClosest.Y() - rkPoint.Y();
            return Math<Real>::FAbs(fDy);
        }
        else
        {
            rkClosest.Y() = fB2*rkPoint.Y()/(fB2-fA2);
            fDy = rkClosest.Y() - rkPoint.Y();
            fYDivB = rkClosest.Y()/afExtent[1];
            rkClosest.X() = afExtent[0]*Math<Real>::Sqrt(
                Math<Real>::FAbs(1.0f-fYDivB*fYDivB));
            return Math<Real>::Sqrt(rkClosest.X()*rkClosest.X()+fDy*fDy);
        }
    }

    if ( Math<Real>::FAbs(rkPoint.Y()) <= fThreshold )  // rkPoint.Y() == 0
    {
        if ( afExtent[1] >= afExtent[0]
        ||   Math<Real>::FAbs(rkPoint.X()) >= afExtent[0]-fB2/afExtent[0] )
        {
            rkClosest.X() = ( rkPoint.X() >= (Real)0.0 ? afExtent[0] :
                -afExtent[0] );
            rkClosest.Y() = (Real)0.0;
            fDx = rkClosest.X() - rkPoint.X();
            return Math<Real>::FAbs(fDx);
        }
        else
        {
            rkClosest.X() = fA2*rkPoint.X()/(fA2-fB2);
            fDx = rkClosest.X() - rkPoint.X();
            fXDivA = rkClosest.X()/afExtent[0];
            rkClosest.Y() = afExtent[1]*Math<Real>::Sqrt(
                Math<Real>::FAbs(1.0f-fXDivA*fXDivA));
            return Math<Real>::Sqrt(fDx*fDx+rkClosest.Y()*rkClosest.Y());
        }
    }

    // initial guess
    Real fURatio = rkPoint.X()/afExtent[0];
    Real fVRatio = rkPoint.Y()/afExtent[1];
    Real fT;
    if ( fURatio*fURatio + fVRatio*fVRatio < (Real)1.0 )
    {
        fT = (Real)0.0;
    }
    else
    {
        Real fMax = afExtent[0];
        if ( afExtent[1] > fMax )
            fMax = afExtent[1];

        fT = fMax*rkPoint.Length();
    }

    // Newton's method
    const int iMaxIteration = 64;
    Real fP, fQ;
    for (int i = 0; i < iMaxIteration; i++)
    {
        fP = fT+fA2;
        fQ = fT+fB2;
        Real fP2 = fP*fP;
        Real fQ2 = fQ*fQ;
        Real fR = fP2*fQ2-fA2U2*fQ2-fB2V2*fP2;
        if ( Math<Real>::FAbs(fR) < Math<Real>::EPSILON )
            break;

        Real fDR = ((Real)2.0)*(fP*fQ*(fP+fQ)-fA2U2*fQ-fB2V2*fP);
        fT -= fR/fDR;
    }

    rkClosest.X() = fA2*rkPoint.X()/fP;
    rkClosest.Y() = fB2*rkPoint.Y()/fQ;
    Vector2<Real> kDiff = rkClosest - rkPoint;
    return kDiff.Length();
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const EllipseStandard2<Real>& rkEllipse,
    const Vector2<Real>& rkPoint, Vector2<Real>& rkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkEllipse,rkPoint,rkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float>
    (const EllipseStandard2<float>&, const Vector2<float>&,
    Vector2<float>&);
template WML_ITEM float Distance<float>
    (const EllipseStandard2<float>&, const Vector2<float>&,
    Vector2<float>&);

template WML_ITEM double SqrDistance<double>
    (const EllipseStandard2<double>&, const Vector2<double>&,
    Vector2<double>&);
template WML_ITEM double Distance<double>
    (const EllipseStandard2<double>&, const Vector2<double>&,
    Vector2<double>&);
}
//----------------------------------------------------------------------------
