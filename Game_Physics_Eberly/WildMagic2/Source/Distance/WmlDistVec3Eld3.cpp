// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDistVec3Eld3.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::SqrDistance (const EllipsoidStandard3<Real>& rkEllipsoid,
    const Vector3<Real>& rkPoint, Vector3<Real>& rkClosest)
{
    const Real* afExtent = rkEllipsoid.Extents();

    Real fA2 = afExtent[0]*afExtent[0];
    Real fB2 = afExtent[1]*afExtent[1];
    Real fC2 = afExtent[2]*afExtent[2];
    Real fU2 = rkPoint.X()*rkPoint.X();
    Real fV2 = rkPoint.Y()*rkPoint.Y();
    Real fW2 = rkPoint.Z()*rkPoint.Z();
    Real fA2U2 = fA2*fU2, fB2V2 = fB2*fV2, fC2W2 = fC2*fW2;

    // initial guess
    Real fURatio = rkPoint.X()/afExtent[0];
    Real fVRatio = rkPoint.Y()/afExtent[1];
    Real fWRatio = rkPoint.Z()/afExtent[2];
    Real fT;
    if ( fURatio*fURatio+fVRatio*fVRatio+fWRatio*fWRatio < (Real)1.0 )
    {
        fT = (Real)0.0;
    }
    else
    {
        Real fMax = afExtent[0];
        if ( afExtent[1] > fMax )
            fMax = afExtent[1];
        if ( afExtent[2] > fMax )
            fMax = afExtent[2];

        fT = fMax*rkPoint.Length();
    }

    // Newton's method
    const int iMaxIteration = 64;
    Real fP, fQ, fR;
    for (int i = 0; i < iMaxIteration; i++)
    {
        fP = fT+fA2;
        fQ = fT+fB2;
        fR = fT+fC2;
        Real fP2 = fP*fP;
        Real fQ2 = fQ*fQ;
        Real fR2 = fR*fR;
        Real fS = fP2*fQ2*fR2-fA2U2*fQ2*fR2-fB2V2*fP2*fR2-fC2W2*fP2*fQ2;
        if ( Math<Real>::FAbs(fS) < Math<Real>::EPSILON )
            break;

        Real fPQ = fP*fQ, fPR = fP*fR, fQR = fQ*fR, fPQR = fP*fQ*fR;
        Real fDS = ((Real)2.0)*(fPQR*(fQR+fPR+fPQ)-fA2U2*fQR*(fQ+fR)-
            fB2V2*fPR*(fP+fR)-fC2W2*fPQ*(fP+fQ));
        fT -= fS/fDS;
    }

    rkClosest.X() = fA2*rkPoint.X()/fP;
    rkClosest.Y() = fB2*rkPoint.Y()/fQ;
    rkClosest.Z() = fC2*rkPoint.Z()/fR;
    Vector3<Real> kDiff = rkClosest - rkPoint;
    return kDiff.SquaredLength();
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::Distance (const EllipsoidStandard3<Real>& rkEllipsoid,
    const Vector3<Real>& rkPoint, Vector3<Real>& rkClosest)
{
    return Math<Real>::Sqrt(SqrDistance(rkEllipsoid,rkPoint,rkClosest));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float SqrDistance<float>
    (const EllipsoidStandard3<float>&, const Vector3<float>&,
    Vector3<float>&);
template WML_ITEM float Distance<float>
    (const EllipsoidStandard3<float>&, const Vector3<float>&,
    Vector3<float>&);

template WML_ITEM double SqrDistance<double>
    (const EllipsoidStandard3<double>&, const Vector3<double>&,
    Vector3<double>&);
template WML_ITEM double Distance<double>
    (const EllipsoidStandard3<double>&, const Vector3<double>&,
    Vector3<double>&);
}
//----------------------------------------------------------------------------
