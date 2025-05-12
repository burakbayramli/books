// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContEllipsoid3.h"
#include "WmlApprGaussPointsFit3.h"
#include "WmlQuaternion.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Ellipsoid3<Real> Wml::ContEllipsoid (int iQuantity,
    const Vector3<Real>* akPoint)
{
    Ellipsoid3<Real> kEllipsoid;

    // Fit the points with a Gaussian distribution.  The covariance matrix
    // is M = D[0]*U[0]*U[0]^T+D[1]*U[1]*U[1]^T+D[2]*U[2]*U[2]^T where D
    // represents afD and U represents akAxis.
    Vector3<Real> akAxis[3];
    Real afD[3];
    GaussPointsFit(iQuantity,akPoint,kEllipsoid.Center(),akAxis,afD);

    // Grow the ellipse, while retaining its shape determined by the
    // covariance matrix, to enclose all the input points.  The quadratic form
    // that is used for the ellipse construction is
    //
    //   Q(X) = (X-C)^T*M*(X-C)
    //        = (X-C)^T*(sum_{j=0}^2 D[j]*U[j]*U[j]^T)*(X-C)
    //        = sum_{j=0}^2 D[j]*Dot(U[i],X-C)^2
    //
    // If the maximum value of Q(X[i]) for all input points is V^2, then a
    // bounding ellipse is Q(X) = V^2 since Q(X[i]) <= V^2 for all i.

    afD[0] = ((Real)1.0)/Math<Real>::FAbs(afD[0]);
    afD[1] = ((Real)1.0)/Math<Real>::FAbs(afD[1]);
    afD[2] = ((Real)1.0)/Math<Real>::FAbs(afD[2]);

    Real fMaxValue = (Real)0.0;
    for (int i = 0; i < iQuantity; i++)
    {
        Vector3<Real> kDiff = akPoint[i] - kEllipsoid.Center();
        Real afDot[3] =
        {
            akAxis[0].Dot(kDiff),
            akAxis[1].Dot(kDiff),
            akAxis[2].Dot(kDiff)
        };

        Real fValue = afD[0]*afDot[0]*afDot[0] + afD[1]*afDot[1]*afDot[1] +
            afD[2]*afDot[2]*afDot[2];

        if ( fValue > fMaxValue )
            fMaxValue = fValue;
    }

    Real fInv = ((Real)1.0)/fMaxValue;
    afD[0] *= fInv;
    afD[1] *= fInv;
    afD[2] *= fInv;

    Matrix3<Real> akTensor0(akAxis[0],akAxis[0]);
    Matrix3<Real> akTensor1(akAxis[1],akAxis[1]);
    Matrix3<Real> akTensor2(akAxis[2],akAxis[2]);
    kEllipsoid.A() = afD[0]*akTensor0+afD[1]*akTensor1+afD[2]*akTensor2;

    return kEllipsoid;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::ProjectEllipsoid (const Vector3<Real>& rkOrigin,
    const Vector3<Real>& rkDir, const Vector3<Real>& rkC,
    const Matrix3<Real>& rkR, const Real afInvD[3], Real& rfMin, Real& rfMax)
{
    // center of projection interval
    Real fCenter = rkDir.Dot(rkC - rkOrigin);

    // radius of projection interval
    Vector3<Real> kPrd = rkDir*rkR;  // R^T*dir
    Vector3<Real> kTmp(afInvD[0]*kPrd.X(),afInvD[1]*kPrd.Y(),
        afInvD[2]*kPrd.Z());
    Real fRadius = Math<Real>::Sqrt(kPrd.Dot(kTmp));

    rfMin = fCenter - fRadius;
    rfMax = fCenter + fRadius;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::MergeEllipsoids (const Vector3<Real>& rkC0,
    const Matrix3<Real>& rkR0, const Real afD0[3], const Vector3<Real>& rkC1,
    const Matrix3<Real>& rkR1, const Real afD1[3], Vector3<Real>& rkC,
    Matrix3<Real>& rkR, Real afD[3])
{
    // compute the average of the input centers
    Vector3<Real> kCAvr = ((Real)0.5)*(rkC0+rkC1);

    // bounding ellipsoid orientation is average of input orientations
    Quaternion<Real> kQ0(rkR0), kQ1(rkR1);
    if ( kQ0.Dot(kQ1) < 0.0f )
        kQ1 = -kQ1;
    Quaternion<Real> kQ = kQ0+kQ1;
    kQ = Math<Real>::InvSqrt(kQ.Dot(kQ))*kQ;
    kQ.ToRotationMatrix(rkR);

    // inverse diagonal matrices
    Real afInvD0[3] =
    {
        ((Real)1.0)/afD0[0],
        ((Real)1.0)/afD0[1],
        ((Real)1.0)/afD0[2]
    };
    Real afInvD1[3] =
    {
        ((Real)1.0)/afD1[0],
        ((Real)1.0)/afD1[1],
        ((Real)1.0)/afD1[2]
    };

    // determine a bounding ellipsoid
    rkC = kCAvr;
    for (int i = 0; i < 3; i++)
    {
        // direction of projection axis
        Vector3<Real> kDir = rkR.GetColumn(i);

        // Project ellipsoids onto axes of new orientation.
        Real fMin0, fMax0, fMin1, fMax1;
        ProjectEllipsoid(kCAvr,kDir,rkC0,rkR0,afInvD0,fMin0,fMax0);
        ProjectEllipsoid(kCAvr,kDir,rkC1,rkR1,afInvD1,fMin1,fMax1);

        // Determine the smallest intervals containing the projected
        // intervals.
        if ( fMax0 < fMax1 )
            fMax0 = fMax1;
        if ( fMin0 > fMin1 )
            fMin0 = fMin1;

        // Update the average center to be the center of the bounding box
        // defined by the projected intervals.
        rkC += (((Real)0.5)*(fMin0+fMax0))*kDir;

        // Compute the extents of the box based on the new center.
        Real fExtent = ((Real)0.5)*(fMax0-fMin0);

        // Compute the minimum volume ellipsoid centered at the box center and
        // passing through the eight box vertices.  For an axis-aligned
        // ellipsoid (x/a)^2+(y/b)^2+(z/c)^2=1 containing (x0,y0,z0), the axis
        // lengths must satisfy (x0/a)^2 = (y0/b)^2 = (z0/c)^2 = 1/3.
        afD[i] = ((Real)1.0)/(((Real)3.0)*fExtent*fExtent);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Ellipsoid3<float> ContEllipsoid<float> (int,
    const Vector3<float>*);
template WML_ITEM void ProjectEllipsoid <float>(const Vector3<float>&,
   const Vector3<float>&, const Vector3<float>&, const Matrix3<float>&,
   const float[3], float&, float&);
template WML_ITEM void MergeEllipsoids<float> (const Vector3<float>&,
    const Matrix3<float>&, const float[3], const Vector3<float>&,
    const Matrix3<float>&, const float[3], Vector3<float>&,
    Matrix3<float>&, float[3]);

template WML_ITEM Ellipsoid3<double> ContEllipsoid<double> (int,
    const Vector3<double>*);
template WML_ITEM void ProjectEllipsoid <double>(const Vector3<double>&,
   const Vector3<double>&, const Vector3<double>&, const Matrix3<double>&,
   const double[3], double&, double&);
template WML_ITEM void MergeEllipsoids<double> (const Vector3<double>&,
    const Matrix3<double>&, const double[3], const Vector3<double>&,
    const Matrix3<double>&, const double[3], Vector3<double>&,
    Matrix3<double>&, double[3]);
}
//----------------------------------------------------------------------------
