// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlContEllipse2.h"
#include "WmlApprGaussPointsFit2.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Ellipse2<Real> Wml::ContEllipse (int iQuantity, const Vector2<Real>* akPoint)
{
    Ellipse2<Real> kEllipse;

    // Fit the points with a Gaussian distribution.  The covariance matrix
    // is M = D[0]*U[0]*U[0]^T+D[1]*U[1]*U[1]^T where D represents afD and
    // U represents akAxis.
    Vector2<Real> akAxis[2];
    Real afD[2];
    GaussPointsFit<Real>(iQuantity,akPoint,kEllipse.Center(),akAxis,afD);

    // Grow the ellipse, while retaining its shape determined by the
    // covariance matrix, to enclose all the input points.  The quadratic form
    // that is used for the ellipse construction is
    //
    //   Q(X) = (X-C)^T*M*(X-C)
    //        = (X-C)^T*(D[0]*U[0]*U[0]^T+D[1]*U[1]*U[1]^T)*(X-C)
    //        = D[0]*Dot(U[0],X-C)^2 + D[1]*Dot(U[1],X-C)^2
    //
    // If the maximum value of Q(X[i]) for all input points is V^2, then a
    // bounding ellipse is Q(X) = V^2 since Q(X[i]) <= V^2 for all i.

    afD[0] = ((Real)1.0)/Math<Real>::FAbs(afD[0]);
    afD[1] = ((Real)1.0)/Math<Real>::FAbs(afD[1]);

    Real fMaxValue = (Real)0.0;
    for (int i = 0; i < iQuantity; i++)
    {
        Vector2<Real> kDiff = akPoint[i] - kEllipse.Center();
        Real afDot[2] =
        {
            akAxis[0].Dot(kDiff),
            akAxis[1].Dot(kDiff)
        };

        Real fValue = afD[0]*afDot[0]*afDot[0] + afD[1]*afDot[1]*afDot[1];
        if ( fValue > fMaxValue )
            fMaxValue = fValue;
    }

    Real fInv = ((Real)1.0)/fMaxValue;
    afD[0] *= fInv;
    afD[1] *= fInv;

    Matrix2<Real> akTensor0(akAxis[0],akAxis[0]);
    Matrix2<Real> akTensor1(akAxis[1],akAxis[1]);
    kEllipse.A() = afD[0]*akTensor0 + afD[1]*akTensor1;

    return kEllipse;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM Ellipse2<float> ContEllipse<float> (int,
    const Vector2<float>*);

template WML_ITEM Ellipse2<double> ContEllipse<double> (int,
    const Vector2<double>*);
}
//----------------------------------------------------------------------------
