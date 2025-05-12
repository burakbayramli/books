// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprGaussPointsFit2.h"
#include "WmlEigen.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::GaussPointsFit (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkCenter, Vector2<Real> akAxis[2], Real afExtent[2])
{
    // compute mean of points
    rkCenter = akPoint[0];
    int i;
    for (i = 1; i < iQuantity; i++)
        rkCenter += akPoint[i];
    Real fInvQuantity = ((Real)1.0)/iQuantity;
    rkCenter *= fInvQuantity;

    // compute covariances of points
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumYY = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        Vector2<Real> kDiff = akPoint[i] - rkCenter;
        fSumXX += kDiff.X()*kDiff.X();
        fSumXY += kDiff.X()*kDiff.Y();
        fSumYY += kDiff.Y()*kDiff.Y();
    }
    fSumXX *= fInvQuantity;
    fSumXY *= fInvQuantity;
    fSumYY *= fInvQuantity;

    // solve eigensystem of covariance matrix
    Eigen<Real> kES(2);
    kES(0,0) = fSumXX;
    kES(0,1) = fSumXY;
    kES(1,0) = fSumXY;
    kES(1,1) = fSumYY;
    kES.IncrSortEigenStuff2();

    for (i = 0; i < 2; i++)
    {
        afExtent[i] = kES.GetEigenvalue(i);
        kES.GetEigenvector(i,akAxis[i]);
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::GaussPointsFit (int iQuantity, const Vector2<Real>* akPoint,
    const bool* abValid, Vector2<Real>& rkCenter, Vector2<Real> akAxis[2],
    Real afExtent[2])
{
    // compute mean of points
    rkCenter = Vector2<Real>::ZERO;
    int i, iValidQuantity = 0;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            rkCenter += akPoint[i];
            iValidQuantity++;
        }
    }
    if ( iValidQuantity == 0 )
        return false;

    Real fInvQuantity = ((Real)1.0)/iValidQuantity;
    rkCenter *= fInvQuantity;

    // compute covariances of points
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumYY = (Real)0.0;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            Vector2<Real> kDiff = akPoint[i] - rkCenter;
            fSumXX += kDiff.X()*kDiff.X();
            fSumXY += kDiff.X()*kDiff.Y();
            fSumYY += kDiff.Y()*kDiff.Y();
        }
    }
    fSumXX *= fInvQuantity;
    fSumXY *= fInvQuantity;
    fSumYY *= fInvQuantity;

    // solve eigensystem of covariance matrix
    Eigen<Real> kES(2);
    kES(0,0) = fSumXX;
    kES(0,1) = fSumXY;
    kES(1,0) = fSumXY;
    kES(1,1) = fSumYY;
    kES.IncrSortEigenStuff2();

    for (i = 0; i < 2; i++)
    {
        afExtent[i] = kES.GetEigenvalue(i);
        kES.GetEigenvector(i,akAxis[i]);
    }

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void GaussPointsFit<float> (int,
    const Vector2<float>*, Vector2<float>&, Vector2<float>[2], float[2]);
template WML_ITEM bool GaussPointsFit<float> (int,
    const Vector2<float>*, const bool*, Vector2<float>&, Vector2<float>[2],
    float[2]);

template WML_ITEM void GaussPointsFit<double> (int,
    const Vector2<double>*, Vector2<double>&, Vector2<double>[2], double[2]);
template WML_ITEM bool GaussPointsFit<double> (int,
    const Vector2<double>*, const bool*, Vector2<double>&, Vector2<double>[2],
    double[2]);
}
//----------------------------------------------------------------------------
