// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprQuadraticFit2.h"
#include "WmlEigen.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real Wml::QuadraticFit (int iQuantity, const Vector2<Real>* akPoint,
    Real afCoeff[6])
{
    Eigen<Real> kES(6);
    int iRow, iCol;
    for (iRow = 0; iRow < 6; iRow++)
    {
        for (iCol = 0; iCol < 6; iCol++)
            kES(iRow,iCol) = (Real)0.0;
    }

    for (int i = 0; i < iQuantity; i++)
    {
        Real fX = akPoint[i].X();
        Real fY = akPoint[i].Y();
        Real fX2 = fX*fX;
        Real fY2 = fY*fY;
        Real fXY = fX*fY;
        Real fX3 = fX*fX2;
        Real fXY2 = fX*fY2;
        Real fX2Y = fX*fXY;
        Real fY3 = fY*fY2;
        Real fX4 = fX*fX3;
        Real fX2Y2 = fX*fXY2;
        Real fX3Y = fX*fX2Y;
        Real fY4 = fY*fY3;
        Real fXY3 = fX*fY3;

        kES(0,1) += fX;
        kES(0,2) += fY;
        kES(0,3) += fX2;
        kES(0,4) += fY2;
        kES(0,5) += fXY;
        kES(1,3) += fX3;
        kES(1,4) += fXY2;
        kES(1,5) += fX2Y;
        kES(2,4) += fY3;
        kES(3,3) += fX4;
        kES(3,4) += fX2Y2;
        kES(3,5) += fX3Y;
        kES(4,4) += fY4;
        kES(4,5) += fXY3;
    }

    kES(0,0) = (Real)iQuantity;
    kES(1,1) = kES(0,3);
    kES(1,2) = kES(0,5);
    kES(2,2) = kES(0,4);
    kES(2,3) = kES(1,5);
    kES(2,5) = kES(1,4);
    kES(5,5) = kES(3,4);

    for (iRow = 0; iRow < 6; iRow++)
    {
        for (iCol = 0; iCol < iRow; iCol++)
            kES(iRow,iCol) = kES(iCol,iRow);
    }

    Real fInvQuantity = ((Real)1.0)/(Real)iQuantity;
    for (iRow = 0; iRow < 6; iRow++)
    {
        for (iCol = 0; iCol < 6; iCol++)
            kES(iRow,iCol) *= fInvQuantity;
    }

    kES.IncrSortEigenStuffN();

    GVector<Real> kEVector = kES.GetEigenvector(0);
    memcpy(afCoeff,(Real*)kEVector,6*sizeof(Real));

    // For exact fit, numeric round-off errors may make the minimum
    // eigenvalue just slightly negative.  Return absolute value since
    // application may rely on the return value being nonnegative.
    return Math<Real>::FAbs(kES.GetEigenvalue(0));
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::QuadraticCircleFit (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkCenter, Real& rfRadius)
{
    Eigen<Real> kES(4);
    int iRow, iCol;
    for (iRow = 0; iRow < 4; iRow++)
    {
        for (iCol = 0; iCol < 4; iCol++)
            kES(iRow,iCol) = (Real)0.0;
    }

    for (int i = 0; i < iQuantity; i++)
    {
        Real fX = akPoint[i].X();
        Real fY = akPoint[i].Y();
        Real fX2 = fX*fX;
        Real fY2 = fY*fY;
        Real fXY = fX*fY;
        Real fR2 = fX2+fY2;
        Real fXR2 = fX*fR2;
        Real fYR2 = fY*fR2;
        Real fR4 = fR2*fR2;

        kES(0,1) += fX;
        kES(0,2) += fY;
        kES(0,3) += fR2;
        kES(1,1) += fX2;
        kES(1,2) += fXY;
        kES(1,3) += fXR2;
        kES(2,2) += fY2;
        kES(2,3) += fYR2;
        kES(3,3) += fR4;
    }

    kES(0,0) = (Real)iQuantity;

    for (iRow = 0; iRow < 4; iRow++)
    {
        for (iCol = 0; iCol < iRow; iCol++)
            kES(iRow,iCol) = kES(iCol,iRow);
    }

    Real fInvQuantity = ((Real)1.0)/(Real)iQuantity;
    for (iRow = 0; iRow < 4; iRow++)
    {
        for (iCol = 0; iCol < 4; iCol++)
            kES(iRow,iCol) *= fInvQuantity;
    }

    kES.IncrSortEigenStuffN();

    GVector<Real> kEVector = kES.GetEigenvector(0);
    Real fInv = ((Real)1.0)/kEVector[3];  // beware zero divide
    Real afCoeff[3];
    for (iRow = 0; iRow < 3; iRow++)
        afCoeff[iRow] = fInv*kEVector[iRow];

    rkCenter.X() = -((Real)0.5)*afCoeff[1];
    rkCenter.Y() = -((Real)0.5)*afCoeff[2];
    rfRadius = Math<Real>::Sqrt(Math<Real>::FAbs(rkCenter.X()*rkCenter.X() +
        rkCenter.Y()*rkCenter.Y() - afCoeff[0]));

    // For exact fit, numeric round-off errors may make the minimum
    // eigenvalue just slightly negative.  Return absolute value since
    // application may rely on the return value being nonnegative.
    return Math<Real>::FAbs(kES.GetEigenvalue(0));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM float QuadraticFit<float> (int,
    const Vector2<float>*, float[6]);
template WML_ITEM float QuadraticCircleFit<float> (int,
    const Vector2<float>*, Vector2<float>&, float&);

template WML_ITEM double QuadraticFit<double> (int,
    const Vector2<double>*, double[6]);
template WML_ITEM double QuadraticCircleFit<double> (int,
    const Vector2<double>*, Vector2<double>&, double&);
}
//----------------------------------------------------------------------------
