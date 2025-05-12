// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprLineFit2.h"
#include "WmlEigen.h"
#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::HeightLineFit (int iQuantity, const Vector2<Real>* akPoint,
    Real& rfA, Real& rfB)
{
    // You need at least two points to determine the line.  Even so, if
    // the points are on a vertical line, there is no least-squares fit in
    // the 'height' sense.  This will be trapped by the determinant of the
    // coefficient matrix.
    if ( iQuantity < 2 )
        return false;

    // compute sums for linear system
    Real fSumX = (Real)0.0, fSumY = (Real)0.0;
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0;
    for (int i = 0; i < iQuantity; i++)
    {
        fSumX += akPoint[i].X();
        fSumY += akPoint[i].Y();
        fSumXX += akPoint[i].X()*akPoint[i].X();
        fSumXY += akPoint[i].X()*akPoint[i].Y();
    }

    Real aafA[2][2] =
    {
        {fSumXX, fSumX},
        {fSumX,  Real(iQuantity)}
    };

    Real afB[2] =
    {
        fSumXY,
        fSumY
    };

    Real afX[2];

    bool bNonsingular = LinearSystem<Real>::Solve2(aafA,afB,afX);
    if ( bNonsingular )
    {
        rfA = afX[0];
        rfB = afX[1];
    }
    else
    {
        rfA = Math<Real>::MAX_REAL;
        rfB = Math<Real>::MAX_REAL;
    }

    return bNonsingular;
}
//----------------------------------------------------------------------------
template <class Real>
void Wml::OrthogonalLineFit (int iQuantity, const Vector2<Real>* akPoint,
    Vector2<Real>& rkOffset, Vector2<Real>& rkDirection)
{
    // compute average of points
    rkOffset = akPoint[0];
    int i;
    for (i = 1; i < iQuantity; i++)
        rkOffset += akPoint[i];
    Real fInvQuantity = ((Real)1.0)/iQuantity;
    rkOffset *= fInvQuantity;

    // compute sums of products
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumYY = (Real)0.0;
    for (i = 0; i < iQuantity; i++) 
    {
        Vector2<Real> kDiff = akPoint[i] - rkOffset;
        fSumXX += kDiff.X()*kDiff.X();
        fSumXY += kDiff.X()*kDiff.Y();
        fSumYY += kDiff.Y()*kDiff.Y();
    }

    // setup the eigensolver
    Eigen<Real> kES(2);
    kES(0,0) = fSumYY;
    kES(0,1) = -fSumXY;
    kES(1,0) = kES(0,1);
    kES(1,1) = fSumXX;

    // compute eigenstuff, smallest eigenvalue is in last position
    kES.DecrSortEigenStuff2();

    // unit-length direction for best-fit line
    kES.GetEigenvector(1,rkDirection);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::OrthogonalLineFit (int iQuantity, const Vector2<Real>* akPoint,
    const bool* abValid, Vector2<Real>& rkOffset, Vector2<Real>& rkDirection)
{
    // compute average of points
    rkOffset = Vector2<Real>::ZERO;
    int i, iValidQuantity = 0;
    for (i = 0; i < iQuantity; i++)
    {
        if ( abValid[i] )
        {
            rkOffset += akPoint[i];
            iValidQuantity++;
        }
    }
    if ( iValidQuantity == 0 )
        return false;

    Real fInvQuantity = ((Real)1.0)/iValidQuantity;
    rkOffset *= fInvQuantity;

    // compute sums of products
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumYY = (Real)0.0;
    for (i = 0; i < iQuantity; i++) 
    {
        if ( abValid[i] )
        {
            Vector2<Real> kDiff = akPoint[i] - rkOffset;
            fSumXX += kDiff.X()*kDiff.X();
            fSumXY += kDiff.X()*kDiff.Y();
            fSumYY += kDiff.Y()*kDiff.Y();
        }
    }

    // setup the eigensolver
    Eigen<Real> kES(2);
    kES(0,0) = fSumYY;
    kES(0,1) = -fSumXY;
    kES(1,0) = kES(0,1);
    kES(1,1) = fSumXX;

    // compute eigenstuff, smallest eigenvalue is in last position
    kES.DecrSortEigenStuff2();

    // unit-length direction for best-fit line
    kES.GetEigenvector(1,rkDirection);

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool HeightLineFit<float> (int,
    const Vector2<float>*, float&, float&);
template WML_ITEM void OrthogonalLineFit<float> (int,
    const Vector2<float>*, Vector2<float>&, Vector2<float>&);
template WML_ITEM bool OrthogonalLineFit<float> (int,
    const Vector2<float>*, const bool*, Vector2<float>&,
    Vector2<float>&);

template WML_ITEM bool HeightLineFit<double> (int,
    const Vector2<double>*, double&, double&);
template WML_ITEM void OrthogonalLineFit<double> (int,
    const Vector2<double>*, Vector2<double>&, Vector2<double>&);
template WML_ITEM bool OrthogonalLineFit<double> (int,
    const Vector2<double>*, const bool*, Vector2<double>&,
    Vector2<double>&);
}
//----------------------------------------------------------------------------
