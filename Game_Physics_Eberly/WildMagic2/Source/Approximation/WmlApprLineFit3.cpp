// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprLineFit3.h"
#include "WmlEigen.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
void Wml::OrthogonalLineFit (int iQuantity, const Vector3<Real>* akPoint,
    Vector3<Real>& rkOffset, Vector3<Real>& rkDirection)
{
    // compute average of points
    rkOffset = akPoint[0];
    int i;
    for (i = 1; i < iQuantity; i++)
        rkOffset += akPoint[i];
    Real fInvQuantity = ((Real)1.0)/iQuantity;
    rkOffset *= fInvQuantity;

    // compute sums of products
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumXZ = (Real)0.0;
    Real fSumYY = (Real)0.0, fSumYZ = (Real)0.0, fSumZZ = (Real)0.0;
    for (i = 0; i < iQuantity; i++) 
    {
        Vector3<Real> kDiff = akPoint[i] - rkOffset;
        fSumXX += kDiff.X()*kDiff.X();
        fSumXY += kDiff.X()*kDiff.Y();
        fSumXZ += kDiff.X()*kDiff.Z();
        fSumYY += kDiff.Y()*kDiff.Y();
        fSumYZ += kDiff.Y()*kDiff.Z();
        fSumZZ += kDiff.Z()*kDiff.Z();
    }
    fSumXX *= fInvQuantity;
    fSumXY *= fInvQuantity;
    fSumXZ *= fInvQuantity;
    fSumYY *= fInvQuantity;
    fSumYZ *= fInvQuantity;
    fSumZZ *= fInvQuantity;

    // setup the eigensolver
    Eigen<Real> kES(3);
    kES(0,0) = fSumYY+fSumZZ;
    kES(0,1) = -fSumXY;
    kES(0,2) = -fSumXZ;
    kES(1,0) = kES(0,1);
    kES(1,1) = fSumXX+fSumZZ;
    kES(1,2) = -fSumYZ;
    kES(2,0) = kES(0,2);
    kES(2,1) = kES(1,2);
    kES(2,2) = fSumXX+fSumYY;

    // compute eigenstuff, smallest eigenvalue is in last position
    kES.DecrSortEigenStuff3();

    // unit-length direction for best-fit line
    kES.GetEigenvector(2,rkDirection);
}
//----------------------------------------------------------------------------
template <class Real>
bool Wml::OrthogonalLineFit (int iQuantity, const Vector3<Real>* akPoint,
    const bool* abValid, Vector3<Real>& rkOffset, Vector3<Real>& rkDirection)
{
    // compute average of points
    rkOffset = Vector3<Real>::ZERO;
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

    Real fInvQuantity = ((Real)1.0)/iQuantity;
    rkOffset *= fInvQuantity;

    // compute sums of products
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumXZ = (Real)0.0;
    Real fSumYY = (Real)0.0, fSumYZ = (Real)0.0, fSumZZ = (Real)0.0;
    for (i = 0; i < iQuantity; i++) 
    {
        if ( abValid[i] )
        {
            Vector3<Real> kDiff = akPoint[i] - rkOffset;
            fSumXX += kDiff.X()*kDiff.X();
            fSumXY += kDiff.X()*kDiff.Y();
            fSumXZ += kDiff.X()*kDiff.Z();
            fSumYY += kDiff.Y()*kDiff.Y();
            fSumYZ += kDiff.Y()*kDiff.Z();
            fSumZZ += kDiff.Z()*kDiff.Z();
        }
    }
    fSumXX *= fInvQuantity;
    fSumXY *= fInvQuantity;
    fSumXZ *= fInvQuantity;
    fSumYY *= fInvQuantity;
    fSumYZ *= fInvQuantity;
    fSumZZ *= fInvQuantity;

    // setup the eigensolver
    Eigen<Real> kES(3);
    kES(0,0) = fSumYY+fSumZZ;
    kES(0,1) = -fSumXY;
    kES(0,2) = -fSumXZ;
    kES(1,0) = kES(0,1);
    kES(1,1) = fSumXX+fSumZZ;
    kES(1,2) = -fSumYZ;
    kES(2,0) = kES(0,2);
    kES(2,1) = kES(1,2);
    kES(2,2) = fSumXX+fSumYY;

    // compute eigenstuff, smallest eigenvalue is in last position
    kES.DecrSortEigenStuff3();

    // unit-length direction for best-fit line
    kES.GetEigenvector(2,rkDirection);
    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM void OrthogonalLineFit<float> (int,
    const Vector3<float>*, Vector3<float>&, Vector3<float>&);
template WML_ITEM bool OrthogonalLineFit<float> (int,
    const Vector3<float>*, const bool*, Vector3<float>&, Vector3<float>&);

template WML_ITEM void OrthogonalLineFit<double> (int,
    const Vector3<double>*, Vector3<double>&, Vector3<double>&);
template WML_ITEM bool OrthogonalLineFit<double> (int,
    const Vector3<double>*, const bool*, Vector3<double>&, Vector3<double>&);
}
//----------------------------------------------------------------------------
