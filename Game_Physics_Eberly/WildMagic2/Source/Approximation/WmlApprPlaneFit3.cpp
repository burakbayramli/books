// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApprPlaneFit3.h"
#include "WmlEigen.h"
#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::HeightPlaneFit (int iQuantity, Vector3<Real>* akPoint, Real& rfA,
    Real& rfB, Real& rfC)
{
    // You need at least three points to determine the plane.  Even so, if
    // the points are on a vertical plane, there is no least-squares fit in
    // the 'height' sense.  This will be trapped by the determinant of the
    // coefficient matrix.
    if ( iQuantity < 3 )
        return false;

    // compute sums for linear system
    Real fSumX = (Real)0.0, fSumY = (Real)0.0, fSumZ = (Real)0.0;
    Real fSumXX = (Real)0.0, fSumXY = (Real)0.0, fSumXZ = (Real)0.0;
    Real fSumYY = (Real)0.0, fSumYZ = (Real)0.0;
    for (int i = 0; i < iQuantity; i++)
    {
        fSumX += akPoint[i].X();
        fSumY += akPoint[i].Y();
        fSumZ += akPoint[i].Z();
        fSumXX += akPoint[i].X()*akPoint[i].X();
        fSumXY += akPoint[i].X()*akPoint[i].Y();
        fSumXZ += akPoint[i].X()*akPoint[i].Z();
        fSumYY += akPoint[i].Y()*akPoint[i].Y();
        fSumYZ += akPoint[i].Y()*akPoint[i].Z();
    }

    Real aafA[3][3] =
    {
        {fSumXX, fSumXY, fSumX},
        {fSumXY, fSumYY, fSumY},
        {fSumX,  fSumY,  (Real)iQuantity}
    };

    Real afB[3] =
    {
        fSumXZ,
        fSumYZ,
        fSumZ
    };

    Real afX[3];

    bool bNonsingular = LinearSystem<Real>::Solve3(aafA,afB,afX);
    if ( bNonsingular )
    {
        rfA = afX[0];
        rfB = afX[1];
        rfC = afX[2];
    }
    else
    {
        rfA = Math<Real>::MAX_REAL;
        rfB = Math<Real>::MAX_REAL;
        rfC = Math<Real>::MAX_REAL;
    }

    return bNonsingular;
}
//----------------------------------------------------------------------------
template <class Real>
Real Wml::OrthogonalPlaneFit (int iQuantity, Vector3<Real>* akPoint,
    Vector3<Real>& rkOffset, Vector3<Real>& rkNormal)
{
    // compute average of points
    rkOffset = akPoint[0];
    int i;
    for (i = 1; i < iQuantity; i++)
        rkOffset += akPoint[i];
    Real fInvQuantity = ((Real)1.0)/(Real)iQuantity;
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

    // setup the eigensolver
    Eigen<Real> kES(3);
    kES(0,0) = fSumXX;
    kES(0,1) = fSumXY;
    kES(0,2) = fSumXZ;
    kES(1,0) = fSumXY;
    kES(1,1) = fSumYY;
    kES(1,2) = fSumYZ;
    kES(2,0) = fSumXZ;
    kES(2,1) = fSumYZ;
    kES(2,2) = fSumZZ;

    // compute eigenstuff, smallest eigenvalue is in last position
    kES.DecrSortEigenStuff3();

    // unit-length direction for best-fit line
    kES.GetEigenvector(2,rkNormal);

    // the minimum energy
    return Math<Real>::FAbs(kES.GetEigenvalue(2));
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool HeightPlaneFit<float> (int,
    Vector3<float>*, float&, float&, float&);
template WML_ITEM float OrthogonalPlaneFit<float> (int,
    Vector3<float>*, Vector3<float>&, Vector3<float>&);

template WML_ITEM bool HeightPlaneFit<double> (int,
    Vector3<double>*, double&, double&, double&);
template WML_ITEM double OrthogonalPlaneFit<double> (int,
    Vector3<double>*, Vector3<double>&, Vector3<double>&);
}
//----------------------------------------------------------------------------
