// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.


// Fitting a Paraboloid Uusing Least Squares
//
// Given a set of samples (x_i,y_i,z_i) for 0 <= i < N and assuming
// that the true values lie on a paraboloid
//     z = p0*x*x + p1*x*y + p2*y*y + p3*x + p4*y + p5  = Dot(P,Q(x,y))
// where P = (p0,p1,p2,p3,p4,p5) and Q(x,y) = (x*x,x*y,y*y,x,y,1),
// select P to minimize the sum of squared errors
//     E(P) = sum_{i=0}^{N-1} [Dot(P,Q_i)-z_i]^2
// where Q_i = Q(x_i,y_i).
//
// The minimum occurs when the gradient of E is the zero vector,
//     grad(E) = 2 sum_{i=0}^{N-1} [Dot(P,Q_i)-z_i] Q_i = 0
// Some algebra converts this to a system of 6 equations in 6 unknowns:
//     [(sum_{i=0}^{N-1} Q_i Q_i^t] P = sum_{i=0}^{N-1} z_i Q_i
// The product Q_i Q_i^t is a product of the 6x1 matrix Q_i with the
// 1x6 matrix Q_i^t, the result being a 6x6 matrix.
//
// Define the 6x6 symmetric matrix A = sum_{i=0}^{N-1} Q_i Q_i^t and the 6x1
// vector B = sum_{i=0}^{N-1} z_i Q_i.  The choice for P is the solution to
// the linear system of equations A*P = B.  The entries of A and B indicate
// summations over the appropriate product of variables.  For example,
// s(x^3 y) = sum_{i=0}^{N-1} x_i^3 y_i.
//
// +-                                                     -++  +   +-      -+
// | s(x^4) s(x^3 y)   s(x^2 y^2) s(x^3)   s(x^2 y) s(x^2) ||p0|   |s(z x^2)|
// |        s(x^2 y^2) s(x y^3)   s(x^2 y) s(x y^2) s(x y) ||p1|   |s(z x y)|
// |                   s(y^4)     s(x y^2) s(y^3)   s(y^2) ||p2| = |s(z y^2)|
// |                              s(x^2)   s(x y)   s(x)   ||p3|   |s(z x)  |
// |                                       s(y^2)   s(y)   ||p4|   |s(z y)  |
// |                                                s(1)   ||p5|   |s(z)    |
// +-                                                     -++  +   +-      -+

#include "WmlApprParaboloidFit3.h"
#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
bool Wml::ParaboloidFit (int iQuantity, const Vector3<Real>* akPoint,
    Real afCoeff[6])
{
    // allocate linear system (matrix is zeroed initially)
    GMatrix<Real> kMat(6,6);
    Real afRHS[6];
    memset(afRHS,0,6*sizeof(Real));

    for (int i = 0; i < iQuantity; i++)
    {
        Real fX2 = akPoint[i].X()*akPoint[i].X();
        Real fXY = akPoint[i].X()*akPoint[i].Y();
        Real fY2 = akPoint[i].Y()*akPoint[i].Y();
        Real fZX = akPoint[i].Z()*akPoint[i].X();
        Real fZY = akPoint[i].Z()*akPoint[i].Y();
        Real fX3 = akPoint[i].X()*fX2;
        Real fX2Y = fX2*akPoint[i].Y();
        Real fXY2 = akPoint[i].X()*fY2;
        Real fY3 = akPoint[i].Y()*fY2;
        Real fZX2 = akPoint[i].Z()*fX2;
        Real fZXY = akPoint[i].Z()*fXY;
        Real fZY2 = akPoint[i].Z()*fY2;
        Real fX4 = fX2*fX2;
        Real fX3Y = fX3*akPoint[i].Y();
        Real fX2Y2 = fX2*fY2;
        Real fXY3 = akPoint[i].X()*fY3;
        Real fY4 = fY2*fY2;

        kMat[0][0] += fX4;
        kMat[0][1] += fX3Y;
        kMat[0][2] += fX2Y2;
        kMat[0][3] += fX3;
        kMat[0][4] += fX2Y;
        kMat[0][5] += fX2;
        kMat[1][2] += fXY3;
        kMat[1][4] += fXY2;
        kMat[1][5] += fXY;
        kMat[2][2] += fY4;
        kMat[2][4] += fY3;
        kMat[2][5] += fY2;
        kMat[3][3] += fX2;
        kMat[3][5] += akPoint[i].X();
        kMat[4][5] += akPoint[i].Y();

        afRHS[0] += fZX2;
        afRHS[1] += fZXY;
        afRHS[2] += fZY2;
        afRHS[3] += fZX;
        afRHS[4] += fZY;
        afRHS[5] += akPoint[i].Z();
    }

    kMat[1][0] = kMat[0][1];
    kMat[1][1] = kMat[0][2];
    kMat[1][3] = kMat[0][4];
    kMat[2][0] = kMat[0][2];
    kMat[2][1] = kMat[1][2];
    kMat[2][3] = kMat[1][4];
    kMat[3][0] = kMat[0][3];
    kMat[3][1] = kMat[1][3];
    kMat[3][2] = kMat[2][3];
    kMat[3][4] = kMat[1][5];
    kMat[4][0] = kMat[0][4];
    kMat[4][1] = kMat[1][4];
    kMat[4][2] = kMat[2][4];
    kMat[4][3] = kMat[3][4];
    kMat[4][4] = kMat[2][5];
    kMat[5][0] = kMat[0][5];
    kMat[5][1] = kMat[1][5];
    kMat[5][2] = kMat[2][5];
    kMat[5][3] = kMat[3][5];
    kMat[5][4] = kMat[4][5];
    kMat[5][5] = (Real)iQuantity;

    return LinearSystem<Real>::SolveSymmetric(kMat,afRHS,afCoeff);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template WML_ITEM bool ParaboloidFit<float> (int,
    const Vector3<float>*, float[6]);

template WML_ITEM bool ParaboloidFit<double> (int,
    const Vector3<double>*, double[6]);
}
//----------------------------------------------------------------------------
