// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLINEARSYSTEM_H
#define WMLLINEARSYSTEM_H

#include "WmlBandedMatrix.h"
#include "WmlGMatrix.h"
#include <map>

namespace Wml
{

template <class Real>
class WML_ITEM LinearSystem
{
public:
    // 2x2 and 3x3 systems (avoids overhead of Gaussian elimination)
    static Real& Tolerance ();
    static bool Solve2 (const Real aafA[2][2], const Real afB[2],
        Real afX[2]);
    static bool Solve3 (const Real aafA[3][3], const Real afB[3],
        Real afX[3]);

    // Input:
    //     A[iSize][iSize], entries are A[row][col]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     InvA[iSize][iSize], inverse matrix
    static bool Inverse (const GMatrix<Real>& rkA, GMatrix<Real>& rkInvA);

    // Input:
    //     A[iSize][iSize] coefficient matrix, entries are A[row][col]
    //     B[iSize] vector, entries are B[row]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     X[iSize] is solution X to AX = B
    static bool Solve (const GMatrix<Real>& rkA, const Real* afB,
        Real* afX);

    // Input:
    //     Matrix is tridiagonal.
    //     Lower diagonal A[iSize-1]
    //     Main  diagonal B[iSize]
    //     Upper diagonal C[iSize-1]
    //     Right-hand side R[iSize]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     U[iSize] is solution
    static bool SolveTri (int iSize, Real* afA, Real* afB, Real* afC,
        Real* afR, Real* afU);

    // Input:
    //     Matrix is tridiagonal.
    //     Lower diagonal is constant, A
    //     Main  diagonal is constant, B
    //     Upper diagonal is constant, C
    //     Right-hand side Rr[iSize]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     U[iSize] is solution
    static bool SolveConstTri (int iSize, Real fA, Real fB, Real fC,
        Real* afR, Real* afU);

    // Input:
    //     A[iSize][iSize] symmetric matrix, entries are A[row][col]
    //     B[iSize] vector, entries are B[row]
    // Output:
    //     return value is TRUE if successful, FALSE if (nearly) singular
    //     X[iSize] is solution X to AX = B
    static bool SolveSymmetric (const GMatrix<Real>& rkA,
        const Real* afB, Real* afX);

    // Input:
    //     A[iSize][iSize] symmetric matrix, entries are A[row][col]
    // Output:
    //     return value is TRUE if successful, FALSE if algorithm failed
    //     InvA[iSize][iSize], inverse matrix
    static bool SymmetricInverse (const GMatrix<Real>& rkA,
        GMatrix<Real>& rkInvA);

    // Solution using the conjugate gradient method.
    // Input:
    //    A[iSize][iSize] symmetrix matrix, entries are A[row][col]
    //    B[iSize] vector, entries are B[row]
    // Output:
    //    X[iSize] is the solution x to Ax = B
    static bool SolveSymmetricCG (const GMatrix<Real>& rkA, const Real* afB,
        Real* afX);

    // Conjugate gradient method for sparse, symmetric matrices.
    // Input:
    //    The nonzero entries of the symmetrix matrix A are stored in a map
    //    whose keys are pairs (i,j) and whose values are real numbers.  The
    //    pair (i,j) is the location of the value in the array.  Only one of
    //    (i,j) and (j,i) should be stored since A is symmetric.  The code
    //    assumes this is how you set up A.  The column vector B is stored as
    //    an array of contiguous values.
    // Output:
    //    X[iSize] is the solution x to Ax = B
    typedef std::map<std::pair<int,int>,Real> SparseMatrix;
    static bool SolveSymmetricCG (int iSize, const SparseMatrix& rkA,
        const Real* afB, Real* afX);

    // solve banded matrix systems
    // Input:
    //     A, a banded matrix
    //     B[iSize] vector, entries are B[row]
    // Output:
    //     return value is TRUE if successful, FALSE if pivoting failed
    //     X[iSize] is solution X to AX = B
    static bool SolveBanded (const BandedMatrix<Real>& rkA, const Real* afB,
        Real* afX);

    // invert a banded matrix
    // Input:
    //     A, a banded matrix
    // Output:
    //     return value is TRUE if the inverse exists, FALSE otherwise
    //     InvA, the inverse of A
    static bool Invert (const BandedMatrix<Real>& rkA,
        GMatrix<Real>& rkInvA);

protected:
    // support for the conjugate gradient method for standard arrays
    static Real Dot (int iSize, const Real* afU, const Real* afV);
    static void Multiply (const GMatrix<Real>& rkA, const Real* afX,
        Real* afProd);
    static void UpdateX (int iSize, Real* afX, Real fAlpha, const Real* afP);
    static void UpdateR (int iSize, Real* afR, Real fAlpha, const Real* afW);
    static void UpdateP (int iSize, Real* afP, Real fBeta, const Real* afR);

    // support for the conjugate gradient method for sparse arrays
    static void Multiply (int iSize, const SparseMatrix& rkA,
        const Real* afX, Real* afProd);

    // support for banded matrices
    static bool ForwardEliminate (int iReduceRow, BandedMatrix<Real>& rkA,
        Real* afB);
    static bool ForwardEliminate (int iReduceRow, BandedMatrix<Real>& rkA,
        GMatrix<Real>& rkB);
    static void BackwardEliminate (int iReduceRow, BandedMatrix<Real>& rkA,
        GMatrix<Real>& rkB);

    // tolerance for 2x2 and 3x3 system solving
    static Real ms_fTolerance;
};

typedef LinearSystem<float> LinearSystemf;
typedef LinearSystem<double> LinearSystemd;

}

#endif
