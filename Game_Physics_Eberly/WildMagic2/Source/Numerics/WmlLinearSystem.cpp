// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLinearSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
Real& LinearSystem<Real>::Tolerance ()
{
    return ms_fTolerance;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::Solve2 (const Real aafA[2][2], const Real afB[2],
    Real afX[2])
{
    Real fDet = aafA[0][0]*aafA[1][1]-aafA[0][1]*aafA[1][0];
    if ( Math<Real>::FAbs(fDet) < ms_fTolerance )
        return false;

    Real fInvDet = ((Real)1.0)/fDet;
    afX[0] = (aafA[1][1]*afB[0]-aafA[0][1]*afB[1])*fInvDet;
    afX[1] = (aafA[0][0]*afB[1]-aafA[1][0]*afB[0])*fInvDet;
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::Solve3 (const Real aafA[3][3], const Real afB[3],
    Real afX[3])
{
    Real aafAInv[3][3];
    aafAInv[0][0] = aafA[1][1]*aafA[2][2]-aafA[1][2]*aafA[2][1];
    aafAInv[0][1] = aafA[0][2]*aafA[2][1]-aafA[0][1]*aafA[2][2];
    aafAInv[0][2] = aafA[0][1]*aafA[1][2]-aafA[0][2]*aafA[1][1];
    aafAInv[1][0] = aafA[1][2]*aafA[2][0]-aafA[1][0]*aafA[2][2];
    aafAInv[1][1] = aafA[0][0]*aafA[2][2]-aafA[0][2]*aafA[2][0];
    aafAInv[1][2] = aafA[0][2]*aafA[1][0]-aafA[0][0]*aafA[1][2];
    aafAInv[2][0] = aafA[1][0]*aafA[2][1]-aafA[1][1]*aafA[2][0];
    aafAInv[2][1] = aafA[0][1]*aafA[2][0]-aafA[0][0]*aafA[2][1];
    aafAInv[2][2] = aafA[0][0]*aafA[1][1]-aafA[0][1]*aafA[1][0];
    Real fDet = aafA[0][0]*aafAInv[0][0] + aafA[0][1]*aafAInv[1][0] +
        aafA[0][2]*aafAInv[2][0];
    if ( Math<Real>::FAbs(fDet) < ms_fTolerance )
        return false;

    Real fInvDet = ((Real)1.0)/fDet;
    for (int iRow = 0; iRow < 3; iRow++)
    {
        for (int iCol = 0; iCol < 3; iCol++)
            aafAInv[iRow][iCol] *= fInvDet;
    }

    afX[0] = aafAInv[0][0]*afB[0]+aafAInv[0][1]*afB[1]+aafAInv[0][2]*afB[2];
    afX[1] = aafAInv[1][0]*afB[0]+aafAInv[1][1]*afB[1]+aafAInv[1][2]*afB[2];
    afX[2] = aafAInv[2][0]*afB[0]+aafAInv[2][1]*afB[1]+aafAInv[2][2]*afB[2];
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::Inverse (const GMatrix<Real>& rkA,
    GMatrix<Real>& rkInvA)
{
    // computations are performed in-place
    assert( rkA.GetRows() == rkA.GetColumns() );
    int iSize = rkInvA.GetRows();
    rkInvA = rkA;

    int* aiColIndex = new int[iSize];
    assert( aiColIndex );

    int* aiRowIndex = new int[iSize];
    assert( aiRowIndex );

    bool* abPivoted = new bool[iSize];
    assert( abPivoted );
    memset(abPivoted,0,iSize*sizeof(bool));

    int i1, i2, iRow, iCol;
    Real fSave;

    // elimination by full pivoting
    for (int i0 = 0; i0 < iSize; i0++)
    {
        // search matrix (excluding pivoted rows) for maximum absolute entry
        Real fMax = 0.0f;
        for (i1 = 0; i1 < iSize; i1++)
        {
            if ( !abPivoted[i1] )
            {
                for (i2 = 0; i2 < iSize; i2++)
                {
                    if ( !abPivoted[i2] )
                    {
                        Real fAbs = Math<Real>::FAbs(rkInvA[i1][i2]);
                        if ( fAbs > fMax )
                        {
                            fMax = fAbs;
                            iRow = i1;
                            iCol = i2;
                        }
                    }
                }
            }
        }

        if ( fMax == (Real)0.0 )
        {
            // matrix is not invertible
            delete[] aiColIndex;
            delete[] aiRowIndex;
            delete[] abPivoted;
            return false;
        }

        abPivoted[iCol] = true;

        // swap rows so that A[iCol][iCol] contains the pivot entry
        if ( iRow != iCol )
            rkInvA.SwapRows(iRow,iCol);

        // keep track of the permutations of the rows
        aiRowIndex[i0] = iRow;
        aiColIndex[i0] = iCol;

        // scale the row so that the pivot entry is 1
        Real fInv = ((Real)1.0)/rkInvA[iCol][iCol];
        rkInvA[iCol][iCol] = (Real)1.0;
        for (i2 = 0; i2 < iSize; i2++)
            rkInvA[iCol][i2] *= fInv;

        // zero out the pivot column locations in the other rows
        for (i1 = 0; i1 < iSize; i1++)
        {
            if ( i1 != iCol )
            {
                fSave = rkInvA[i1][iCol];
                rkInvA[i1][iCol] = (Real)0.0;
                for (i2 = 0; i2 < iSize; i2++)
                    rkInvA[i1][i2] -= rkInvA[iCol][i2]*fSave;
            }
        }
    }

    // reorder rows so that A[][] stores the inverse of the original matrix
    for (i1 = iSize-1; i1 >= 0; i1--)
    {
        if ( aiRowIndex[i1] != aiColIndex[i1] )
        {
            for (i2 = 0; i2 < iSize; i2++)
            {
                fSave = rkInvA[i2][aiRowIndex[i1]];
                rkInvA[i2][aiRowIndex[i1]] = rkInvA[i2][aiColIndex[i1]];
                rkInvA[i2][aiColIndex[i1]] = fSave;
            }
        }
    }

    delete[] aiColIndex;
    delete[] aiRowIndex;
    delete[] abPivoted;
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::Solve (const GMatrix<Real>& rkA, const Real* afB,
    Real* afX)
{
    // computations are performed in-place
    int iSize = rkA.GetColumns();
    GMatrix<Real> kInvA = rkA;
    memcpy(afX,afB,iSize*sizeof(Real));

    int* aiColIndex = new int[iSize];
    assert( aiColIndex );

    int* aiRowIndex = new int[iSize];
    assert( aiRowIndex );

    bool* abPivoted = new bool[iSize];
    assert( abPivoted );
    memset(abPivoted,0,iSize*sizeof(bool));

    int i1, i2, iRow, iCol;
    Real fSave;

    // elimination by full pivoting
    for (int i0 = 0; i0 < iSize; i0++)
    {
        // search matrix (excluding pivoted rows) for maximum absolute entry
        Real fMax = 0.0f;
        for (i1 = 0; i1 < iSize; i1++)
        {
            if ( !abPivoted[i1] )
            {
                for (i2 = 0; i2 < iSize; i2++)
                {
                    if ( !abPivoted[i2] )
                    {
                        Real fAbs = Math<Real>::FAbs(kInvA[i1][i2]);
                        if ( fAbs > fMax )
                        {
                            fMax = fAbs;
                            iRow = i1;
                            iCol = i2;
                        }
                    }
                }
            }
        }

        if ( fMax == (Real)0.0 )
        {
            // matrix is not invertible
            delete[] aiColIndex;
            delete[] aiRowIndex;
            delete[] abPivoted;
            return false;
        }

        abPivoted[iCol] = true;

        // swap rows so that A[iCol][iCol] contains the pivot entry
        if ( iRow != iCol )
        {
            kInvA.SwapRows(iRow,iCol);

            fSave = afX[iRow];
            afX[iRow] = afX[iCol];
            afX[iCol] = fSave;
        }

        // keep track of the permutations of the rows
        aiRowIndex[i0] = iRow;
        aiColIndex[i0] = iCol;

        // scale the row so that the pivot entry is 1
        Real fInv = ((Real)1.0)/kInvA[iCol][iCol];
        kInvA[iCol][iCol] = (Real)1.0;
        for (i2 = 0; i2 < iSize; i2++)
            kInvA[iCol][i2] *= fInv;
        afX[iCol] *= fInv;

        // zero out the pivot column locations in the other rows
        for (i1 = 0; i1 < iSize; i1++)
        {
            if ( i1 != iCol )
            {
                fSave = kInvA[i1][iCol];
                kInvA[i1][iCol] = (Real)0.0;
                for (i2 = 0; i2 < iSize; i2++)
                    kInvA[i1][i2] -= kInvA[iCol][i2]*fSave;
                afX[i1] -= afX[iCol]*fSave;
            }
        }
    }

    // reorder rows so that A[][] stores the inverse of the original matrix
    for (i1 = iSize-1; i1 >= 0; i1--)
    {
        if ( aiRowIndex[i1] != aiColIndex[i1] )
        {
            for (i2 = 0; i2 < iSize; i2++)
            {
                fSave = kInvA[i2][aiRowIndex[i1]];
                kInvA[i2][aiRowIndex[i1]] = kInvA[i2][aiColIndex[i1]];
                kInvA[i2][aiColIndex[i1]] = fSave;
            }
        }
    }

    delete[] aiColIndex;
    delete[] aiRowIndex;
    delete[] abPivoted;
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SolveTri (int iSize, Real* afA, Real* afB,
    Real* afC, Real* afR, Real* afU)
{
    if ( afB[0] == (Real)0.0 )
        return false;

    Real* afD = new Real[iSize-1];
    assert( afD );

    Real fE = afB[0];
    Real fInvE = ((Real)1.0)/fE;
    afU[0] = afR[0]*fInvE;

    int i0, i1;
    for (i0 = 0, i1 = 1; i1 < iSize; i0++, i1++)
    {
        afD[i0] = afC[i0]*fInvE;
        fE = afB[i1] - afA[i0]*afD[i0];
        if ( fE == (Real)0.0 )
        {
            delete[] afD;
            return false;
        }
        fInvE = ((Real)1.0)/fE;
        afU[i1] = (afR[i1] - afA[i0]*afU[i0])*fInvE;
    }

    for (i0 = iSize-1, i1 = iSize-2; i1 >= 0; i0--, i1--)
        afU[i1] -= afD[i1]*afU[i0];

    delete[] afD;
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SolveConstTri (int iSize, Real fA, Real fB, Real fC,
    Real* afR, Real* afU)
{
    if ( fB == (Real)0.0 )
        return false;

    Real* afD = new Real[iSize-1];
    assert( afD );

    Real fE = fB;
    Real fInvE = ((Real)1.0)/fE;
    afU[0] = afR[0]*fInvE;

    int i0, i1;
    for (i0 = 0, i1 = 1; i1 < iSize; i0++, i1++)
    {
        afD[i0] = fC*fInvE;
        fE = fB - fA*afD[i0];
        if ( fE == (Real)0.0 )
        {
            delete[] afD;
            return false;
        }
        fInvE = ((Real)1.0)/fE;
        afU[i1] = (afR[i1] - fA*afU[i0])*fInvE;
    }
    for (i0 = iSize-1, i1 = iSize-2; i1 >= 0; i0--, i1--)
        afU[i1] -= afD[i1]*afU[i0];

    delete[] afD;
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SolveSymmetric (const GMatrix<Real>& rkA,
    const Real* afB, Real* afX)
{
    // A = L D L^t decomposition with diagonal terms of L equal to 1.  The
    // algorithm stores D terms in A[i][i] and off-diagonal L terms in
    // A[i][j] for i > j.  (G. Golub and C. Van Loan, Matrix Computations)

    // computations are performed in-place
    int iSize = rkA.GetColumns();
    GMatrix<Real> kLMat = rkA;
    memcpy(afX,afB,iSize*sizeof(Real));

    int i0, i1;
    Real* afV = new Real[iSize];
    assert( afV );

    for (i1 = 0; i1 < iSize; i1++)
    {
        for (i0 = 0; i0 < i1; i0++)
            afV[i0] = kLMat[i1][i0]*kLMat[i0][i0];

        afV[i1] = kLMat[i1][i1];
        for (i0 = 0; i0 < i1; i0++)
            afV[i1] -= kLMat[i1][i0]*afV[i0];

        kLMat[i1][i1] = afV[i1];
        if ( Math<Real>::FAbs(afV[i1]) <= Math<Real>::EPSILON )
        {
            delete[] afV;
            return false;
        }

        Real fInv = ((Real)1.0)/afV[i1];
        for (i0 = i1+1; i0 < iSize; i0++)
        {
            for (int i2 = 0; i2 < i1; i2++)
                kLMat[i0][i1] -= kLMat[i0][i2]*afV[i2];
            kLMat[i0][i1] *= fInv;
        }
    }
    delete[] afV;

    // Solve Ax = B.

    // Forward substitution:  Let z = DL^t x, then Lz = B.  Algorithm
    // stores z terms in B vector.
    for (i0 = 0; i0 < iSize; i0++)
    {
        for (i1 = 0; i1 < i0; i1++)
            afX[i0] -= kLMat[i0][i1]*afX[i1];

    }

    // Diagonal division:  Let y = L^t x, then Dy = z.  Algorithm stores
    // y terms in B vector.
    for (i0 = 0; i0 < iSize; i0++)
    {
        if ( Math<Real>::FAbs(kLMat[i0][i0]) <= Math<Real>::EPSILON )
            return false;
        afX[i0] /= kLMat[i0][i0];
    }

    // Back substitution:  Solve L^t x = y.
    for (i0 = iSize-2; i0 >= 0; i0--)
    {
        for (i1 = i0+1; i1 < iSize; i1++)
            afX[i0] -= kLMat[i1][i0]*afX[i1];
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SymmetricInverse (const GMatrix<Real>& rkA,
    GMatrix<Real>& rkInvA)
{
    // Same algorithm as SolveSymmetric, but applied simultaneously to
    // columns of identity matrix.
    int iSize = rkA.GetRows();
    GMatrix<Real> kTmp = rkA;

    Real* afV = new Real[iSize];
    assert( afV );

    int i0, i1;
    for (i0 = 0; i0 < iSize; i0++)
    {
        for (i1 = 0; i1 < iSize; i1++)
            rkInvA[i0][i1] = ( i0 != i1 ? (Real)0.0 : (Real)1.0 );
    }

    for (i1 = 0; i1 < iSize; i1++)
    {
        for (i0 = 0; i0 < i1; i0++)
            afV[i0] = kTmp[i1][i0]*kTmp[i0][i0];

        afV[i1] = kTmp[i1][i1];
        for (i0 = 0; i0 < i1; i0++)
            afV[i1] -= kTmp[i1][i0]*afV[i0];

        kTmp[i1][i1] = afV[i1];
        for (i0 = i1+1; i0 < iSize; i0++)
        {
            for (int i2 = 0; i2 < i1; i2++)
                kTmp[i0][i1] -= kTmp[i0][i2]*afV[i2];
            kTmp[i0][i1] /= afV[i1];
        }
    }
    delete[] afV;

    for (int iCol = 0; iCol < iSize; iCol++)
    {
        // forward substitution
        for (i0 = 0; i0 < iSize; i0++)
        {
            for (i1 = 0; i1 < i0; i1++)
                rkInvA[i0][iCol] -= kTmp[i0][i1]*rkInvA[i1][iCol];
        }

        // diagonal division
        for (i0 = 0; i0 < iSize; i0++)
        {
            if ( Math<Real>::FAbs(kTmp[i0][i0]) <= Math<Real>::EPSILON )
                return false;
            rkInvA[i0][iCol] /= kTmp[i0][i0];
        }

        // back substitution
        for (i0 = iSize-2; i0 >= 0; i0--)
        {
            for (i1 = i0+1; i1 < iSize; i1++)
                rkInvA[i0][iCol] -= kTmp[i1][i0]*rkInvA[i1][iCol];
        }
    }

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// conjugate gradient methods
//----------------------------------------------------------------------------
template <class Real>
Real LinearSystem<Real>::Dot (int iSize, const Real* afU, const Real* afV)
{
    Real fDot = (Real)0.0;
    for (int i = 0; i < iSize; i++)
        fDot += afU[i]*afV[i];
    return fDot;
}
//----------------------------------------------------------------------------
template <class Real>
void LinearSystem<Real>::Multiply (const GMatrix<Real>& rkA, const Real* afX,
    Real* afProd)
{
    int iSize = rkA.GetRows();
    memset(afProd,0,iSize*sizeof(Real));
    for (int iRow = 0; iRow < iSize; iRow++)
    {
        for (int iCol = 0; iCol < iSize; iCol++)
            afProd[iRow] += rkA[iRow][iCol]*afX[iCol];
    }
}
//----------------------------------------------------------------------------
template <class Real>
void LinearSystem<Real>::Multiply (int iSize, const SparseMatrix& rkA,
    const Real* afX, Real* afProd)
{
    memset(afProd,0,iSize*sizeof(Real));
    typename SparseMatrix::const_iterator pkIter = rkA.begin();
    for (/**/; pkIter != rkA.end(); pkIter++)
    {
        int i = pkIter->first.first;
        int j = pkIter->first.second;
        Real fValue = pkIter->second;
        afProd[i] += fValue*afX[j];
        if ( i != j )
            afProd[j] += fValue*afX[i];
    }
}
//----------------------------------------------------------------------------
template <class Real>
void LinearSystem<Real>::UpdateX (int iSize, Real* afX, Real fAlpha,
    const Real* afP)
{
    for (int i = 0; i < iSize; i++)
        afX[i] += fAlpha*afP[i];
}
//----------------------------------------------------------------------------
template <class Real>
void LinearSystem<Real>::UpdateR (int iSize, Real* afR, Real fAlpha,
    const Real* afW)
{
    for (int i = 0; i < iSize; i++)
        afR[i] -= fAlpha*afW[i];
}
//----------------------------------------------------------------------------
template <class Real>
void LinearSystem<Real>::UpdateP (int iSize, Real* afP, Real fBeta,
    const Real* afR)
{
    for (int i = 0; i < iSize; i++)
        afP[i] = afR[i] + fBeta*afP[i];
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SolveSymmetricCG (const GMatrix<Real>& rkA,
    const Real* afB, Real* afX)
{
    // based on the algorithm in "Matrix Computations" by Golum and Van Loan
    assert( rkA.GetRows() == rkA.GetColumns() );
    int iSize = rkA.GetRows();
    Real* afR = new Real[iSize];
    Real* afP = new Real[iSize];
    Real* afW = new Real[iSize];

    // first iteration
    memset(afX,0,iSize*sizeof(Real));
    memcpy(afR,afB,iSize*sizeof(Real));
    Real fRho0 = Dot(iSize,afR,afR);
    memcpy(afP,afR,iSize*sizeof(Real));
    Multiply(rkA,afP,afW);
    Real fAlpha = fRho0/Dot(iSize,afP,afW);
    UpdateX(iSize,afX,fAlpha,afP);
    UpdateR(iSize,afR,fAlpha,afW);
    Real fRho1 = Dot(iSize,afR,afR);

    // remaining iterations
    const int iMax = 1024;
    int i;
    for (i = 1; i < iMax; i++)
    {
        Real fRoot0 = Math<Real>::Sqrt(fRho1);
        Real fNorm = Dot(iSize,afB,afB);
        Real fRoot1 = Math<Real>::Sqrt(fNorm);
        if ( fRoot0 <= ms_fTolerance*fRoot1 )
            break;

        Real fBeta = fRho1/fRho0;
        UpdateP(iSize,afP,fBeta,afR);
        Multiply(rkA,afP,afW);
        fAlpha = fRho1/Dot(iSize,afP,afW);
        UpdateX(iSize,afX,fAlpha,afP);
        UpdateR(iSize,afR,fAlpha,afW);
        fRho0 = fRho1;
        fRho1 = Dot(iSize,afR,afR);
    }

    delete[] afW;
    delete[] afP;
    delete[] afR;

    return i < iMax;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SolveSymmetricCG (int iSize,
    const SparseMatrix& rkA, const Real* afB, Real* afX)
{
    // based on the algorithm in "Matrix Computations" by Golum and Van Loan
    Real* afR = new Real[iSize];
    Real* afP = new Real[iSize];
    Real* afW = new Real[iSize];

    // first iteration
    memset(afX,0,iSize*sizeof(Real));
    memcpy(afR,afB,iSize*sizeof(Real));
    Real fRho0 = Dot(iSize,afR,afR);
    memcpy(afP,afR,iSize*sizeof(Real));
    Multiply(iSize,rkA,afP,afW);
    Real fAlpha = fRho0/Dot(iSize,afP,afW);
    UpdateX(iSize,afX,fAlpha,afP);
    UpdateR(iSize,afR,fAlpha,afW);
    Real fRho1 = Dot(iSize,afR,afR);

    // remaining iterations
    const int iMax = 1024;
    int i;
    for (i = 1; i < iMax; i++)
    {
        Real fRoot0 = Math<Real>::Sqrt(fRho1);
        Real fNorm = Dot(iSize,afB,afB);
        Real fRoot1 = Math<Real>::Sqrt(fNorm);
        if ( fRoot0 <= ms_fTolerance*fRoot1 )
            break;

        Real fBeta = fRho1/fRho0;
        UpdateP(iSize,afP,fBeta,afR);
        Multiply(iSize,rkA,afP,afW);
        fAlpha = fRho1/Dot(iSize,afP,afW);
        UpdateX(iSize,afX,fAlpha,afP);
        UpdateR(iSize,afR,fAlpha,afW);
        fRho0 = fRho1;
        fRho1 = Dot(iSize,afR,afR);
    }

    delete[] afW;
    delete[] afP;
    delete[] afR;

    return i < iMax;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// banded matrices
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::ForwardEliminate (int iReduceRow,
    BandedMatrix<Real>& rkA, Real* afB)
{
    // the pivot must be nonzero in order to proceed
    Real fDiag = rkA(iReduceRow,iReduceRow);
    if ( fDiag == (Real)0.0 )
        return false;

    Real fInvDiag = ((Real)1.0)/fDiag;
    rkA(iReduceRow,iReduceRow) = (Real)1.0;

    // multiply row to be consistent with diagonal term of 1
    int iColMin = iReduceRow + 1;
    int iColMax = iColMin + rkA.GetUBands();
    if ( iColMax > rkA.GetSize() )
        iColMax = rkA.GetSize();

    int iCol;
    for (iCol = iColMin; iCol < iColMax; iCol++)
        rkA(iReduceRow,iCol) *= fInvDiag;

    afB[iReduceRow] *= fInvDiag;

    // reduce remaining rows
    int iRowMin = iReduceRow + 1;
    int iRowMax = iRowMin + rkA.GetLBands();
    if ( iRowMax > rkA.GetSize() )
        iRowMax = rkA.GetSize();

    for (int iRow = iRowMin; iRow < iRowMax; iRow++)
    {
        Real fMult = rkA(iRow,iReduceRow);
        rkA(iRow,iReduceRow) = (Real)0.0;
        for (iCol = iColMin; iCol < iColMax; iCol++)
            rkA(iRow,iCol) -= fMult*rkA(iReduceRow,iCol);
        afB[iRow] -= fMult*afB[iReduceRow];
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::SolveBanded (const BandedMatrix<Real>& rkA,
    const Real* afB, Real* afX)
{
    BandedMatrix<Real> kTmp = rkA;
    int iSize = rkA.GetSize();
    memcpy(afX,afB,iSize*sizeof(Real));

    // forward elimination
    int iRow;
    for (iRow = 0; iRow < iSize; iRow++)
    {
        if ( !ForwardEliminate(iRow,kTmp,afX) )
            return false;
    }

    // backward substitution
    for (iRow = iSize-2; iRow >= 0; iRow--)
    {
        int iColMin = iRow + 1;
        int iColMax = iColMin + kTmp.GetUBands();
        if ( iColMax > iSize )
            iColMax = iSize;
        for (int iCol = iColMin; iCol < iColMax; iCol++)
            afX[iRow] -= kTmp(iRow,iCol)*afX[iCol];
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::ForwardEliminate (int iReduceRow,
    BandedMatrix<Real>& rkA, GMatrix<Real>& rkB)
{
    // the pivot must be nonzero in order to proceed
    Real fDiag = rkA(iReduceRow,iReduceRow);
    if ( fDiag == (Real)0.0 )
        return false;

    Real fInvDiag = ((Real)1.0)/fDiag;
    rkA(iReduceRow,iReduceRow) = (Real)1.0;

    // multiply row to be consistent with diagonal term of 1
    int iColMin = iReduceRow + 1;
    int iColMax = iColMin + rkA.GetUBands();
    if ( iColMax > rkA.GetSize() )
        iColMax = rkA.GetSize();

    int iCol;
    for (iCol = iColMin; iCol < iColMax; iCol++)
        rkA(iReduceRow,iCol) *= fInvDiag;
    for (iCol = 0; iCol <= iReduceRow; iCol++)
        rkB(iReduceRow,iCol) *= fInvDiag;

    // reduce remaining rows
    int iRowMin = iReduceRow + 1;
    int iRowMax = iRowMin + rkA.GetLBands();
    if ( iRowMax > rkA.GetSize() )
        iRowMax = rkA.GetSize();

    for (int iRow = iRowMin; iRow < iRowMax; iRow++)
    {
        Real fMult = rkA(iRow,iReduceRow);
        rkA(iRow,iReduceRow) = (Real)0.0;
        for (iCol = iColMin; iCol < iColMax; iCol++)
            rkA(iRow,iCol) -= fMult*rkA(iReduceRow,iCol);
        for (iCol = 0; iCol <= iReduceRow; iCol++)
            rkB(iRow,iCol) -= fMult*rkB(iReduceRow,iCol);
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void LinearSystem<Real>::BackwardEliminate (int iReduceRow,
    BandedMatrix<Real>& rkA, GMatrix<Real>& rkB)
{
    int iRowMax = iReduceRow - 1;
    int iRowMin = iReduceRow - rkA.GetUBands();
    if ( iRowMin < 0 )
        iRowMin = 0;

    for (int iRow = iRowMax; iRow >= iRowMin; iRow--)
    {
        Real fMult = rkA(iRow,iReduceRow);
        rkA(iRow,iReduceRow) = (Real)0.0;
        for (int iCol = 0; iCol < rkB.GetColumns(); iCol++)
            rkB(iRow,iCol) -= fMult*rkB(iReduceRow,iCol);
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool LinearSystem<Real>::Invert (const BandedMatrix<Real>& rkA,
    GMatrix<Real>& rkInvA)
{
    int iSize = rkA.GetSize();
    BandedMatrix<Real> kTmp = rkA;
    int iRow;
    for (iRow = 0; iRow < iSize; iRow++)
    {
        for (int iCol = 0; iCol < iSize; iCol++)
        {
            if ( iRow != iCol )
                rkInvA(iRow,iCol) = (Real)0.0;
            else
                rkInvA(iRow,iRow) = (Real)1.0;
        }
    }

    // forward elimination
    for (iRow = 0; iRow < iSize; iRow++)
    {
        if ( !ForwardEliminate(iRow,kTmp,rkInvA) )
            return false;
    }

    // backward elimination
    for (iRow = iSize-1; iRow >= 1; iRow--)
        BackwardEliminate(iRow,kTmp,rkInvA);

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM LinearSystem<float>;
float LinearSystemf::ms_fTolerance = 1e-06f;

template class WML_ITEM LinearSystem<double>;
double LinearSystemd::ms_fTolerance = 1e-06;
}
//----------------------------------------------------------------------------
