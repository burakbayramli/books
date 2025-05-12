// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindA (Real fC0, Real fC1, Real fC2, Real fC3)
{
    if ( Math<Real>::FAbs(fC3) <= m_fEpsilon )
    {
        // polynomial is quadratic
        return FindA(fC0,fC1,fC2);
    }

    // make polynomial monic, x^3+c2*x^2+c1*x+c0
    Real fInvC3 = ((Real)1.0)/fC3;
    fC0 *= fInvC3;
    fC1 *= fInvC3;
    fC2 *= fInvC3;

    // convert to y^3+a*y+b = 0 by x = y-c2/3
    Real fOffset = THIRD*fC2;
    Real fA = fC1 - fC2*fOffset;
    Real fB = fC0+fC2*(((Real)2.0)*fC2*fC2-((Real)9.0)*fC1)*TWENTYSEVENTH;
    Real fHalfB = ((Real)0.5)*fB;

    Real fDiscr = fHalfB*fHalfB + fA*fA*fA*TWENTYSEVENTH;
    if ( Math<Real>::FAbs(fDiscr) <= m_fEpsilon )
        fDiscr = (Real)0.0;

    if ( fDiscr > (Real)0.0 )  // 1 real, 2 complex roots
    {
        fDiscr = Math<Real>::Sqrt(fDiscr);
        Real fTemp = -fHalfB + fDiscr;
        if ( fTemp >= (Real)0.0 )
            m_afRoot[0] = Math<Real>::Pow(fTemp,THIRD);
        else
            m_afRoot[0] = -Math<Real>::Pow(-fTemp,THIRD);
        fTemp = -fHalfB - fDiscr;
        if ( fTemp >= (Real)0.0 )
            m_afRoot[0] += Math<Real>::Pow(fTemp,THIRD);
        else
            m_afRoot[0] -= Math<Real>::Pow(-fTemp,THIRD);
        m_afRoot[0] -= fOffset;
        m_iCount = 1;
    }
    else if ( fDiscr < (Real)0.0 ) 
    {
        Real fDist = Math<Real>::Sqrt(-THIRD*fA);
        Real fAngle = THIRD*Math<Real>::ATan2(Math<Real>::Sqrt(-fDiscr),
            -fHalfB);
        Real fCos = Math<Real>::Cos(fAngle);
        Real fSin = Math<Real>::Sin(fAngle);
        m_afRoot[0] = ((Real)2.0)*fDist*fCos-fOffset;
        m_afRoot[1] = -fDist*(fCos+SQRT3*fSin)-fOffset;
        m_afRoot[2] = -fDist*(fCos-SQRT3*fSin)-fOffset;
        m_iCount = 3;
    }
    else 
    {
        Real fTemp;
        if ( fHalfB >= (Real)0.0 )
            fTemp = -Math<Real>::Pow(fHalfB,THIRD);
        else
            fTemp = Math<Real>::Pow(-fHalfB,THIRD);
        m_afRoot[0] = ((Real)2.0)*fTemp-fOffset;
        m_afRoot[1] = -fTemp-fOffset;
        m_afRoot[2] = m_afRoot[1];
        m_iCount = 3;
    }

    return true;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindE (Real fC0, Real fC1, Real fC2, Real fC3,
    bool bDoBalancing)
{
    if ( Math<Real>::FAbs(fC3) <= m_fEpsilon )
    {
        // polynomial is quadratic
        return FindA(fC0,fC1,fC2);
    }

    // make polynomial monic, x^3+c2*x^2+c1*x+c0
    Real fInvC3 = ((Real)1.0)/fC3;
    fC0 *= fInvC3;
    fC1 *= fInvC3;
    fC2 *= fInvC3;

    // construct the 3-by-3 companion matrix
    GMatrix<Real> kMat(3,3);  // initialized to zero
    kMat[1][0] = (Real)1.0;
    kMat[2][1] = (Real)1.0;
    kMat[0][2] = -fC0;
    kMat[1][2] = -fC1;
    kMat[2][2] = -fC2;

    if ( bDoBalancing )
        BalanceCompanion3(kMat);

    return QRIteration3(kMat);
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetBound (Real fC0, Real fC1, Real fC2, Real fC3)
{
    if ( Math<Real>::FAbs(fC3) <= m_fEpsilon )
    {
        // polynomial is quadratic
        return GetBound(fC0,fC1,fC2);
    }

    Real fInvC3 = ((Real)1.0)/fC3;
    Real fMax = Math<Real>::FAbs(fC0)*fInvC3;

    Real fTmp = Math<Real>::FAbs(fC1)*fInvC3;
    if ( fTmp > fMax )
        fMax = fTmp;

    fTmp = Math<Real>::FAbs(fC2)*fInvC3;
    if ( fTmp > fMax )
        fMax = fTmp;

    return (Real)1.0 + fMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::SpecialCubic (Real fA, Real fB, Real fC)
{
    // Solve A*r^3 + B*r = C where A > 0 and B > 0.
    //
    // Let r = D*sinh(u) where D = sqrt(4*B/(3*A)).  Then
    // sinh(3*u) = 4*[sinh(u)]^3+3*sinh(u) = E where E = 4*C/(A*D^3).
    // sinh(3*u) = E has solution u = (1/3)*log(E+sqrt(E^2+1)).  This
    // leads to sinh(u) = ((E+sqrt(E^2+1))^{1/3}-(E+sqrt(E^2+1))^{-1/3})/2.
    // Therefore,  r = D*((E+sqrt(E^2+1))^{1/3}-(E+sqrt(E^2+1))^{-1/3})/2.

    Real fD = Math<Real>::Sqrt(((Real)4.0)*THIRD*fB/fA);
    Real fE = ((Real)4.0)*fC/(fA*fD*fD*fD);
    Real fF = Math<Real>::Pow(fE+Math<Real>::Sqrt(fE*fE+(Real)1.0),THIRD);
    Real fRoot = ((Real)0.5)*fD*(fF-((Real)1.0)/fF);
    return fRoot;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetRowNorm (int iRow, GMatrix<Real>& rkMat)
{
    Real fNorm = Math<Real>::FAbs(rkMat[iRow][0]);
    for (int iCol = 1; iCol < rkMat.GetColumns(); iCol++)
    {
        Real fAbs = Math<Real>::FAbs(rkMat[iRow][iCol]);
        if ( fAbs > fNorm )
            fNorm = fAbs;
    }
    return fNorm;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetColNorm (int iCol, GMatrix<Real>& rkMat)
{
    Real fNorm = Math<Real>::FAbs(rkMat[0][iCol]);
    for (int iRow = 1; iRow < rkMat.GetRows(); iRow++)
    {
        Real fAbs = Math<Real>::FAbs(rkMat[iRow][iCol]);
        if ( fAbs > fNorm )
            fNorm = fAbs;
    }
    return fNorm;
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::ScaleRow (int iRow, Real fScale,
    GMatrix<Real>& rkMat)
{
    for (int iCol = 0; iCol < rkMat.GetColumns(); iCol++)
        rkMat[iRow][iCol] *= fScale;
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::ScaleCol (int iCol, Real fScale,
    GMatrix<Real>& rkMat)
{
    for (int iRow = 0; iRow < rkMat.GetRows(); iRow++)
        rkMat[iRow][iCol] *= fScale;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::IsBalanced3 (GMatrix<Real>& rkMat)
{
    const Real fTolerance = (Real)0.001;
    for (int i = 0; i < 3; i++)
    {
        Real fRowNorm = GetRowNorm(i,rkMat);
        Real fColNorm = GetColNorm(i,rkMat);
        Real fTest = Math<Real>::FAbs((Real)1.0 - fColNorm/fRowNorm);
        if ( fTest > fTolerance )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::Balance3 (GMatrix<Real>& rkMat)
{
    const int iMax = 16;
    int i;
    for (i = 0; i < iMax; i++)
    {
        for (int j = 0; j < 3; j++)
        {
            Real fRowNorm = GetRowNorm(j,rkMat);
            Real fColNorm = GetColNorm(j,rkMat);
            Real fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
            Real fInvScale = ((Real)1.0)/fScale;
            ScaleRow(j,fScale,rkMat);
            ScaleCol(j,fInvScale,rkMat);
        }

        if ( IsBalanced3(rkMat) )
            break;
    }
    assert( i < iMax );
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::IsBalancedCompanion3 (Real fA10, Real fA21,
    Real fA02, Real fA12, Real fA22)
{
    const Real fTolerance = (Real)0.001;

    // row/col 0
    Real fRowNorm = fA02;
    Real fColNorm = fA10;
    Real fTest = Math<Real>::FAbs((Real)1.0 - fColNorm/fRowNorm);
    if ( fTest > fTolerance )
        return false;

    // row/col 1
    fRowNorm = ( fA10 >= fA12 ? fA10 : fA12 );
    fColNorm = fA21;
    fTest = Math<Real>::FAbs((Real)1.0 - fColNorm/fRowNorm);
    if ( fTest > fTolerance )
        return false;

    // row/col 2
    fRowNorm = ( fA21 >= fA22 ? fA21 : fA22 );
    fColNorm = ( fA02 >= fA12 ? fA02 : fA12 );
    if ( fA22 > fColNorm )
        fColNorm = fA22;
    fTest = Math<Real>::FAbs((Real)1.0 - fColNorm/fRowNorm);
    return fTest <= fTolerance;
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::BalanceCompanion3 (GMatrix<Real>& rkMat)
{
    Real fA10 = Math<Real>::FAbs(rkMat[1][0]);
    Real fA21 = Math<Real>::FAbs(rkMat[2][1]);
    Real fA02 = Math<Real>::FAbs(rkMat[0][2]);
    Real fA12 = Math<Real>::FAbs(rkMat[1][2]);
    Real fA22 = Math<Real>::FAbs(rkMat[2][2]);
    Real fRowNorm, fColNorm, fScale, fInvScale;

    const int iMax = 16;
    int i;
    for (i = 0; i < iMax; i++)
    {
        // balance row/col 0
        fRowNorm = fA02;
        fColNorm = fA10;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fA02 *= fScale;
        fA10 = fA02;

        // balance row/col 1
        fRowNorm = ( fA10 >= fA12 ? fA10 : fA12 );
        fColNorm = fA21;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fInvScale = ((Real)1.0)/fScale;
        fA10 *= fScale;
        fA12 *= fScale;
        fA21 *= fInvScale;

        // balance row/col 2
        fRowNorm = ( fA21 >= fA22 ? fA21 : fA22 );
        fColNorm = ( fA02 >= fA12 ? fA02 : fA12 );
        if ( fA22 > fColNorm )
            fColNorm = fA22;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fInvScale = ((Real)1.0)/fScale;
        fA21 *= fScale;
        fA02 *= fInvScale;
        fA12 *= fInvScale;

        if ( IsBalancedCompanion3(fA10,fA21,fA02,fA12,fA22) )
            break;
    }
    assert( i < iMax );

    rkMat[1][0] = ( rkMat[1][0] >= (Real)0.0 ? fA10 : -fA10);
    rkMat[2][1] = ( rkMat[2][1] >= (Real)0.0 ? fA21 : -fA21);
    rkMat[0][2] = ( rkMat[0][2] >= (Real)0.0 ? fA02 : -fA02);
    rkMat[1][2] = ( rkMat[1][2] >= (Real)0.0 ? fA12 : -fA12);
    rkMat[2][2] = ( rkMat[2][2] >= (Real)0.0 ? fA22 : -fA22);
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::QRIteration3 (GMatrix<Real>& rkMat)
{
    GVector<Real> kW(3);
    Real fRHS, fTrace, fDet;
    for (int i = 0; i < m_iMaxIterations; i++)
    {
        fRHS = m_fEpsilon*(Math<Real>::FAbs(rkMat[0][0]) +
            Math<Real>::FAbs(rkMat[1][1]));

        if ( Math<Real>::FAbs(rkMat[1][0]) <= fRHS )
        {
            // mat[0][0] is a root, solve the quadratic for the submatrix
            fTrace = rkMat[1][1] + rkMat[2][2];
            fDet = rkMat[1][1]*rkMat[2][2] - rkMat[1][2]*rkMat[2][1];
            FindA(fDet,-fTrace,(Real)1.0);
            m_afRoot[m_iCount++] = rkMat[0][0];
            return true;
        }

        fRHS = m_fEpsilon*(Math<Real>::FAbs(rkMat[1][1]) +
            Math<Real>::FAbs(rkMat[2][2]));

        if ( Math<Real>::FAbs(rkMat[2][1]) <= fRHS )
        {
            // mat[2][2] is a root, solve the quadratic for the submatrix
            fTrace = rkMat[0][0] + rkMat[1][1];
            fDet = rkMat[0][0]*rkMat[1][1] - rkMat[0][1]*rkMat[1][0];
            FindA(fDet,-fTrace,(Real)1.0);
            m_afRoot[m_iCount++] = rkMat[2][2];
            return true;
        }

        FrancisQRStep(rkMat,kW);
    }

    // TO DO: In theory, cubic polynomials always have one real-valued root,
    // but if the maximum iterations were exceeded, what to do?  Some
    // experiments show that when the polynomial nearly has a double root,
    // the convergence of the algorithm is slow.  Maybe a random perturbation
    // to "kick" the system a bit might work?
    //
    // If you want to trap exceeding the maximum iterations, uncomment the
    // 'assert' line of code.
    //
    // assert( false );

    // For now, zero out the smallest subdiagonal entry to decouple the
    // matrix.
    if ( Math<Real>::FAbs(rkMat[1][0]) <= Math<Real>::FAbs(rkMat[2][1]) )
    {
        // mat[0][0] is a root, solve the quadratic for the submatrix
        fTrace = rkMat[1][1] + rkMat[2][2];
        fDet = rkMat[1][1]*rkMat[2][2] - rkMat[1][2]*rkMat[2][1];
        FindA(fDet,-fTrace,(Real)1.0);
        m_afRoot[m_iCount++] = rkMat[0][0];
    }
    else
    {
        // mat[2][2] is a root, solve the quadratic for the submatrix
        fTrace = rkMat[0][0] + rkMat[1][1];
        fDet = rkMat[0][0]*rkMat[1][1] - rkMat[0][1]*rkMat[1][0];
        FindA(fDet,-fTrace,(Real)1.0);
        m_afRoot[m_iCount++] = rkMat[2][2];
    }

    return true;
}
//----------------------------------------------------------------------------
