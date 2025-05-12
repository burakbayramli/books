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
bool PolynomialRoots<Real>::FindA (Real fC0, Real fC1, Real fC2, Real fC3,
    Real fC4)
{
    if ( Math<Real>::FAbs(fC3) <= m_fEpsilon )
    {
        // polynomial is cubic
        return FindA(fC0,fC1,fC2,fC3);
    }

    // make polynomial monic, x^4+c3*x^3+c2*x^2+c1*x+c0
    Real fInvC4 = ((Real)1.0)/fC4;
    fC0 *= fInvC4;
    fC1 *= fInvC4;
    fC2 *= fInvC4;
    fC3 *= fInvC4;

    // reduction to resolvent cubic polynomial y^3+r2*y^2+r1*y+r0 = 0
    Real fR0 = -fC3*fC3*fC0 + ((Real)4.0)*fC2*fC0 - fC1*fC1;
    Real fR1 = fC3*fC1 - ((Real)4.0)*fC0;
    Real fR2 = -fC2;
    FindA(fR0,fR1,fR2,(Real)1.0);  // always produces at least one root
    Real fY = m_afRoot[0];

    m_iCount = 0;
    Real fDiscr = ((Real)0.25)*fC3*fC3 - fC2 + fY;
    if ( Math<Real>::FAbs(fDiscr) <= m_fEpsilon )
        fDiscr = (Real)0.0;

    if ( fDiscr > (Real)0.0 ) 
    {
        Real fR = Math<Real>::Sqrt(fDiscr);
        Real fT1 = ((Real)0.75)*fC3*fC3 - fR*fR - ((Real)2.0)*fC2;
        Real fT2 = (((Real)4.0)*fC3*fC2 - ((Real)8.0)*fC1 - fC3*fC3*fC3) /
            (((Real)4.0)*fR);

        Real fTplus = fT1+fT2;
        Real fTminus = fT1-fT2;
        if ( Math<Real>::FAbs(fTplus) <= m_fEpsilon ) 
            fTplus = (Real)0.0;
        if ( Math<Real>::FAbs(fTminus) <= m_fEpsilon ) 
            fTminus = (Real)0.0;

        if ( fTplus >= (Real)0.0 )
        {
            Real fD = Math<Real>::Sqrt(fTplus);
            m_afRoot[0] = -((Real)0.25)*fC3+((Real)0.5)*(fR+fD);
            m_afRoot[1] = -((Real)0.25)*fC3+((Real)0.5)*(fR-fD);
            m_iCount += 2;
        }
        if ( fTminus >= (Real)0.0 )
        {
            Real fE = Math<Real>::Sqrt(fTminus);
            m_afRoot[m_iCount++] = -((Real)0.25)*fC3+((Real)0.5)*(fE-fR);
            m_afRoot[m_iCount++] = -((Real)0.25)*fC3-((Real)0.5)*(fE+fR);
        }
    }
    else if ( fDiscr < (Real)0.0 )
    {
        m_iCount = 0;
    }
    else
    {
        Real fT2 = fY*fY-((Real)4.0)*fC0;
        if ( fT2 >= -m_fEpsilon ) 
        {
            if ( fT2 < (Real)0.0 ) // round to zero
                fT2 = (Real)0.0;
            fT2 = ((Real)2.0)*Math<Real>::Sqrt(fT2);
            Real fT1 = ((Real)0.75)*fC3*fC3 - ((Real)2.0)*fC2;
            if ( fT1+fT2 >= m_fEpsilon ) 
            {
                Real fD = Math<Real>::Sqrt(fT1+fT2);
                m_afRoot[0] = -((Real)0.25)*fC3+((Real)0.5)*fD;
                m_afRoot[1] = -((Real)0.25)*fC3-((Real)0.5)*fD;
                m_iCount += 2;
            }
            if ( fT1-fT2 >= m_fEpsilon ) 
            {
                Real fE = Math<Real>::Sqrt(fT1-fT2);
                m_afRoot[m_iCount++] = -((Real)0.25)*fC3+((Real)0.5)*fE;
                m_afRoot[m_iCount++] = -((Real)0.25)*fC3-((Real)0.5)*fE;
            }
        }
    }

    return m_iCount > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindE (Real fC0, Real fC1, Real fC2, Real fC3,
    Real fC4, bool bDoBalancing)
{
    if ( Math<Real>::FAbs(fC4) <= m_fEpsilon )
    {
        // polynomial is cubic
        return FindA(fC0,fC1,fC2,fC3);
    }

    // make polynomial monic, x^4+c3*x^3+c2*x^2+c1*x+c0
    Real fInvC4 = ((Real)1.0)/fC4;
    fC0 *= fInvC4;
    fC1 *= fInvC4;
    fC2 *= fInvC4;
    fC3 *= fInvC4;

    // construct the 4-by-4 companion matrix
    GMatrix<Real> kMat(4,4);  // initialized to zero
    kMat[1][0] = (Real)1.0;
    kMat[2][1] = (Real)1.0;
    kMat[3][2] = (Real)1.0;
    kMat[0][3] = -fC0;
    kMat[1][3] = -fC1;
    kMat[2][3] = -fC2;
    kMat[3][3] = -fC3;

    if ( bDoBalancing )
        BalanceCompanion4(kMat);

    return QRIteration4(kMat);
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetBound (Real fC0, Real fC1, Real fC2, Real fC3,
    Real fC4)
{
    if ( Math<Real>::FAbs(fC4) <= m_fEpsilon )
    {
        // polynomial is cubic
        return GetBound(fC0,fC1,fC2,fC3);
    }

    Real fInvC4 = ((Real)1.0)/fC4;
    Real fMax = Math<Real>::FAbs(fC0)*fInvC4;

    Real fTmp = Math<Real>::FAbs(fC1)*fInvC4;
    if ( fTmp > fMax )
        fMax = fTmp;

    fTmp = Math<Real>::FAbs(fC2)*fInvC4;
    if ( fTmp > fMax )
        fMax = fTmp;

    fTmp = Math<Real>::FAbs(fC3)*fInvC4;
    if ( fTmp > fMax )
        fMax = fTmp;

    return (Real)1.0 + fMax;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::IsBalancedCompanion4 (Real fA10, Real fA21,
    Real fA32, Real fA03, Real fA13, Real fA23, Real fA33)
{
    const Real fTolerance = (Real)0.001;

    // row/col 0
    Real fRowNorm = fA03;
    Real fColNorm = fA10;
    Real fTest = Math<Real>::FAbs(1.0f - fColNorm/fRowNorm);
    if ( fTest > fTolerance )
        return false;

    // row/col 1
    fRowNorm = ( fA10 >= fA13 ? fA10 : fA13 );
    fColNorm = fA21;
    fTest = Math<Real>::FAbs(1.0f - fColNorm/fRowNorm);
    if ( fTest > fTolerance )
        return false;

    // row/col 2
    fRowNorm = ( fA21 >= fA23 ? fA21 : fA23 );
    fColNorm = fA32;
    fTest = Math<Real>::FAbs(1.0f - fColNorm/fRowNorm);
    if ( fTest > fTolerance )
        return false;

    // row/col 3
    fRowNorm = ( fA32 >= fA33 ? fA32 : fA33 );
    fColNorm = ( fA03 >= fA13 ? fA03 : fA13 );
    if ( fA23 > fColNorm )
        fColNorm = fA23;
    if ( fA33 > fColNorm )
        fColNorm = fA33;
    fTest = Math<Real>::FAbs(1.0f - fColNorm/fRowNorm);
    return fTest <= fTolerance;
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::BalanceCompanion4 (GMatrix<Real>& rkMat)
{
    Real fA10 = Math<Real>::FAbs(rkMat[1][0]);
    Real fA21 = Math<Real>::FAbs(rkMat[2][1]);
    Real fA32 = Math<Real>::FAbs(rkMat[3][2]);
    Real fA03 = Math<Real>::FAbs(rkMat[0][3]);
    Real fA13 = Math<Real>::FAbs(rkMat[1][3]);
    Real fA23 = Math<Real>::FAbs(rkMat[2][3]);
    Real fA33 = Math<Real>::FAbs(rkMat[3][3]);
    Real fRowNorm, fColNorm, fScale, fInvScale;

    const int iMax = 16;
    int i;
    for (i = 0; i < iMax; i++)
    {
        // balance row/col 0
        fRowNorm = fA03;
        fColNorm = fA10;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fA03 *= fScale;
        fA10 = fA03;

        // balance row/col 1
        fRowNorm = ( fA10 >= fA13 ? fA10 : fA13 );
        fColNorm = fA21;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fInvScale = ((Real)1.0)/fScale;
        fA10 *= fScale;
        fA13 *= fScale;
        fA21 *= fInvScale;

        // balance row/col 2
        fRowNorm = ( fA21 >= fA23 ? fA21 : fA23 );
        fColNorm = fA32;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fInvScale = ((Real)1.0)/fScale;
        fA21 *= fScale;
        fA23 *= fScale;
        fA32 *= fInvScale;

        // balance row/col 3
        fRowNorm = ( fA32 >= fA33 ? fA32 : fA33 );
        fColNorm = ( fA03 >= fA13 ? fA03 : fA13 );
        if ( fA23 > fColNorm )
            fColNorm = fA23;
        if ( fA33 > fColNorm )
            fColNorm = fA33;
        fScale = Math<Real>::Sqrt(fColNorm/fRowNorm);
        fInvScale = ((Real)1.0)/fScale;
        fA32 *= fScale;
        fA03 *= fInvScale;
        fA13 *= fInvScale;
        fA23 *= fInvScale;

        if ( IsBalancedCompanion4(fA10,fA21,fA32,fA03,fA13,fA23,fA33) )
            break;
    }
    assert( i < iMax );

    rkMat[1][0] = ( rkMat[1][0] >= (Real)0.0 ? fA10 : -fA10);
    rkMat[2][1] = ( rkMat[2][1] >= (Real)0.0 ? fA21 : -fA21);
    rkMat[3][2] = ( rkMat[3][2] >= (Real)0.0 ? fA32 : -fA32);
    rkMat[0][3] = ( rkMat[0][3] >= (Real)0.0 ? fA03 : -fA03);
    rkMat[1][3] = ( rkMat[1][3] >= (Real)0.0 ? fA13 : -fA13);
    rkMat[2][3] = ( rkMat[2][3] >= (Real)0.0 ? fA23 : -fA23);
    rkMat[3][3] = ( rkMat[3][3] >= (Real)0.0 ? fA33 : -fA33);
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::QRIteration4 (GMatrix<Real>& rkMat)
{
    GVector<Real> kW(4);
    GMatrix<Real> kMS(3,3);
    Real fRHS, fTrace, fDet, afSaveRoot[2];
    int i, j, iSaveCount;
    for (i = 0; i < m_iMaxIterations; i++)
    {
        fRHS = m_fEpsilon*(Math<Real>::FAbs(rkMat[0][0]) +
            Math<Real>::FAbs(rkMat[1][1]));

        if ( Math<Real>::FAbs(rkMat[1][0]) <= fRHS )
        {
            // mat[0][0] is a root, reduce the 3-by-3 submatrix
            // TO DO:  Avoid the copy and pass row/column offsets to the
            // FrancisQR method.
            kMS[0][0] = rkMat[1][1];
            kMS[0][1] = rkMat[1][2];
            kMS[0][2] = rkMat[1][3];
            kMS[1][0] = rkMat[2][1];
            kMS[1][1] = rkMat[2][2];
            kMS[1][2] = rkMat[2][3];
            kMS[2][0] = rkMat[3][1];
            kMS[2][1] = rkMat[3][2];
            kMS[2][2] = rkMat[3][3];
            QRIteration3(kMS);
            m_afRoot[m_iCount++] = rkMat[0][0];
            return true;
        }

        fRHS = m_fEpsilon*(Math<Real>::FAbs(rkMat[1][1]) +
            Math<Real>::FAbs(rkMat[2][2]));

        if ( Math<Real>::FAbs(rkMat[2][1]) <= fRHS )
        {
            // The matrix is decoupled into two 2-by-2 blocks.  Solve the
            // quadratics for the blocks.
            fTrace = rkMat[0][0] + rkMat[1][1];
            fDet = rkMat[0][0]*rkMat[1][1] - rkMat[0][1]*rkMat[1][0];
            FindA(fDet,-fTrace,(Real)1.0);
            iSaveCount = m_iCount;
            for (j = 0; j < iSaveCount; j++)
                afSaveRoot[j] = m_afRoot[j];

            fTrace = rkMat[2][2] + rkMat[3][3];
            fDet = rkMat[2][2]*rkMat[3][3] - rkMat[2][3]*rkMat[3][2];
            FindA(fDet,-fTrace,(Real)1.0);
            for (j = 0; j < iSaveCount; j++)
                m_afRoot[m_iCount++] = afSaveRoot[j];
            return m_iCount > 0;
        }

        fRHS = m_fEpsilon*(Math<Real>::FAbs(rkMat[2][2]) +
            Math<Real>::FAbs(rkMat[3][3]));

        if ( Math<Real>::FAbs(rkMat[3][2]) <= fRHS )
        {
            // mat[3][3] is a root, reduce the 3-by-3 submatrix
            // TO DO:  Avoid the copy and pass row/column offsets to the
            // FrancisQR method.
            kMS[0][0] = rkMat[0][0];
            kMS[0][1] = rkMat[0][1];
            kMS[0][2] = rkMat[0][2];
            kMS[1][0] = rkMat[1][0];
            kMS[1][1] = rkMat[1][1];
            kMS[1][2] = rkMat[1][2];
            kMS[2][0] = rkMat[2][0];
            kMS[2][1] = rkMat[2][1];
            kMS[2][2] = rkMat[2][2];
            QRIteration3(kMS);
            m_afRoot[m_iCount++] = rkMat[3][3];
            return true;
        }

        FrancisQRStep(rkMat,kW);
    }

    // TO DO:  What to do if the maximum iterations were exceeded?  Maybe a
    // random perturbation to "kick" the system a bit might work?
    //
    // If you want to trap exceeding the maximum iterations, uncomment the
    // 'assert' line of code.
    //
    // assert( false );

    // For now, decouple the matrix using the smallest subdiagonal entry.
    i = 0;
    Real fMin = Math<Real>::FAbs(rkMat[1][0]);
    Real fAbs = Math<Real>::FAbs(rkMat[2][1]);
    if ( fAbs < fMin )
    {
        fMin = fAbs;
        i = 1;
    }
    fAbs = Math<Real>::FAbs(rkMat[3][2]);
    if ( fAbs < fMin )
    {
        fMin = fAbs;
        i = 2;
    }

    if ( i == 0 )
    {
        // mat[0][0] is a root, reduce the 3-by-3 submatrix
        // TO DO:  Avoid the copy and pass row/column offsets to the
        // FrancisQR method.
        kMS[0][0] = rkMat[1][1];
        kMS[0][1] = rkMat[1][2];
        kMS[0][2] = rkMat[1][3];
        kMS[1][0] = rkMat[2][1];
        kMS[1][1] = rkMat[2][2];
        kMS[1][2] = rkMat[2][3];
        kMS[2][0] = rkMat[3][1];
        kMS[2][1] = rkMat[3][2];
        kMS[2][2] = rkMat[3][3];
        QRIteration3(kMS);
        m_afRoot[m_iCount++] = rkMat[0][0];
    }
    else if ( i == 1 )
    {
        // The matrix is decoupled into two 2-by-2 blocks.  Solve the
        // quadratics for the blocks.
        fTrace = rkMat[0][0] + rkMat[1][1];
        fDet = rkMat[0][0]*rkMat[1][1] - rkMat[0][1]*rkMat[1][0];
        FindA(fDet,-fTrace,(Real)1.0);
        iSaveCount = m_iCount;
        for (j = 0; j < iSaveCount; j++)
            afSaveRoot[j] = m_afRoot[j];

        fTrace = rkMat[2][2] + rkMat[3][3];
        fDet = rkMat[2][2]*rkMat[3][3] - rkMat[2][3]*rkMat[3][2];
        FindA(fDet,-fTrace,(Real)1.0);
        for (j = 0; j < iSaveCount; j++)
            m_afRoot[m_iCount++] = afSaveRoot[j];
    }
    else  // i == 2
    {
        // mat[3][3] is a root, reduce the 3-by-3 submatrix
        // TO DO:  Avoid the copy and pass row/column offsets to the
        // FrancisQR method.
        kMS[0][0] = rkMat[0][0];
        kMS[0][1] = rkMat[0][1];
        kMS[0][2] = rkMat[0][2];
        kMS[1][0] = rkMat[1][0];
        kMS[1][1] = rkMat[1][1];
        kMS[1][2] = rkMat[1][2];
        kMS[2][0] = rkMat[2][0];
        kMS[2][1] = rkMat[2][1];
        kMS[2][2] = rkMat[2][2];
        QRIteration3(kMS);
        m_afRoot[m_iCount++] = rkMat[3][3];
    }

    return m_iCount > 0;
}
//----------------------------------------------------------------------------
