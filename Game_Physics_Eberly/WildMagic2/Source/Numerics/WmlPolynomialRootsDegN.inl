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
bool PolynomialRoots<Real>::FindB (const Polynomial1<Real>& rkPoly,
    int iDigits)
{
    Real fBound = GetBound(rkPoly);
    return FindB(rkPoly,-fBound,fBound,iDigits);
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindN (const Polynomial1<Real>&, int)
{
    // TO DO:  Implement this.
    assert( false );
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindE (const Polynomial1<Real>&, bool)
{
    // TO DO:  Implement this.
    assert( false );
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
Real PolynomialRoots<Real>::GetBound (const Polynomial1<Real>& rkPoly)
{
    Polynomial1<Real> kCPoly = rkPoly;
    kCPoly.Compress(m_fEpsilon);
    int iDegree = kCPoly.GetDegree();
    if ( iDegree < 1 )
    {
        // polynomial is constant, return invalid bound
        return -(Real)1.0;
    }

    Real fInvCDeg = ((Real)1.0)/kCPoly[iDegree];
    Real fMax = (Real)0.0;
    for (int i = 0; i < iDegree; i++)
    {
        Real fTmp = Math<Real>::FAbs(kCPoly[i])*fInvCDeg;
        if ( fTmp > fMax )
            fMax = fTmp;
    }

    return (Real)1.0 + fMax;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindB (const Polynomial1<Real>& rkPoly,
    Real fXMin, Real fXMax, int iDigits)
{
    // reallocate root array if necessary
    if ( rkPoly.GetDegree() > m_iMaxRoot )
    {
        m_iMaxRoot = rkPoly.GetDegree();
        delete[] m_afRoot;
        m_afRoot = new Real[m_iMaxRoot];
    }

    Real fRoot;
    if ( rkPoly.GetDegree() == 1 )
    {
        if ( Bisection(rkPoly,fXMin,fXMax,iDigits,fRoot) )
        {
            m_iCount = 1;
            m_afRoot[0] = fRoot;
            return true;
        }
        m_iCount = 0;
        return false;
    }

    // get roots of derivative polynomial
    Polynomial1<Real> kDeriv = rkPoly.GetDerivative();
    FindB(kDeriv,fXMin,fXMax,iDigits);

    int i, iNewCount = 0;
    Real* afNewRoot = new Real[m_iCount+1];

    if ( m_iCount > 0 )
    {
        // find root on [xmin,root[0]]
        if ( Bisection(rkPoly,fXMin,m_afRoot[0],iDigits,fRoot) )
            afNewRoot[iNewCount++] = fRoot;

        // find root on [root[i],root[i+1]] for 0 <= i <= count-2
        for (i = 0; i <= m_iCount-2; i++)
        {
            if ( Bisection(rkPoly,m_afRoot[i],m_afRoot[i+1],iDigits,fRoot) )
                afNewRoot[iNewCount++] = fRoot;
        }

        // find root on [root[count-1],xmax]
        if ( Bisection(rkPoly,m_afRoot[m_iCount-1],fXMax,iDigits,fRoot) )
            afNewRoot[iNewCount++] = fRoot;
    }
    else
    {
        // polynomial is monotone on [xmin,xmax], has at most one root
        if ( Bisection(rkPoly,fXMin,fXMax,iDigits,fRoot) )
            afNewRoot[iNewCount++] = fRoot;
    }

    // copy to old buffer
    if ( iNewCount > 0 )
    {
        m_iCount = 1;
        m_afRoot[0] = afNewRoot[0];
        for (i = 1; i < iNewCount; i++)
        {
            Real fRootDiff = afNewRoot[i]-afNewRoot[i-1];
            if ( Math<Real>::FAbs(fRootDiff) > m_fEpsilon )
                m_afRoot[m_iCount++] = afNewRoot[i];
        }
    }
    else
    {
        m_iCount = 0;
    }

    delete[] afNewRoot;
    return m_iCount > 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::FindN (const Polynomial1<Real>&, Real, Real, int)
{
    // TO DO:  Implement this.
    assert( false );
    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::AllRealPartsNegative (
    const Polynomial1<Real>& rkPoly)
{
    // make a copy of coefficients, later calls will change the copy
    int iDegree = rkPoly.GetDegree();
    const Real* afPolyCoeff = (const Real*)rkPoly;
    Real* afCoeff = new Real[iDegree+1];
    memcpy(afCoeff,afPolyCoeff,(iDegree+1)*sizeof(Real));

    // make polynomial monic
    if ( afCoeff[iDegree] != (Real)1.0 )
    {
        Real fInv = ((Real)1.0)/afCoeff[iDegree];
        for (int i = 0; i < iDegree; i++)
            afCoeff[i] *= fInv;
        afCoeff[iDegree] = (Real)1.0;
    }

    return AllRealPartsNegative(iDegree,afCoeff);
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::AllRealPartsPositive (
    const Polynomial1<Real>& rkPoly)
{
    // make a copy of coefficients, later calls will change the copy
    int iDegree = rkPoly.GetDegree();
    const Real* afPolyCoeff = (const Real*)rkPoly;
    Real* afCoeff = new Real[iDegree+1];
    memcpy(afCoeff,afPolyCoeff,(iDegree+1)*sizeof(Real));

    // make polynomial monic
    int i;
    if ( afCoeff[iDegree] != (Real)1.0 )
    {
        Real fInv = ((Real)1.0)/afCoeff[iDegree];
        for (i = 0; i < iDegree; i++)
            afCoeff[i] *= fInv;
        afCoeff[iDegree] = (Real)1.0;
    }

    // reflect z -> -z
    int iSign = -1;
    for (i = iDegree-1; i >= 0; i--, iSign = -iSign)
        afCoeff[i] *= iSign;

    return AllRealPartsNegative(iDegree,afCoeff);
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::AllRealPartsNegative (int iDegree, Real* afCoeff)
{
    // assert:  afCoeff[iDegree] = 1

    if ( afCoeff[iDegree-1] <= (Real)0.0 )
        return false;
    if ( iDegree == 1 )
        return true;

    Real* afTmpCoeff = new Real[iDegree];
    afTmpCoeff[0] = ((Real)2.0)*afCoeff[0]*afCoeff[iDegree-1];
    int i;
    for (i = 1; i <= iDegree-2; i++) 
    {
        afTmpCoeff[i] = afCoeff[iDegree-1]*afCoeff[i];
        if ( ((iDegree-i) % 2) == 0 )
            afTmpCoeff[i] -= afCoeff[i-1];
        afTmpCoeff[i] *= (Real)2.0;
    }
    afTmpCoeff[iDegree-1] =
        ((Real)2.0)*afCoeff[iDegree-1]*afCoeff[iDegree-1];

    int iNextDegree;
    for (iNextDegree = iDegree-1; iNextDegree >= 0; iNextDegree--)
    {
        if ( afTmpCoeff[iNextDegree] != (Real)0.0 )
            break;
    }
    for (i = 0; i <= iNextDegree-1; i++)
        afCoeff[i] = afTmpCoeff[i]/afTmpCoeff[iNextDegree];
    delete[] afTmpCoeff;

    return AllRealPartsNegative(iNextDegree,afCoeff);
}
//----------------------------------------------------------------------------
template <class Real>
int PolynomialRoots<Real>::GetRootCount (const Polynomial1<Real>& rkPoly,
    Real fT0, Real fT1)
{
    int iDegree = rkPoly.GetDegree();
    const Real* afCoeff = (const Real*)rkPoly;

    if ( iDegree == 0 )
    {
        // polynomial is constant on the interval
        if ( afCoeff[0] != (Real)0.0 )
            return 0;
        else
            return -1;  // to indicate "infinitely many"
    }

    // generate the Sturm sequence
    std::vector<Polynomial1<Real>*> kSturm;
    Polynomial1<Real>* pkF0 = new Polynomial1<Real>(rkPoly);
    Polynomial1<Real>* pkF1 = new Polynomial1<Real>(pkF0->GetDerivative());
    kSturm.push_back(pkF0);
    kSturm.push_back(pkF1);

    while ( pkF1->GetDegree() > 0 )
    {
        Polynomial1<Real>* pkF2 = new Polynomial1<Real>(-1);
        Polynomial1<Real> kQuot;
        pkF0->Divide(*pkF1,kQuot,*pkF2,Math<Real>::EPSILON);
        *pkF2 = -(*pkF2);
        kSturm.push_back(pkF2);
        pkF0 = pkF1;
        pkF1 = pkF2;
    }

    int i;
    Real fValue0, fValue1;

    // count the sign changes at t0
    int iSignChanges0 = 0;
    if ( fT0 == -Math<Real>::MAX_REAL )
    {
        pkF0 = kSturm[0];
        if ( pkF0->GetDegree() & 1 )
            fValue0 = -(*pkF0)[pkF0->GetDegree()];
        else
            fValue0 = (*pkF0)[pkF0->GetDegree()];

        if ( Math<Real>::FAbs(fValue0) < m_fEpsilon )
            fValue0 = (Real)0.0;

        for (i = 1; i < (int)kSturm.size(); i++)
        {
            pkF1 = kSturm[i];

            if ( pkF1->GetDegree() & 1 )
                fValue1 = -(*pkF1)[pkF1->GetDegree()];
            else
                fValue1 = (*pkF1)[pkF1->GetDegree()];

            if ( Math<Real>::FAbs(fValue1) < m_fEpsilon )
                fValue1 = (Real)0.0;

            if ( fValue0*fValue1 < (Real)0.0 || fValue0 == (Real)0.0 )
                iSignChanges0++;

            fValue0 = fValue1;
            pkF0 = pkF1;
        }
    }
    else
    {
        pkF0 = kSturm[0];
        fValue0 = (*pkF0)(fT0);
        if ( Math<Real>::FAbs(fValue0) < m_fEpsilon )
            fValue0 = (Real)0.0;

        for (i = 1; i < (int)kSturm.size(); i++)
        {
            pkF1 = kSturm[i];
            fValue1 = (*pkF1)(fT0);
            if ( Math<Real>::FAbs(fValue1) < m_fEpsilon )
                fValue1 = (Real)0.0;

            if ( fValue0*fValue1 < (Real)0.0 || fValue0 == (Real)0.0 )
                iSignChanges0++;

            fValue0 = fValue1;
            pkF0 = pkF1;
        }
    }

    // count the sign changes at t1
    int iSignChanges1 = 0;
    if ( fT1 == Math<Real>::MAX_REAL )
    {
        pkF0 = kSturm[0];
        fValue0 = (*pkF0)[pkF0->GetDegree()];
        if ( Math<Real>::FAbs(fValue0) < m_fEpsilon )
            fValue0 = (Real)0.0;

        for (i = 1; i < (int)kSturm.size(); i++)
        {
            pkF1 = kSturm[i];
            fValue1 = (*pkF1)[pkF1->GetDegree()];
            if ( Math<Real>::FAbs(fValue1) < m_fEpsilon )
                fValue1 = (Real)0.0;

            if ( fValue0*fValue1 < (Real)0.0 || fValue0 == (Real)0.0 )
                iSignChanges1++;

            fValue0 = fValue1;
            pkF0 = pkF1;
        }
    }
    else
    {
        pkF0 = kSturm[0];
        fValue0 = (*pkF0)(fT1);
        if ( Math<Real>::FAbs(fValue0) < m_fEpsilon )
            fValue0 = (Real)0.0;

        for (i = 1; i < (int)kSturm.size(); i++)
        {
            pkF1 = kSturm[i];
            fValue1 = (*pkF1)(fT1);
            if ( Math<Real>::FAbs(fValue1) < m_fEpsilon )
                fValue1 = (Real)0.0;

            if ( fValue0*fValue1 < (Real)0.0 || fValue0 == (Real)0.0 )
                iSignChanges1++;

            fValue0 = fValue1;
            pkF0 = pkF1;
        }
    }

    // clean up
    for (i = 0; i < (int)kSturm.size(); i++)
        delete kSturm[i];

    if ( iSignChanges0 >= iSignChanges1 )
        return iSignChanges0 - iSignChanges1;

    // theoretically we should not get here
    assert( false );
    return 0;
}
//----------------------------------------------------------------------------
template <class Real>
bool PolynomialRoots<Real>::Bisection (const Polynomial1<Real>& rkPoly,
    Real fXMin, Real fXMax, int iDigits, Real& rfRoot)
{
    Real fP0 = rkPoly(fXMin);
    Real fP1 = rkPoly(fXMax);
    if ( fP0*fP1 > (Real)0.0 )
        return false;

    // determine number of iterations to get 'digits' accuracy.
    Real fTmp0 = Math<Real>::Log(fXMax-fXMin);
    Real fTmp1 = ((Real)iDigits)*LOG10;
    Real fArg = (fTmp0 + fTmp1)*INVLOG2;
    int iMaxIter = (int)(fArg + 0.5f);

    for (int i = 0; i < iMaxIter; i++)
    {
        rfRoot = ((Real)0.5)*(fXMin + fXMax);
        Real fP = rkPoly(rfRoot);
        if ( fP*fP0 < (Real)0.0 )
        {
            fXMax = rfRoot;
            fP1 = fP;
        }
        else
        {
            fXMin = rfRoot;
            fP0 = fP;
        }
    }

    return true;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// FindE support
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::GetHouseholderVector (int iSize,
    const Vector3<Real>& rkU, Vector3<Real>& rkV)
{
    // Householder vector V:
    // Given a vector U, compute a vector V such that V[0] = 1 and
    // (I-2*V*V^T/|V|^2)*U is zero in all but the first component.  The
    // matrix P = I-2*V*V^T/|V|^2 is a Householder transformation, a
    // reflection matrix.

    Real fLength = rkU[0]*rkU[0];
    int i;
    for (i = 1; i < iSize; i++)
        fLength += rkU[i]*rkU[i];
    fLength = Math<Real>::Sqrt(fLength);

    if ( fLength > m_fEpsilon )
    {
        Real fInv = ((Real)1.0)/(rkU[0]+Math<Real>::Sign(rkU[0])*fLength);
        rkV[0] = (Real)1.0;
        for (i = 1; i < iSize; i++)
            rkV[i] = fInv*rkU[i];
    }
    else
    {
        // U is the zero vector, any vector will do
        rkV[0] = (Real)1.0;
        for (i = 1; i < iSize; i++)
            rkV[i] = (Real)0.0;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::PremultiplyHouseholder (GMatrix<Real>& rkMat,
    GVector<Real>& rkW, int iRMin, int iRMax, int iCMin, int iCMax,
    int iVSize, const Vector3<Real>& rkV)
{
    // Householder premultiplication:
    // Given a matrix A and an m-by-1 vector V with V[0] = 1, let S be the
    // submatrix of A with m rows rmin <= r <= m+rmin-1 and columns
    // cmin <= c <= cmax.  Overwrite S with P*S where P = I-2*V*V^T/|V|^2.

    int iSubRows = iRMax - iRMin + 1, iSubCols = iCMax - iCMin + 1;
    int iRow, iCol;

    Real fSqrLen = rkV[0]*rkV[0];
    for (int i = 1; i < iVSize; i++)
        fSqrLen += rkV[i]*rkV[i];

    Real fBeta = -((Real)2.0)/fSqrLen;
    for (iCol = 0; iCol < iSubCols; iCol++)
    {
        rkW[iCol] = (Real)0.0;
        for (iRow = 0; iRow < iSubRows; iRow++)
            rkW[iCol] += rkV[iRow]*rkMat[iRMin+iRow][iCMin+iCol];
        rkW[iCol] *= fBeta;
    }

    for (iRow = 0; iRow < iSubRows; iRow++)
    {
        for (iCol = 0; iCol < iSubCols; iCol++)
            rkMat[iRMin+iRow][iCMin+iCol] += rkV[iRow]*rkW[iCol];
    }
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::PostmultiplyHouseholder (GMatrix<Real>& rkMat,
    GVector<Real>& rkW, int iRMin, int iRMax, int iCMin, int iCMax,
    int iVSize, const Vector3<Real>& rkV)
{
    // Householder postmultiplication:
    // Given a matrix A and an n-by-1 vector V with V[0] = 1, let S be the
    // submatrix of A with n columns cmin <= c <= n+cmin-1 and rows
    // rmin <= r <= rmax.  Overwrite S with S*P where P = I-2*V*V^T/|V|^2.

    int iSubRows = iRMax - iRMin + 1, iSubCols = iCMax - iCMin + 1;
    int iRow, iCol;

    Real fSqrLen = rkV[0]*rkV[0];
    for (int i = 1; i < iVSize; i++)
        fSqrLen += rkV[i]*rkV[i];

    Real fBeta = -((Real)2.0)/fSqrLen;
    for (iRow = 0; iRow < iSubRows; iRow++)
    {
        rkW[iRow] = (Real)0.0;
        for (iCol = 0; iCol < iSubCols; iCol++)
            rkW[iRow] += rkMat[iRMin+iRow][iCMin+iCol]*rkV[iCol];
        rkW[iRow] *= fBeta;
    }

    for (iRow = 0; iRow < iSubRows; iRow++)
    {
        for (iCol = 0; iCol < iSubCols; iCol++)
            rkMat[iRMin+iRow][iCMin+iCol] += rkW[iRow]*rkV[iCol];
    }
}
//----------------------------------------------------------------------------
template <class Real>
void PolynomialRoots<Real>::FrancisQRStep (GMatrix<Real>& rkH,
    GVector<Real>& rkW)
{
    // Given an n-by-n unreduced upper Hessenberg matrix H whose trailing
    // 2-by-2 principal submatrix has eigenvalues a1 and a2, overwrite H
    // with Z^T*H*Z where Z = P(1)*...*P(n-2) is a product of Householder
    // matrices and Z^T*(H-a1*I)*(H-a2*I) is upper triangular.

    // assert:  H is unreduced upper Hessenberg and n >= 3

    // compute first column of (H-a1*I)*(H-a2*I)
    int iN = rkH.GetRows();
    Real fTrace = rkH[iN-2][iN-2] + rkH[iN-1][iN-1];
    Real fDet = rkH[iN-2][iN-2]*rkH[iN-1][iN-1] -
        rkH[iN-2][iN-1]*rkH[iN-1][iN-2];
    Vector3<Real> kU;
    kU[0] = rkH[0][0]*rkH[1][1]+rkH[0][1]*rkH[1][0]-fTrace*rkH[0][0]+fDet;
    kU[1] = rkH[1][0]*(rkH[0][0]+rkH[1][1]-fTrace);
    kU[2] = rkH[1][0]*rkH[2][1];

    // overwrite H with P(0)*H*P(0)^T
    Vector3<Real> kV;
    GetHouseholderVector(3,kU,kV);
    PremultiplyHouseholder(rkH,rkW,0,2,0,iN-1,3,kV);
    PostmultiplyHouseholder(rkH,rkW,0,iN-1,0,2,3,kV);

    for (int i = 1; i <= iN-3; i++)
    {
        // overwrite H with P(i)*H*P(i)^T
        kU[0] = rkH[i  ][i-1];
        kU[1] = rkH[i+1][i-1];
        kU[2] = rkH[i+2][i-1];
        GetHouseholderVector(3,kU,kV);

        // The column range does not need to be 0 to iN-1 because of the
        // pattern of zeros that occur in matrix H.
        PremultiplyHouseholder(rkH,rkW,i,i+2,i-1,iN-1,3,kV);

        // The row range does not need to be 0 to iN-1 because of the pattern
        // of zeros that occur in matrix H.
        int iRMax = i+3;
        if ( iRMax >= iN )
            iRMax = iN-1;
        PostmultiplyHouseholder(rkH,rkW,0,iRMax,i,i+2,3,kV);
    }

    // overwrite H with P(n-2)*H*P(n-2)^T
    kU[0] = rkH[iN-2][iN-3];
    kU[1] = rkH[iN-1][iN-3];
    GetHouseholderVector(2,kU,kV);

    // The column range does not need to be 0 to iN-1 because of the pattern
    // of zeros that occur in matrix H.
    PremultiplyHouseholder(rkH,rkW,iN-2,iN-1,iN-3,iN-1,2,kV);

    PostmultiplyHouseholder(rkH,rkW,0,iN-1,iN-2,iN-1,2,kV);
}
//----------------------------------------------------------------------------
