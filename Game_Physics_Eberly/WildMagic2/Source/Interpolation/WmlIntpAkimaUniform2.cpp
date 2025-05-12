// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpAkimaUniform2.h"
#include "WmlMath.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpAkimaUniform2<Real>::IntpAkimaUniform2 (int iXBound, int iYBound,
    Real fXMin, Real fXSpacing, Real fYMin, Real fYSpacing, Real** aafF)
{
    // At least a 3x3 block of data points are needed to construct the
    // estimates of the boundary derivatives.
    assert( iXBound >= 3 && iYBound >= 3 && aafF );
    assert( fXSpacing > (Real)0.0 && fYSpacing > (Real)0.0 );

    m_iXBound = iXBound;
    m_iYBound = iYBound;
    m_fXMin = fXMin;
    m_fXSpacing = fXSpacing;
    m_fYMin = fYMin;
    m_fYSpacing = fYSpacing;
    m_aafF = aafF;

    int iXBoundM1 = iXBound - 1, iYBoundM1 = iYBound - 1;
    m_iQuantity = iXBound*iYBound;
    m_fXMax = fXMin + fXSpacing*iXBoundM1;
    m_fYMax = fYMin + fYSpacing*iYBoundM1;

    // compute slopes
    Real fInvDX = ((Real)1.0)/fXSpacing, fInvDY = ((Real)1.0)/fYSpacing;
    Real fInvDXDY = fInvDX*fInvDY;
    Real** aafXSlope;
    Allocate2D(iXBound+3,iYBound,aafXSlope);  // xslope[y][x]
    Real** aafYSlope;
    Allocate2D(iYBound+3,iXBound,aafYSlope);  // yslope[x][y]

    int iX, iY;
    for (iY = 0; iY < iYBound; iY++)
    {
        for (iX = 0; iX < iXBoundM1; iX++)
            aafXSlope[iY][iX+2] = (aafF[iY][iX+1]-aafF[iY][iX])*fInvDX;

        aafXSlope[iY][1] = ((Real)2.0)*aafXSlope[iY][2] - aafXSlope[iY][3];
        aafXSlope[iY][0] = ((Real)2.0)*aafXSlope[iY][1] - aafXSlope[iY][2];
        aafXSlope[iY][iXBound+1] = ((Real)2.0)*aafXSlope[iY][iXBound] -
            aafXSlope[iY][iXBound-1];
        aafXSlope[iY][iXBound+2] = ((Real)2.0)*aafXSlope[iY][iXBound+1] -
            aafXSlope[iY][iXBound];
    }

    for (iX = 0; iX < iXBound; iX++)
    {
        for (iY = 0; iY < iYBoundM1; iY++)
            aafYSlope[iX][iY+2] = (aafF[iY+1][iX]-aafF[iY][iX])*fInvDY;

        aafYSlope[iX][1] = ((Real)2.0)*aafYSlope[iX][2] - aafYSlope[iX][3];
        aafYSlope[iX][0] = ((Real)2.0)*aafYSlope[iX][1] - aafYSlope[iX][2];
        aafYSlope[iX][iYBound+1] = ((Real)2.0)*aafYSlope[iX][iYBound] -
            aafYSlope[iX][iYBound-1];
        aafYSlope[iX][iYBound+2] = ((Real)2.0)*aafYSlope[iX][iYBound+1] -
            aafYSlope[iX][iYBound];
    }

    // construct first-order derivatives
    Real** aafFX;
    Allocate2D(iXBound,iYBound,aafFX);
    Real** aafFY;
    Allocate2D(iXBound,iYBound,aafFY);

    for (iY = 0; iY < iYBound; iY++)
    {
        for (iX = 0; iX < iXBound; iX++)
            aafFX[iY][iX] = ComputeDerivative(aafXSlope[iY]+iX);
    }

    for (iX = 0; iX < iXBound; iX++)
    {
        for (iY = 0; iY < iYBound; iY++)
            aafFY[iY][iX] = ComputeDerivative(aafYSlope[iX]+iY);
    }

    // construct second-order derivatives
    Real** aafFXY;
    Allocate2D(iXBound,iYBound,aafFXY);

    unsigned int iX0 = iXBoundM1, iX1 = iX0-1,  iX2 = iX1-1;
    unsigned int iY0 = iYBoundM1, iY1 = iY0-1,  iY2 = iY1-1;

    // corners
    aafFXY[0][0] = ((Real)0.25)*fInvDXDY*(
         ((Real) 9.0)*aafF[0][0]
        -((Real)12.0)*aafF[0][1]
        +((Real) 3.0)*aafF[0][2]
        -((Real)12.0)*aafF[1][0]
        +((Real)16.0)*aafF[1][1]
        -((Real) 4.0)*aafF[1][2]
        +((Real) 3.0)*aafF[2][0]
        -((Real) 4.0)*aafF[2][1]
        +             aafF[2][2]);

    aafFXY[0][iXBoundM1] = ((Real)0.25)*fInvDXDY*(
         ((Real) 9.0)*aafF[0][iX0]
        -((Real)12.0)*aafF[0][iX1]
        +((Real) 3.0)*aafF[0][iX2]
        -((Real)12.0)*aafF[1][iX0]
        +((Real)16.0)*aafF[1][iX1]
        -((Real) 4.0)*aafF[1][iX2]
        +((Real) 3.0)*aafF[2][iX0]
        -((Real) 4.0)*aafF[2][iX1]
        +             aafF[2][iX2]);

    aafFXY[iYBoundM1][0] = ((Real)0.25)*fInvDXDY*(
         ((Real)9.0)*aafF[iY0][0]
        -((Real)12.0)*aafF[iY0][1]
        +((Real) 3.0)*aafF[iY0][2]
        -((Real)12.0)*aafF[iY1][0]
        +((Real)16.0)*aafF[iY1][1]
        -((Real) 4.0)*aafF[iY1][2]
        +((Real) 3.0)*aafF[iY2][0]
        -((Real) 4.0)*aafF[iY2][1]
        +             aafF[iY2][2]);

    aafFXY[iYBoundM1][iXBoundM1] = ((Real)0.25)*fInvDXDY*(
         ((Real)9.0)*aafF[iY0][iX0]
        -((Real)12.0)*aafF[iY0][iX1]
        +((Real) 3.0)*aafF[iY0][iX2]
        -((Real)12.0)*aafF[iY1][iX0]
        +((Real)16.0)*aafF[iY1][iX1]
        -((Real) 4.0)*aafF[iY1][iX2]
        +((Real) 3.0)*aafF[iY2][iX0]
        -((Real) 4.0)*aafF[iY2][iX1]
        +             aafF[iY2][iX2]);

    // x-edges
    for (iX = 1; iX < iXBoundM1; iX++)
    {
        aafFXY[0][iX] = ((Real)0.25)*fInvDXDY*(
            ((Real)3.0)*(aafF[0][iX-1] - aafF[0][iX+1])
           -((Real)4.0)*(aafF[1][iX-1] - aafF[1][iX+1])
           +            (aafF[2][iX-1] - aafF[2][iX+1]));

        aafFXY[iYBoundM1][iX] = ((Real)0.25)*fInvDXDY*(
            3.0f*(aafF[iY0][iX-1] - aafF[iY0][iX+1])
           -4.0f*(aafF[iY1][iX-1] - aafF[iY1][iX+1])
           +     (aafF[iY2][iX-1] - aafF[iY2][iX+1]));
    }

    // y-edges
    for (iY = 1; iY < iYBoundM1; iY++)
    {
        aafFXY[iY][0] = ((Real)0.25)*fInvDXDY*(
            ((Real)3.0)*(aafF[iY-1][0] - aafF[iY+1][0])
           -((Real)4.0)*(aafF[iY-1][1] - aafF[iY+1][1])
           +            (aafF[iY-1][2] - aafF[iY+1][2]));

        aafFXY[iY][iXBoundM1] = ((Real)0.25)*fInvDXDY*(
            ((Real)3.0)*(aafF[iY-1][iX0] - aafF[iY+1][iX0])
           -((Real)4.0)*(aafF[iY-1][iX1] - aafF[iY+1][iX1])
           +            (aafF[iY-1][iX2] - aafF[iY+1][iX2]));
    }

    // interior
    for (iY = 1; iY < iYBoundM1; iY++)
    {
        for (iX = 1; iX < iXBoundM1; iX++)
        {
            aafFXY[iY][iX] = ((Real)0.25)*fInvDXDY*(aafF[iY-1][iX-1] -
                aafF[iY-1][iX+1] - aafF[iY+1][iX-1] + aafF[iY+1][iX+1]);
        }
    }

    // construct polynomials
    Allocate2D(iXBoundM1,iYBoundM1,m_aakPoly);
    for (iY = 0; iY < iYBoundM1; iY++)
    {
        for (iX = 0; iX < iXBoundM1; iX++)
        {
            // Note the 'transposing' of the 2x2 blocks (to match notation
            // used in the polynomial definition).
            Real aafG[2][2] =
            {
                {aafF[iY][iX], aafF[iY+1][iX]},
                {aafF[iY][iX+1], aafF[iY+1][iX+1]}
            };

            Real aafGX[2][2] =
            {
                {aafFX[iY][iX], aafFX[iY+1][iX]},
                {aafFX[iY][iX+1], aafFX[iY+1][iX+1]}
            };

            Real aafGY[2][2] =
            {
                {aafFY[iY][iX], aafFY[iY+1][iX]},
                {aafFY[iY][iX+1], aafFY[iY+1][iX+1]}
            };

            Real aafGXY[2][2] =
            {
                {aafFXY[iY][iX], aafFXY[iY+1][iX]},
                {aafFXY[iY][iX+1], aafFXY[iY+1][iX+1]}
            };

            Construct(m_aakPoly[iY][iX],aafG,aafGX,aafGY,aafGXY);
        }
    }

    Deallocate2D(aafXSlope);
    Deallocate2D(aafYSlope);
    Deallocate2D(aafFX);
    Deallocate2D(aafFY);
    Deallocate2D(aafFXY);
}
//----------------------------------------------------------------------------
template <class Real>
IntpAkimaUniform2<Real>::~IntpAkimaUniform2 ()
{
    Deallocate2D(m_aakPoly);
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform2<Real>::GetXBound () const
{
    return m_iXBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform2<Real>::GetYBound () const
{
    return m_iYBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpAkimaUniform2<Real>::GetQuantity () const
{
    return m_iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
Real** IntpAkimaUniform2<Real>::GetF () const
{
    return m_aafF;
}
//----------------------------------------------------------------------------
template <class Real>
typename IntpAkimaUniform2<Real>::Polynomial**
IntpAkimaUniform2<Real>::GetPolynomials () const
{
    return m_aakPoly;
}
//----------------------------------------------------------------------------
template <class Real>
const typename IntpAkimaUniform2<Real>::Polynomial&
IntpAkimaUniform2<Real>::GetPolynomial (int iX, int iY) const
{
    assert( iX < m_iXBound-1 && iY < m_iYBound-1 );
    return m_aakPoly[iY][iX];
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::GetXMin () const
{
    return m_fXMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::GetXMax () const
{
    return m_fXMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::GetXSpacing () const
{
    return m_fXSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::GetYMin () const
{
    return m_fYMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::GetYMax () const
{
    return m_fYMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::GetYSpacing () const
{
    return m_fYSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::ComputeDerivative (Real* afSlope) const
{
    if ( afSlope[1] != afSlope[2] )
    {
        if ( afSlope[0] != afSlope[1] )
        {
            if ( afSlope[2] != afSlope[3] )
            {
                Real fAD0 = Math<Real>::FAbs(afSlope[3] - afSlope[2]);
                Real fAD1 = Math<Real>::FAbs(afSlope[0] - afSlope[1]);
                return (fAD0*afSlope[1]+fAD1*afSlope[2])/(fAD0+fAD1);
            }
            else
            {
                return afSlope[2];
            }
        }
        else
        {
            if ( afSlope[2] != afSlope[3] )
                return afSlope[1];
            else
                return ((Real)0.5)*(afSlope[1]+afSlope[2]);
        }
    }
    else
    {
        return afSlope[1];
    }
}
//----------------------------------------------------------------------------
template <class Real>
void IntpAkimaUniform2<Real>::Construct (Polynomial& rkPoly, Real aafF[2][2],
    Real aafFX[2][2], Real aafFY[2][2], Real aafFXY[2][2])
{
    Real fDX = m_fXSpacing;
    Real fDY = m_fYSpacing;
    Real fInvDX = ((Real)1.0)/fDX, fInvDX2 = fInvDX*fInvDX;
    Real fInvDY = ((Real)1.0)/fDY, fInvDY2 = fInvDY*fInvDY;
    Real fB0, fB1, fB2, fB3;

    rkPoly.A(0,0) = aafF[0][0];
    rkPoly.A(1,0) = aafFX[0][0];
    rkPoly.A(0,1) = aafFY[0][0];
    rkPoly.A(1,1) = aafFXY[0][0];

    fB0 = (aafF[1][0] - rkPoly(0,0,fDX,(Real)0.0))*fInvDX2;
    fB1 = (aafFX[1][0] - rkPoly(1,0,fDX,(Real)0.0))*fInvDX;
    rkPoly.A(2,0) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(3,0) = (-((Real)2.0)*fB0 + fB1)*fInvDX;

    fB0 = (aafF[0][1] - rkPoly(0,0,(Real)0.0,fDY))*fInvDY2;
    fB1 = (aafFY[0][1] - rkPoly(0,1,(Real)0.0,fDY))*fInvDY;
    rkPoly.A(0,2) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(0,3) = (-((Real)2.0)*fB0 + fB1)*fInvDY;

    fB0 = (aafFY[1][0] - rkPoly(0,1,fDX,(Real)0.0))*fInvDX2;
    fB1 = (aafFXY[1][0] - rkPoly(1,1,fDX,(Real)0.0))*fInvDX;
    rkPoly.A(2,1) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(3,1) = (-((Real)2.0)*fB0 + fB1)*fInvDX;

    fB0 = (aafFX[0][1] - rkPoly(1,0,(Real)0.0,fDY))*fInvDY2;
    fB1 = (aafFXY[0][1] - rkPoly(1,1,(Real)0.0,fDY))*fInvDY;
    rkPoly.A(1,2) = ((Real)3.0)*fB0 - fB1;
    rkPoly.A(1,3) = (-((Real)2.0)*fB0 + fB1)*fInvDY;

    fB0 = (aafF[1][1] - rkPoly(0,0,fDX,fDY))*fInvDX2*fInvDY2;
    fB1 = (aafFX[1][1] - rkPoly(1,0,fDX,fDY))*fInvDX*fInvDY2;
    fB2 = (aafFY[1][1] - rkPoly(0,1,fDX,fDY))*fInvDX2*fInvDY;
    fB3 = (aafFXY[1][1] - rkPoly(1,1,fDX,fDY))*fInvDX*fInvDY;
    rkPoly.A(2,2) = ((Real)9.0)*fB0 - ((Real)3.0)*fB1 - ((Real)3.0)*fB2 + fB3;
    rkPoly.A(3,2) = (-((Real)6.0)*fB0 + ((Real)3.0)*fB1 + ((Real)2.0)*fB2 -
        fB3)*fInvDX;
    rkPoly.A(2,3) = (-((Real)6.0)*fB0 + ((Real)2.0)*fB1 + ((Real)3.0)*fB2 -
        fB3)*fInvDY;
    rkPoly.A(3,3) = (((Real)4.0)*fB0 - ((Real)2.0)*fB1 - ((Real)2.0)*fB2 +
        fB3)*fInvDX*fInvDY;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpAkimaUniform2<Real>::XLookup (Real fX, int& riXIndex, Real& rfDX)
    const
{
    if ( fX >= m_fXMin )
    {
        if ( fX <= m_fXMax )
        {
            for (riXIndex = 0; riXIndex+1 < m_iXBound; riXIndex++)
            {
                if ( fX < m_fXMin + m_fXSpacing*(riXIndex+1) )
                {
                    rfDX = fX - (m_fXMin + m_fXSpacing*riXIndex);
                    return true;
                }
            }

            riXIndex--;
            rfDX = fX - (m_fXMin + m_fXSpacing*riXIndex);
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
bool IntpAkimaUniform2<Real>::YLookup (Real fY, int& riYIndex, Real& rfDY)
    const
{
    if ( fY >= m_fYMin )
    {
        if ( fY <= m_fYMax )
        {
            for (riYIndex = 0; riYIndex+1 < m_iYBound; riYIndex++)
            {
                if ( fY < m_fYMin + m_fYSpacing*(riYIndex+1) )
                {
                    rfDY = fY - (m_fYMin + m_fYSpacing*riYIndex);
                    return true;
                }
            }

            riYIndex--;
            rfDY = fY - (m_fYMin + m_fYSpacing*riYIndex);
            return true;
        }
    }

    return false;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::operator() (Real fX, Real fY) const
{
    int iX, iY;
    Real fDX, fDY;

    if ( XLookup(fX,iX,fDX) && YLookup(fY,iY,fDY) )
        return m_aakPoly[iY][iX](fDX,fDY);
    else
        return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpAkimaUniform2<Real>::operator() (int iXOrder, int iYOrder, Real fX,
    Real fY) const
{
    int iX, iY;
    Real fDX, fDY;

    if ( XLookup(fX,iX,fDX) && YLookup(fY,iY,fDY) )
        return m_aakPoly[iY][iX](iXOrder,iYOrder,fDX,fDY);
    else
        return Math<Real>::MAX_REAL;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpAkimaUniform2<float>;
template class WML_ITEM IntpAkimaUniform2<double>;
}
//----------------------------------------------------------------------------
