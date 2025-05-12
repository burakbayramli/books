// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpBicubic2.h"
#include "WmlMath.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpBicubic2<Real>::IntpBicubic2 (int iXBound, int iYBound, Real fXMin,
    Real fXSpacing, Real fYMin, Real fYSpacing, Real** aafF, bool bCatmullRom)
{
    // At least a 3x3 block of data points are needed to construct the
    // estimates of the boundary derivatives.
    assert( iXBound >= 3 && iYBound >= 3 && aafF );
    assert( fXSpacing > (Real)0.0 && fYSpacing > (Real)0.0 );

    m_iXBound = iXBound;
    m_iYBound = iYBound;
    m_iQuantity = iXBound*iYBound;

    m_fXMin = fXMin;
    m_fXSpacing = fXSpacing;
    m_fInvXSpacing = ((Real)1.0)/fXSpacing;
    m_fYMin = fYMin;
    m_fYSpacing = fYSpacing;
    m_fInvYSpacing = ((Real)1.0)/fYSpacing;
    m_fXMax = fXMin + fXSpacing*(iXBound-1);
    m_fYMax = fYMin + fYSpacing*(iYBound-1);

    m_aafF = aafF;

    m_aafBlend = ( bCatmullRom ? ms_aafCRBlend : ms_aafBSBlend );
}
//----------------------------------------------------------------------------
template <class Real>
int IntpBicubic2<Real>::GetXBound () const
{
    return m_iXBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpBicubic2<Real>::GetYBound () const
{
    return m_iYBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpBicubic2<Real>::GetQuantity () const
{
    return m_iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
Real** IntpBicubic2<Real>::GetF () const
{
    return m_aafF;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::GetXMin () const
{
    return m_fXMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::GetXMax () const
{
    return m_fXMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::GetXSpacing () const
{
    return m_fXSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::GetYMin () const
{
    return m_fYMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::GetYMax () const
{
    return m_fYMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::GetYSpacing () const
{
    return m_fYSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::operator() (Real fX, Real fY) const
{
    // compute x-index and clamp to image
    Real fXIndex = (fX - m_fXMin)*m_fInvXSpacing;
    int iX = (int)fXIndex;
    if ( iX < 0 || iX > m_iXBound - 1 )
        return Math<Real>::MAX_REAL;

    // compute y-index and clamp to image
    Real fYIndex = (fY - m_fYMin)*m_fInvYSpacing;
    int iY = (int)fYIndex;
    if ( iY < 0 || iY > m_iYBound - 1 )
        return Math<Real>::MAX_REAL;

    Real afU[4];
    afU[0] = (Real)1.0;
    afU[1] = fXIndex - iX;
    afU[2] = afU[1]*afU[1];
    afU[3] = afU[1]*afU[2];

    Real afV[4];
    afV[0] = (Real)1.0;
    afV[1] = fYIndex - iY;
    afV[2] = afV[1]*afV[1];
    afV[3] = afV[1]*afV[2];

    // compute P = M*U and Q = M*V
    Real afP[4], afQ[4];
    int iRow, iCol;
    for (iRow = 0; iRow < 4; iRow++)
    {
        afP[iRow] = (Real)0.0;
        afQ[iRow] = (Real)0.0;
        for (iCol = 0; iCol < 4; iCol++)
        {
            afP[iRow] += m_aafBlend[iRow][iCol]*afU[iCol];
            afQ[iRow] += m_aafBlend[iRow][iCol]*afV[iCol];
        }
    }

    // compute (M*U)^t D (M*V) where D is the 4x4 subimage containing (x,y)
    iX--;
    iY--;
    Real fResult = (Real)0.0;
    for (iRow = 0; iRow < 4; iRow++)
    {
        int iYClamp = iY + iRow;
        if ( iYClamp < 0 )
            iYClamp = 0;
        else if ( iYClamp > m_iYBound - 1 )
            iYClamp = m_iYBound - 1;

        for (iCol = 0; iCol < 4; iCol++)
        {
            int iXClamp = iX + iCol;
            if ( iXClamp < 0 )
                iXClamp = 0;
            else if ( iXClamp > m_iXBound - 1 )
                iXClamp = m_iXBound - 1;

            fResult += afQ[iRow]*m_aafF[iYClamp][iXClamp]*afP[iCol];
        }
    }

    return fResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpBicubic2<Real>::operator() (int iXOrder, int iYOrder, Real fX,
    Real fY) const
{
    // compute x-index and clamp to image
    Real fXIndex = (fX - m_fXMin)*m_fInvXSpacing;
    int iX = (int)fXIndex;
    if ( iX < 0 || iX > m_iXBound - 1 )
        return Math<Real>::MAX_REAL;

    // compute y-index and clamp to image
    Real fYIndex = (fY - m_fYMin)*m_fInvYSpacing;
    int iY = (int)fYIndex;
    if ( iY < 0 || iY > m_iYBound - 1 )
        return Math<Real>::MAX_REAL;

    Real afU[4], fDX, fXMult;
    switch ( iXOrder )
    {
    case 0:
        fDX = fXIndex - iX;
        afU[0] = (Real)1.0;
        afU[1] = fDX;
        afU[2] = fDX*afU[1];
        afU[3] = fDX*afU[2];
        fXMult = (Real)1.0;
        break;
    case 1:
        fDX = fXIndex - iX;
        afU[0] = (Real)0.0;
        afU[1] = (Real)1.0;
        afU[2] = ((Real)2.0)*fDX;
        afU[3] = ((Real)3.0)*fDX*fDX;
        fXMult = m_fInvXSpacing;
        break;
    case 2:
        fDX = fXIndex - iX;
        afU[0] = (Real)0.0;
        afU[1] = (Real)0.0;
        afU[2] = (Real)2.0;
        afU[3] = (Real)6.0*fDX;
        fXMult = m_fInvXSpacing*m_fInvXSpacing;
        break;
    case 3:
        afU[0] = (Real)0.0;
        afU[1] = (Real)0.0;
        afU[2] = (Real)0.0;
        afU[3] = (Real)6.0;
        fXMult = m_fInvXSpacing*m_fInvXSpacing*m_fInvXSpacing;
        break;
    default:
        return (Real)0.0;
    }

    Real afV[4], fDY, fYMult;
    switch ( iYOrder )
    {
    case 0:
        fDY = fYIndex - iY;
        afV[0] = (Real)1.0;
        afV[1] = fDY;
        afV[2] = fDY*afV[1];
        afV[3] = fDY*afV[2];
        fYMult = (Real)1.0;
        break;
    case 1:
        fDY = fYIndex - iY;
        afV[0] = (Real)0.0;
        afV[1] = (Real)1.0;
        afV[2] = ((Real)2.0)*fDY;
        afV[3] = ((Real)3.0)*fDY*fDY;
        fYMult = m_fInvYSpacing;
        break;
    case 2:
        fDY = fYIndex - iY;
        afV[0] = (Real)0.0;
        afV[1] = (Real)0.0;
        afV[2] = (Real)2.0;
        afV[3] = ((Real)6.0)*fDY;
        fYMult = m_fInvYSpacing*m_fInvYSpacing;
        break;
    case 3:
        afV[0] = (Real)0.0;
        afV[1] = (Real)0.0;
        afV[2] = (Real)0.0;
        afV[3] = (Real)6.0;
        fYMult = m_fInvYSpacing*m_fInvYSpacing*m_fInvYSpacing;
        break;
    default:
        return (Real)0.0;
    }

    // compute P = M*U and Q = M*V
    Real afP[4], afQ[4];
    int iRow, iCol;
    for (iRow = 0; iRow < 4; iRow++)
    {
        afP[iRow] = (Real)0.0;
        afQ[iRow] = (Real)0.0;
        for (iCol = 0; iCol < 4; iCol++)
        {
            afP[iRow] += m_aafBlend[iRow][iCol]*afU[iCol];
            afQ[iRow] += m_aafBlend[iRow][iCol]*afV[iCol];
        }
    }

    // compute (M*U)^t D (M*V) where D is the 4x4 subimage containing (x,y)
    iX--;
    iY--;
    Real fResult = (Real)0.0;
    for (iRow = 0; iRow < 4; iRow++)
    {
        int iYClamp = iY + iRow;
        if ( iYClamp < 0 )
            iYClamp = 0;
        else if ( iYClamp > m_iYBound - 1 )
            iYClamp = m_iYBound - 1;

        for (iCol = 0; iCol < 4; iCol++)
        {
            int iXClamp = iX + iCol;
            if ( iXClamp < 0 )
                iXClamp = 0;
            else if ( iXClamp > m_iXBound - 1 )
                iXClamp = m_iXBound - 1;

            fResult += afQ[iRow]*m_aafF[iYClamp][iXClamp]*afP[iCol];
        }
    }
    fResult *= fXMult*fYMult;

    return fResult;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpBicubic2<float>;
const float IntpBicubic2f::ms_aafCRBlend[4][4] =
{
    { 0.0f, -0.5f,  1.0f, -0.5f },
    { 1.0f,  0.0f, -2.5f,  1.5f },
    { 0.0f,  0.5f,  2.0f, -1.5f },
    { 0.0f,  0.0f, -0.5f,  0.5f }
};
const float IntpBicubic2f::ms_aafBSBlend[4][4] =
{
    { 1.0f/6.0f, -3.0f/6.0f,  3.0f/6.0f, -1.0f/6.0f },
    { 4.0f/6.0f,  0.0f/6.0f, -6.0f/6.0f,  3.0f/6.0f },
    { 1.0f/6.0f,  3.0f/6.0f,  3.0f/6.0f, -3.0f/6.0f },
    { 0.0f/6.0f,  0.0f/6.0f,  0.0f/6.0f,  1.0f/6.0f }
};

template class WML_ITEM IntpBicubic2<double>;
const double IntpBicubic2d::ms_aafCRBlend[4][4] =
{
    { 0.0, -0.5,  1.0, -0.5 },
    { 1.0,  0.0, -2.5,  1.5 },
    { 0.0,  0.5,  2.0, -1.5 },
    { 0.0,  0.0, -0.5,  0.5 }
};
const double IntpBicubic2d::ms_aafBSBlend[4][4] =
{
    { 1.0/6.0, -3.0/6.0,  3.0/6.0, -1.0/6.0 },
    { 4.0/6.0,  0.0/6.0, -6.0/6.0,  3.0/6.0 },
    { 1.0/6.0,  3.0/6.0,  3.0/6.0, -3.0/6.0 },
    { 0.0/6.0,  0.0/6.0,  0.0/6.0,  1.0/6.0 }
};
}
//----------------------------------------------------------------------------
