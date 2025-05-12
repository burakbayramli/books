// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlIntpTricubic3.h"
#include "WmlMath.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
IntpTricubic3<Real>::IntpTricubic3 (int iXBound, int iYBound, int iZBound,
    Real fXMin, Real fXSpacing, Real fYMin, Real fYSpacing, Real fZMin,
    Real fZSpacing, Real*** aaafF, bool bCatmullRom)
{
    // At least a 4x4x4 block of data points are needed to construct the
    // tricubic interpolation.
    assert( iXBound >= 4 && iYBound >= 4 && iZBound >= 4 && aaafF );
    assert( fXSpacing > (Real)0.0 && fYSpacing > (Real)0.0
        &&  fZSpacing > (Real)0.0 );

    m_iXBound = iXBound;
    m_iYBound = iYBound;
    m_iZBound = iZBound;
    m_iQuantity = iXBound*iYBound*iZBound;

    m_fXMin = fXMin;
    m_fXSpacing = fXSpacing;
    m_fInvXSpacing = ((Real)1.0)/fXSpacing;
    m_fXMax = fXMin + fXSpacing*(iXBound-1);

    m_fYMin = fYMin;
    m_fYSpacing = fYSpacing;
    m_fInvYSpacing = ((Real)1.0)/fYSpacing;
    m_fYMax = fYMin + fYSpacing*(iYBound-1);

    m_fZMin = fZMin;
    m_fZSpacing = fZSpacing;
    m_fInvZSpacing = ((Real)1.0)/fZSpacing;
    m_fZMax = fYMin + fZSpacing*(iZBound-1);

    m_aaafF = aaafF;

    m_aafBlend = ( bCatmullRom ? ms_aafCRBlend : ms_aafBSBlend );
}
//----------------------------------------------------------------------------
template <class Real>
int IntpTricubic3<Real>::GetXBound () const
{
    return m_iXBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpTricubic3<Real>::GetYBound () const
{
    return m_iYBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpTricubic3<Real>::GetZBound () const
{
    return m_iZBound;
}
//----------------------------------------------------------------------------
template <class Real>
int IntpTricubic3<Real>::GetQuantity () const
{
    return m_iQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
Real*** IntpTricubic3<Real>::GetF () const
{
    return m_aaafF;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetXMin () const
{
    return m_fXMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetXMax () const
{
    return m_fXMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetXSpacing () const
{
    return m_fXSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetYMin () const
{
    return m_fYMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetYMax () const
{
    return m_fYMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetYSpacing () const
{
    return m_fYSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetZMin () const
{
    return m_fZMin;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetZMax () const
{
    return m_fZMax;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::GetZSpacing () const
{
    return m_fZSpacing;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::operator() (Real fX, Real fY, Real fZ) const
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

    // compute z-index and clamp to image
    Real fZIndex = (fZ - m_fZMin)*m_fInvZSpacing;
    int iZ = (int)fZIndex;
    if ( iZ < 0 || iZ > m_iZBound - 1 )
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

    Real afW[4];
    afW[0] = (Real)1.0;
    afW[1] = fZIndex - iZ;
    afW[2] = afW[1]*afW[1];
    afW[3] = afW[1]*afW[2];

    // compute P = M*U, Q = M*V, R = M*W
    Real afP[4], afQ[4], afR[4];
    int iRow, iCol;
    for (iRow = 0; iRow < 4; iRow++)
    {
        afP[iRow] = (Real)0.0;
        afQ[iRow] = (Real)0.0;
        afR[iRow] = (Real)0.0;
        for (iCol = 0; iCol < 4; iCol++)
        {
            afP[iRow] += m_aafBlend[iRow][iCol]*afU[iCol];
            afQ[iRow] += m_aafBlend[iRow][iCol]*afV[iCol];
            afR[iRow] += m_aafBlend[iRow][iCol]*afW[iCol];
        }
    }

    // compute the tensor product (M*U)(M*V)(M*W)*D where D is the 4x4x4
    // subimage containing (x,y,z)
    iX--;
    iY--;
    iZ--;
    Real fResult = (Real)0.0;
    for (int iSlice = 0; iSlice < 4; iSlice++)
    {
        int iZClamp = iZ + iSlice;
        if ( iZClamp < 0 )
            iZClamp = 0;
        else if ( iZClamp > m_iZBound - 1 )
            iZClamp = m_iZBound - 1;

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

                fResult += afP[iCol]*afQ[iRow]*afR[iSlice]*
                    m_aaafF[iZClamp][iYClamp][iXClamp];
            }
        }
    }

    return fResult;
}
//----------------------------------------------------------------------------
template <class Real>
Real IntpTricubic3<Real>::operator() (int iXOrder, int iYOrder, int iZOrder,
    Real fX, Real fY, Real fZ) const
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

    // compute z-index and clamp to image
    Real fZIndex = (fZ - m_fZMin)*m_fInvZSpacing;
    int iZ = (int)fZIndex;
    if ( iZ < 0 || iZ > m_iZBound - 1 )
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
        afU[3] = ((Real)6.0)*fDX;
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

    Real afW[4], fDZ, fZMult;
    switch ( iZOrder )
    {
    case 0:
        fDZ = fZIndex - iZ;
        afW[0] = (Real)1.0;
        afW[1] = fDZ;
        afW[2] = fDZ*afW[1];
        afW[3] = fDZ*afW[2];
        fZMult = (Real)1.0;
        break;
    case 1:
        fDZ = fZIndex - iZ;
        afW[0] = (Real)0.0;
        afW[1] = (Real)1.0;
        afW[2] = ((Real)2.0)*fDZ;
        afW[3] = ((Real)3.0)*fDZ*fDZ;
        fZMult = m_fInvZSpacing;
        break;
    case 2:
        fDZ = fZIndex - iZ;
        afW[0] = (Real)0.0;
        afW[1] = (Real)0.0;
        afW[2] = (Real)2.0;
        afW[3] = ((Real)6.0)*fDZ;
        fZMult = m_fInvZSpacing*m_fInvZSpacing;
        break;
    case 3:
        afW[0] = (Real)0.0;
        afW[1] = (Real)0.0;
        afW[2] = (Real)0.0;
        afW[3] = (Real)6.0;
        fZMult = m_fInvZSpacing*m_fInvZSpacing*m_fInvZSpacing;
        break;
    default:
        return (Real)0.0;
    }

    // compute P = M*U, Q = M*V, and R = M*W
    Real afP[4], afQ[4], afR[4];
    int iRow, iCol;
    for (iRow = 0; iRow < 4; iRow++)
    {
        afP[iRow] = (Real)0.0;
        afQ[iRow] = (Real)0.0;
        afR[iRow] = (Real)0.0;
        for (iCol = 0; iCol < 4; iCol++)
        {
            afP[iRow] += m_aafBlend[iRow][iCol]*afU[iCol];
            afQ[iRow] += m_aafBlend[iRow][iCol]*afV[iCol];
            afR[iRow] += m_aafBlend[iRow][iCol]*afW[iCol];
        }
    }

    // compute the tensor product (M*U)(M*V)(M*W)*D where D is the 4x4x4
    // subimage containing (x,y,z)
    iX--;
    iY--;
    iZ--;
    Real fResult = (Real)0.0;
    for (int iSlice = 0; iSlice < 4; iSlice++)
    {
        int iZClamp = iZ + iSlice;
        if ( iZClamp < 0 )
            iZClamp = 0;
        else if ( iZClamp > m_iZBound - 1 )
            iZClamp = m_iZBound - 1;

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

                fResult += afP[iCol]*afQ[iRow]*afR[iSlice]*
                    m_aaafF[iZClamp][iYClamp][iXClamp];
            }
        }
    }
    fResult *= fXMult*fYMult*fZMult;

    return fResult;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM IntpTricubic3<float>;
const float IntpTricubic3f::ms_aafCRBlend[4][4] =
{
    { 0.0f, -0.5f,  1.0f, -0.5f },
    { 1.0f,  0.0f, -2.5f,  1.5f },
    { 0.0f,  0.5f,  2.0f, -1.5f },
    { 0.0f,  0.0f, -0.5f,  0.5f }
};
const float IntpTricubic3f::ms_aafBSBlend[4][4] =
{
    { 1.0f/6.0f, -3.0f/6.0f,  3.0f/6.0f, -1.0f/6.0f },
    { 4.0f/6.0f,  0.0f/6.0f, -6.0f/6.0f,  3.0f/6.0f },
    { 1.0f/6.0f,  3.0f/6.0f,  3.0f/6.0f, -3.0f/6.0f },
    { 0.0f/6.0f,  0.0f/6.0f,  0.0f/6.0f,  1.0f/6.0f }
};

template class WML_ITEM IntpTricubic3<double>;
const double IntpTricubic3d::ms_aafCRBlend[4][4] =
{
    { 0.0, -0.5,  1.0, -0.5 },
    { 1.0,  0.0, -2.5,  1.5 },
    { 0.0,  0.5,  2.0, -1.5 },
    { 0.0,  0.0, -0.5,  0.5 }
};
const double IntpTricubic3d::ms_aafBSBlend[4][4] =
{
    { 1.0/6.0, -3.0/6.0,  3.0/6.0, -1.0/6.0 },
    { 4.0/6.0,  0.0/6.0, -6.0/6.0,  3.0/6.0 },
    { 1.0/6.0,  3.0/6.0,  3.0/6.0, -3.0/6.0 },
    { 0.0/6.0,  0.0/6.0,  0.0/6.0,  1.0/6.0 }
};
}
//----------------------------------------------------------------------------
