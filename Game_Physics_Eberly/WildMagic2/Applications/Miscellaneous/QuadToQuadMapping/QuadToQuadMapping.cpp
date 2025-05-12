// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlQuadToQuadTransforms.h"
#include "WmlImages.h"
using namespace Wml;

//----------------------------------------------------------------------------
void DoHmSqrToQuad (const Vector2f& rkP00, const Vector2f& rkP10,
    const Vector2f& rkP11, const Vector2f& rkP01)
{
    HmSqrToQuadf kMap(rkP00,rkP10,rkP11,rkP01);

    float fXMin, fXMax, fXRange, fYMin, fYMax, fYRange;
    fXMax = fXMin = rkP00.X();
    fYMax = fYMin = rkP00.Y();

    if ( rkP10.X() < fXMin ) fXMin = rkP10.X();
    if ( rkP10.X() > fXMax ) fXMax = rkP10.X();
    if ( rkP10.Y() < fYMin ) fYMin = rkP10.Y();
    if ( rkP10.Y() > fYMax ) fYMax = rkP10.Y();

    if ( rkP11.X() < fXMin ) fXMin = rkP11.X();
    if ( rkP11.X() > fXMax ) fXMax = rkP11.X();
    if ( rkP11.Y() < fYMin ) fYMin = rkP11.Y();
    if ( rkP11.Y() > fYMax ) fYMax = rkP11.Y();

    if ( rkP01.X() < fXMin ) fXMin = rkP01.X();
    if ( rkP01.X() > fXMax ) fXMax = rkP01.X();
    if ( rkP01.Y() < fYMin ) fYMin = rkP01.Y();
    if ( rkP01.Y() > fYMax ) fYMax = rkP01.Y();

    fXRange = fXMax-fXMin;
    fYRange = fYMax-fYMin;

    int iXBase, iYBase;
    if ( fXRange <= fYRange )
    {
        iYBase = 256;
        iXBase = (int)(256.0f*fXRange/fYRange);
        if ( iXBase % 2 )
            iXBase++;
    }
    else
    {
        iXBase = 256;
        iYBase = (int)(256.0f*fYRange/fXRange);
        if ( iYBase % 2 )
            iYBase++;
    }

    ImageUChar2D kQuad(iXBase,iYBase);
    kQuad = 255;

    int iX, iY, iXMap, iYMap;
    Vector2f kInput, kOutput;

    // transform columns of square
    for (iX = 0; iX < 256; iX += 16)
    {
        kInput.X() = iX/255.0f;
        for (iY = 0; iY < 256; iY++)
        {
            kInput.Y() = iY/255.0f;
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
            iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
            kQuad(iXMap,iYMap) = 0;
        }
    }

    // transform last column
    kInput.X() = 1.0f;
    for (iY = 0; iY < 256; iY++)
    {
        kInput.Y() = iY/255.0f;
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;
    }

    // transform rows of square
    for (iY = 0; iY < 256; iY += 16)
    {
        kInput.Y() = iY/255.0f;
        for (iX = 0; iX < 256; iX++)
        {
            kInput.X() = iX/255.0f;
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
            iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
            kQuad(iXMap,iYMap) = 0;
        }
    }

    // transform last row
    kInput.Y() = 1.0f;
    for (iX = 0; iX < 256; iX++)
    {
        kInput.X() = iX/255.0f;
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;
    }

    // transform diagonals of square
    for (iX = 0; iX < 256; iX++)
    {
        kInput.X() = iX/255.0f;
        kInput.Y() = kInput.X();
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;

        kInput.Y() = 1.0f-kInput.X();
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;
    }

    kQuad.Save("HmSqrToQuad.im");
}
//----------------------------------------------------------------------------
void DoBiSqrToQuad (const Vector2f& rkP00, const Vector2f& rkP10,
    const Vector2f& rkP11, const Vector2f& rkP01)
{
    BiSqrToQuadf kMap(rkP00,rkP10,rkP11,rkP01);

    float fXMin, fXMax, fXRange, fYMin, fYMax, fYRange;
    fXMax = fXMin = rkP00.X();
    fYMax = fYMin = rkP00.Y();

    if ( rkP10.X() < fXMin ) fXMin = rkP10.X();
    if ( rkP10.X() > fXMax ) fXMax = rkP10.X();
    if ( rkP10.Y() < fYMin ) fYMin = rkP10.Y();
    if ( rkP10.Y() > fYMax ) fYMax = rkP10.Y();

    if ( rkP11.X() < fXMin ) fXMin = rkP11.X();
    if ( rkP11.X() > fXMax ) fXMax = rkP11.X();
    if ( rkP11.Y() < fYMin ) fYMin = rkP11.Y();
    if ( rkP11.Y() > fYMax ) fYMax = rkP11.Y();

    if ( rkP01.X() < fXMin ) fXMin = rkP01.X();
    if ( rkP01.X() > fXMax ) fXMax = rkP01.X();
    if ( rkP01.Y() < fYMin ) fYMin = rkP01.Y();
    if ( rkP01.Y() > fYMax ) fYMax = rkP01.Y();

    fXRange = fXMax-fXMin;
    fYRange = fYMax-fYMin;

    int iXBase, iYBase;
    if ( fXRange <= fYRange )
    {
        iYBase = 256;
        iXBase = (int)(256.0f*fXRange/fYRange);
        if ( iXBase % 2 )
            iXBase++;
    }
    else
    {
        iXBase = 256;
        iYBase = (int)(256.0f*fYRange/fXRange);
        if ( iYBase % 2 )
            iYBase++;
    }

    ImageUChar2D kQuad(iXBase,iYBase);
    kQuad = 255;

    int iX, iY, iXMap, iYMap;
    Vector2f kInput, kOutput;

    // transform columns of square
    for (iX = 0; iX < 256; iX += 16)
    {
        kInput.X() = iX/255.0f;
        for (iY = 0; iY < 256; iY++)
        {
            kInput.Y() = iY/255.0f;
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
            iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
            kQuad(iXMap,iYMap) = 0;
        }
    }

    // transform last column
    kInput.X() = 1.0f;
    for (iY = 0; iY < 256; iY++)
    {
        kInput.Y() = iY/255.0f;
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;
    }

    // transform rows of square
    for (iY = 0; iY < 256; iY += 16)
    {
        kInput.Y() = iY/255.0f;
        for (iX = 0; iX < 256; iX++)
        {
            kInput.X() = iX/255.0f;
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
            iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
            kQuad(iXMap,iYMap) = 0;
        }
    }

    // transform last row
    kInput.Y() = 1.0f;
    for (iX = 0; iX < 256; iX++)
    {
        kInput.X() = iX/255.0f;
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;
    }

    // transform diagonals of square
    for (iX = 0; iX < 256; iX++)
    {
        kInput.X() = iX/255.0f;
        kInput.Y() = kInput.X();
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;

        kInput.Y() = 1.0f-kInput.X();
        kOutput = kMap.Transform(kInput);
        iXMap = int((iXBase-1)*(kOutput.X()-fXMin)/fXRange);
        iYMap = int((iYBase-1)*(kOutput.Y()-fYMin)/fYRange);
        kQuad(iXMap,iYMap) = 0;
    }

    kQuad.Save("BiSqrToQuad.im");
}
//----------------------------------------------------------------------------
void DoHmQuadToSqr (const Vector2f& rkP00, const Vector2f& rkP10,
    const Vector2f& rkP11, const Vector2f& rkP01)
{
    HmQuadToSqrf kMap(rkP00,rkP10,rkP11,rkP01);

    float fXMin, fXMax, fXRange, fYMin, fYMax, fYRange;
    fXMax = fXMin = rkP00.X();
    fYMax = fYMin = rkP00.Y();

    if ( rkP10.X() < fXMin ) fXMin = rkP10.X();
    if ( rkP10.X() > fXMax ) fXMax = rkP10.X();
    if ( rkP10.Y() < fYMin ) fYMin = rkP10.Y();
    if ( rkP10.Y() > fYMax ) fYMax = rkP10.Y();

    if ( rkP11.X() < fXMin ) fXMin = rkP11.X();
    if ( rkP11.X() > fXMax ) fXMax = rkP11.X();
    if ( rkP11.Y() < fYMin ) fYMin = rkP11.Y();
    if ( rkP11.Y() > fYMax ) fYMax = rkP11.Y();

    if ( rkP01.X() < fXMin ) fXMin = rkP01.X();
    if ( rkP01.X() > fXMax ) fXMax = rkP01.X();
    if ( rkP01.Y() < fYMin ) fYMin = rkP01.Y();
    if ( rkP01.Y() > fYMax ) fYMax = rkP01.Y();

    fXRange = fXMax-fXMin;
    fYRange = fYMax-fYMin;

    int iXBase = 256, iYBase = 256;
    ImageUChar2D kSquare(iXBase,iYBase);
    kSquare = 255;

    int iX, iY, iXMap, iYMap;
    Vector2f kInput, kOutput;

    // transform columns
    const int iYSample = 1000;
    for (iX = 0; iX < 16; iX++)
    {
        kInput.X() = fXMin+fXRange*iX/15.0f;
        for (iY = 0; iY < iYSample; iY++)
        {
            kInput.Y() = fYMin+fYRange*iY/(iYSample-1.0f);
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*kOutput.X());
            iYMap = int((iYBase-1)*kOutput.Y());
            if ( 0 <= iXMap && iXMap < iXBase
            &&   0 <= iYMap && iYMap < iYBase )
            {
                kSquare(iXMap,iYMap) = 0;
            }
        }
    }

    // transform rows
    const int iXSample = 1000;
    for (iY = 0; iY < 16; iY++)
    {
        kInput.Y() = fYMin+fYRange*iY/15.0f;
        for (iX = 0; iX < iXSample; iX++)
        {
            kInput.X() = fXMin+fXRange*iX/(iXSample-1.0f);
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*kOutput.X());
            iYMap = int((iYBase-1)*kOutput.Y());
            if ( 0 <= iXMap && iXMap < iXBase
            &&   0 <= iYMap && iYMap < iYBase )
            {
                kSquare(iXMap,iYMap) = 0;
            }
        }
    }

    kSquare.Save("HmQuadToSqr.im");
}
//----------------------------------------------------------------------------
void DoBiQuadToSqr (const Vector2f& rkP00, const Vector2f& rkP10,
    const Vector2f& rkP11, const Vector2f& rkP01)
{
    BiQuadToSqrf kMap(rkP00,rkP10,rkP11,rkP01);

    float fXMin, fXMax, fXRange, fYMin, fYMax, fYRange;
    fXMax = fXMin = rkP00.X();
    fYMax = fYMin = rkP00.Y();

    if ( rkP10.X() < fXMin ) fXMin = rkP10.X();
    if ( rkP10.X() > fXMax ) fXMax = rkP10.X();
    if ( rkP10.Y() < fYMin ) fYMin = rkP10.Y();
    if ( rkP10.Y() > fYMax ) fYMax = rkP10.Y();

    if ( rkP11.X() < fXMin ) fXMin = rkP11.X();
    if ( rkP11.X() > fXMax ) fXMax = rkP11.X();
    if ( rkP11.Y() < fYMin ) fYMin = rkP11.Y();
    if ( rkP11.Y() > fYMax ) fYMax = rkP11.Y();

    if ( rkP01.X() < fXMin ) fXMin = rkP01.X();
    if ( rkP01.X() > fXMax ) fXMax = rkP01.X();
    if ( rkP01.Y() < fYMin ) fYMin = rkP01.Y();
    if ( rkP01.Y() > fYMax ) fYMax = rkP01.Y();

    fXRange = fXMax-fXMin;
    fYRange = fYMax-fYMin;

    int iXBase = 256, iYBase = 256;
    ImageUChar2D kSquare(iXBase,iYBase);
    kSquare = 255;

    int iX, iY, iXMap, iYMap;
    Vector2f kInput, kOutput;

    // transform columns
    const int iYSample = 1000;
    for (iX = 0; iX < 16; iX++)
    {
        kInput.X() = fXMin+fXRange*iX/15.0f;
        for (iY = 0; iY < iYSample; iY++)
        {
            kInput.Y() = fYMin+fYRange*iY/(iYSample-1.0f);
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*kOutput.X());
            iYMap = int((iYBase-1)*kOutput.Y());
            if ( 0 <= iXMap && iXMap < iXBase
            &&   0 <= iYMap && iYMap < iYBase )
            {
                kSquare(iXMap,iYMap) = 0;
            }
        }
    }

    // transform rows
    const int iXSample = 1000;
    for (iY = 0; iY < 16; iY++)
    {
        kInput.Y() = fYMin+fYRange*iY/15.0f;
        for (iX = 0; iX < iXSample; iX++)
        {
            kInput.X() = fXMin+fXRange*iX/(iXSample-1.0f);
            kOutput = kMap.Transform(kInput);
            iXMap = int((iXBase-1)*kOutput.X());
            iYMap = int((iYBase-1)*kOutput.Y());
            if ( 0 <= iXMap && iXMap < iXBase
            &&   0 <= iYMap && iYMap < iYBase )
            {
                kSquare(iXMap,iYMap) = 0;
            }
        }
    }

    kSquare.Save("BiQuadToSqr.im");
}
//----------------------------------------------------------------------------
int main ()
{
    Vector2f kP00(1.0f,1.0f);
    Vector2f kP10(2.0f,1.0f);
    Vector2f kP11(4.0f,3.0f);
    Vector2f kP01(1.0f,2.0f);

    DoHmSqrToQuad(kP00,kP10,kP11,kP01);
    DoBiSqrToQuad(kP00,kP10,kP11,kP01);
    DoHmQuadToSqr(kP00,kP10,kP11,kP01);
    DoBiQuadToSqr(kP00,kP10,kP11,kP01);

    return 0;
}
//----------------------------------------------------------------------------
