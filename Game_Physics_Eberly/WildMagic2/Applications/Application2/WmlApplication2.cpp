// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlApplication2.h"

//----------------------------------------------------------------------------
Application2::Application2 (char* acWindowTitle, int iXPos, int iYPos,
    int iWidth, int iHeight, const ColorRGB& rkBackgroundColor, bool bUseCLI)
    :
    Application(
        acWindowTitle,
        iXPos,
        iYPos,
        iWidth - (iWidth % 4),  // rows required to be multiple of 4 bytes 
        iHeight,
        rkBackgroundColor,
        bUseCLI)
{
    m_iScrWidth = 0;
    m_iScrHeight = 0;
    m_akScreen = NULL;
    m_bClampToWindow = true;
    m_akFlipScreen = NULL;
}
//----------------------------------------------------------------------------
bool Application2::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // the RGB screen pixels
    m_iScrWidth = GetWidth();
    m_iScrHeight = GetHeight();
    m_akScreen = new Color[m_iScrWidth*m_iScrHeight];
    ClearScreen();
    return true;
}
//----------------------------------------------------------------------------
void Application2::OnTerminate ()
{
    delete[] m_akScreen;
    delete[] m_akFlipScreen;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Application2::OnDisplay ()
{
    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( !m_akFlipScreen )
        {
            ms_spkRenderer->Draw((const unsigned char*)m_akScreen);
        }
        else
        {
            // flip the screen
            Color* akSPtr = m_akScreen;
            Color* akFPtr = m_akFlipScreen + m_iScrWidth*(m_iScrHeight-1);
            int iQuantity = m_iScrWidth*sizeof(Color);
            for (int i = 0; i < m_iScrHeight; i++)
            {
                memcpy(akFPtr,akSPtr,iQuantity);
                akSPtr += m_iScrWidth;
                akFPtr -= m_iScrWidth;
            }

            ms_spkRenderer->Draw((const unsigned char*)m_akFlipScreen);
        }

        // Screen overlays should use ms_spkRenderer and not access the
        // m_akScreen array directly.
        ScreenOverlay();

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
void Application2::OnReshape (int iWidth, int iHeight)
{
    iWidth = iWidth - (iWidth % 4);
    Application::OnReshape(iWidth,iHeight);
    if ( iWidth*iHeight <= 0 )
        return;

    if ( iWidth != m_iScrWidth || iHeight != m_iScrHeight )
    {
        delete[] m_akScreen;
        m_iScrWidth = iWidth;
        m_iScrHeight = iHeight;
        m_akScreen = new Color[m_iScrWidth*m_iScrHeight];
        ClearScreen();

        if ( m_akFlipScreen )
        {
            delete[] m_akFlipScreen;
            m_akFlipScreen = new Color[m_iScrWidth*m_iScrHeight];
        }
    }
}
//----------------------------------------------------------------------------
void Application2::ScreenOverlay ()
{
    // stub for derived classes
}
//----------------------------------------------------------------------------
void Application2::ClearScreen ()
{
    for (int i = 0; i < ms_iWidth*ms_iHeight; i++)
    {
        m_akScreen[i].r = (unsigned char)(255.0f*ms_kBackgroundColor.r);
        m_akScreen[i].g = (unsigned char)(255.0f*ms_kBackgroundColor.g);
        m_akScreen[i].b = (unsigned char)(255.0f*ms_kBackgroundColor.b);
    }
}
//----------------------------------------------------------------------------
bool& Application2::ClampToWindow ()
{
    return m_bClampToWindow;
}
//----------------------------------------------------------------------------
void Application2::SetPixel (int iX, int iY, Color kColor)
{
    if ( m_bClampToWindow )
    {
        if ( 0 <= iX && iX < ms_iWidth && 0 <= iY && iY < ms_iHeight )
            m_akScreen[Index(iX,iY)] = kColor;
    }
    else
    {
        m_akScreen[Index(iX,iY)] = kColor;
    }
}
//----------------------------------------------------------------------------
Application2::Color Application2::GetPixel (int iX, int iY)
{
    if ( m_bClampToWindow )
    {
        if ( 0 <= iX && iX < ms_iWidth && 0 <= iY && iY < ms_iHeight )
            return m_akScreen[Index(iX,iY)];
        else
            return Color(0,0,0);
    }
    else
    {
        return m_akScreen[Index(iX,iY)];
    }
}
//----------------------------------------------------------------------------
void Application2::DrawLine (int iX0, int iY0, int iX1, int iY1, Color kColor)
{
    int iX = iX0, iY = iY0;

    // direction of line
    int iDx = iX1-iX0, iDy = iY1-iY0;

    // increment or decrement depending on direction of line
    int iSx = (iDx > 0 ? 1 : (iDx < 0 ? -1 : 0));
    int iSy = (iDy > 0 ? 1 : (iDy < 0 ? -1 : 0));

    // decision parameters for voxel selection
    if ( iDx < 0 ) iDx = -iDx;
    if ( iDy < 0 ) iDy = -iDy;
    int iAx = 2*iDx, iAy = 2*iDy;
    int iDecX, iDecY;

    // determine largest direction component, single-step related variable
    int iMax = iDx, iVar = 0;
    if ( iDy > iMax ) { iVar = 1; }

    // traverse Bresenham line
    switch ( iVar )
    {
    case 0:  // single-step in x-direction
        iDecY = iAy - iDx;
        for (/**/; /**/; iX += iSx, iDecY += iAy)
        {
            // process pixel
            SetPixel(iX,iY,kColor);

            // take Bresenham step
            if ( iX == iX1 )  break;
            if ( iDecY >= 0 ) { iDecY -= iAx; iY += iSy; }
        }
        break;
    case 1:  // single-step in y-direction
        iDecX = iAx - iDy;
        for (/**/; /**/; iY += iSy, iDecX += iAx)
        {
            // process pixel
            SetPixel(iX,iY,kColor);

            // take Bresenham step
            if ( iY == iY1 ) break;
            if ( iDecX >= 0 ) { iDecX -= iAy; iX += iSx; }
        }
        break;
    }
}
//----------------------------------------------------------------------------
void Application2::DrawRectangle (int iXMin, int iYMin, int iXMax, int iYMax,
    Color kColor, bool bSolid)
{
    if ( iXMin >= ms_iWidth || iXMax < 0
    ||   iYMin >= ms_iHeight || iYMax < 0 )
    {
        // rectangle not visible
        return;
    }

    // clamp to window border
    if ( iXMin < 0 )
        iXMin = 0;
    if ( iXMax >= ms_iWidth )
        iXMax = ms_iWidth-1;
    if ( iYMin < 0 )
        iYMin = 0;
    if ( iYMax >= ms_iHeight )
        iYMax = ms_iHeight-1;

    int iX, iY;

    if ( bSolid )
    {
        for (iY = iYMin; iY <= iYMax; iY++)
        {
            for (iX = iXMin; iX <= iXMax; iX++)
                m_akScreen[Index(iX,iY)] = kColor;
        }
    }
    else
    {
        for (iX = iXMin; iX <= iXMax; iX++)
        {
            m_akScreen[Index(iX,iYMin)] = kColor;
            m_akScreen[Index(iX,iYMax)] = kColor;
        }
        for (iY = iYMin+1; iY <= iYMax-1; iY++)
        {
            m_akScreen[Index(iXMin,iY)] = kColor;
            m_akScreen[Index(iXMax,iY)] = kColor;
        }
    }
}
//----------------------------------------------------------------------------
void Application2::DrawCircle (int iXCenter, int iYCenter, int iRadius,
    Color kColor, bool bSolid)
{
    int iX, iY, iDec;

    if ( bSolid )
    {
        int iXValue, iYMin, iYMax, i;
        for (iX = 0, iY = iRadius, iDec = 3-2*iRadius; iX <= iY; iX++)
        {
            iXValue = iXCenter + iX;
            iYMin = iYCenter - iY;
            iYMax = iYCenter + iY;
            for (i = iYMin; i <= iYMax; i++)
                m_akScreen[Index(iXValue,i)] = kColor;

            iXValue = iXCenter - iX;
            for (i = iYMin; i <= iYMax; i++)
                m_akScreen[Index(iXValue,i)] = kColor;

            iXValue = iXCenter + iY;
            iYMin = iYCenter - iX;
            iYMax = iYCenter + iX;
            for (i = iYMin; i <= iYMax; i++)
                m_akScreen[Index(iXValue,i)] = kColor;

            iXValue = iXCenter - iY;
            for (i = iYMin; i <= iYMax; i++)
                m_akScreen[Index(iXValue,i)] = kColor;

            if ( iDec >= 0 )
                iDec += -4*(iY--)+4;
            iDec += 4*iX+6;
        }
    }
    else
    {
        for (iX = 0, iY = iRadius, iDec = 3-2*iRadius; iX <= iY; iX++)
        {
            m_akScreen[Index(iXCenter+iX,iYCenter+iY)] = kColor;
            m_akScreen[Index(iXCenter+iX,iYCenter-iY)] = kColor;
            m_akScreen[Index(iXCenter-iX,iYCenter+iY)] = kColor;
            m_akScreen[Index(iXCenter-iX,iYCenter-iY)] = kColor;
            m_akScreen[Index(iXCenter+iY,iYCenter+iX)] = kColor;
            m_akScreen[Index(iXCenter+iY,iYCenter-iX)] = kColor;
            m_akScreen[Index(iXCenter-iY,iYCenter+iX)] = kColor;
            m_akScreen[Index(iXCenter-iY,iYCenter-iX)] = kColor;

            if ( iDec >= 0 )
                iDec += -4*(iY--)+4;
            iDec += 4*iX+6;
        }
    }
}
//----------------------------------------------------------------------------
void Application2::DoFlip (bool bDoFlip)
{
    if ( m_akFlipScreen )
    {
        if ( !bDoFlip )
        {
            delete[] m_akFlipScreen;
            m_akFlipScreen = NULL;
        }
    }
    else
    {
        if ( bDoFlip )
            m_akFlipScreen = new Color[m_iScrWidth*m_iScrHeight];
    }
}
//----------------------------------------------------------------------------
