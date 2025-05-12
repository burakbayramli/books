// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLAPPLICATION2_H
#define WMLAPPLICATION2_H

#include "WmlApplication.h"
using namespace Wml;

class Application2 : public Application
{
public:
    Application2 (char* acWindowTitle, int iXPos, int iYPos, int iWidth,
        int iHeight, const ColorRGB& rkBackgroundColor, bool bUseCLI = true);

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnReshape (int iWidth, int iHeight);

    // Allows you to do additional drawing after the screen polygon is drawn.
    virtual void ScreenOverlay ();

    void ClearScreen ();

    class Color
    {
    public:
        Color (unsigned char ucR=0, unsigned char ucG=0, unsigned char ucB=0)
        {
            r = ucR;
            g = ucG;
            b = ucB;
        }

        // BGR ordering that Microsoft Windows uses for DirectX.  OpenGL is
        // told to use GL_BGR_EXT to match this ordering.
        unsigned char b, g, r;
    };

    void SetPixel (int iX, int iY, Color kColor);
    Color GetPixel (int iX, int iY);
    void DrawLine (int iX0, int iY0, int iX1, int iY1, Color kColor);
    void DrawRectangle (int iXMin, int iYMin, int iXMax, int iYMax,
        Color kColor, bool bSolid = false);
    void DrawCircle (int iXCenter, int iYCenter, int iRadius, Color kColor,
        bool bSolid = false);

    bool& ClampToWindow ();

    // For right-handed drawing.  You still draw to the left-handed screen,
    // but immediately before drawing the screen is copied into another buffer
    // with the rows reversed.  You need only call DoFlip(true) once for an
    // application.  The default is 'false'.
    void DoFlip (bool bDoFlip);
    
protected:
    static int Index (int iX, int iY)
    {
        // left-handed screen coordinates
        return iX + ms_iWidth*iY;
    }

    int m_iScrWidth, m_iScrHeight;
    Color* m_akScreen;
    bool m_bClampToWindow;

    // For right-handed drawing.  The array m_akScreen is copied to
    // m_akFlipScreen so that the rows are reversed.
    Color* m_akFlipScreen;
};

#endif
