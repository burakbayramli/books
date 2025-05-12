// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLWGLRENDERER_H
#define WMLWGLRENDERER_H

#include "WmlOpenGLRenderer.h"
#include <map>

namespace Wml
{

class WML_RENDERER_ITEM WglRenderer : public OpenGLRenderer
{
    WmlDeclareRTTI;

public:
    // construction and destruction
    WglRenderer (HWND hWnd, int iWidth, int iHeight);
    virtual ~WglRenderer ();

    virtual void DisplayBackBuffer ();
    virtual void Draw (int iX, int iY, const ColorRGB& rkColor,
        const char* acText);
    virtual bool LoadFont (const RendererFont& rkFont);
    virtual void UnloadFont (const RendererFont& rkFont);
    virtual void Draw (int iX, int iY, const RendererFont& rkFont,
        const char* acText);

    // Test if a specified extension is supported.  If you need to test for
    // multiple extensions, the function must be called for each extension.
    // The extensions could vary per rendering context, so check for
    // extensions immediately after setting the context.
    static bool ExtensionSupported (const char* acExt);

protected:
    class WML_RENDERER_ITEM FontDesc
    {
    public:
        RendererFont kFont;
        int iListStart;
        int iNumChars;
        int iListBase;
    };

    // window parameters
    HWND m_hWnd;
    HDC m_hWindowDC;
    HGLRC m_hWindowRC;
    FontDesc m_kDefaultFont;
    std::map<unsigned int,FontDesc*> m_kFontMap;
};

WmlSmartPointer(WglRenderer);

}

#endif
