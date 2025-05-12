// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLGLUTRENDERER_H
#define WMLGLUTRENDERER_H

#include "WmlOpenGLRenderer.h"

namespace Wml
{

class WML_RENDERER_ITEM GlutRenderer : public OpenGLRenderer
{
    WmlDeclareRTTI;

public:
    // construction and destruction
    GlutRenderer (int iWindowID, int iWidth, int iHeight);

    void Activate ();
    int GetWindowID () const;

    virtual void DisplayBackBuffer ();
    virtual void Draw (int iX, int iY, const ColorRGB& rkColor,
        const char* acText);

    // TO DO. Alternate font handling has not yet been added to the GLUT
    // renderer.  For now the Load/Unload do nothing.  The Draw function just
    // calls the Draw function above using ColorRGB::WHITE as the color.
    virtual bool LoadFont (const RendererFont& rkFont);
    virtual void UnloadFont (const RendererFont& rkFont);
    virtual void Draw (int iX, int iY, const RendererFont& rkFont,
        const char* acText);

protected:
    // GLUT identifier for the window of the renderer
    int m_iWindowID;
};

WmlSmartPointer(GlutRenderer);

}

#endif
