// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// Conditional compilation allows GLUT and WGL to be compiled from within
// the same project, but using different build configurations.  CodeWarrior
// and ProjectBuilder on the Macintosh allow such selection within the project
// itself, but not DeveloperStudio on the PC.
#ifdef WML_USE_WGL

#include "WmlWglRenderer.h"
using namespace Wml;
using namespace std;

WmlImplementRTTI(WglRenderer,OpenGLRenderer);

//----------------------------------------------------------------------------
WglRenderer::WglRenderer (HWND hWnd, int iWidth, int iHeight)
    :
    OpenGLRenderer(iWidth,iHeight)
{
    assert( m_iWidth > 0 && m_iHeight > 0 );

    m_hWnd = hWnd;
    m_hWindowDC = GetDC(m_hWnd);

    // select format for drawing surface
    PIXELFORMATDESCRIPTOR kPFD;
    memset(&kPFD,0,sizeof(PIXELFORMATDESCRIPTOR));
    kPFD.nSize = sizeof(PIXELFORMATDESCRIPTOR);
    kPFD.nVersion = 1;
    kPFD.dwFlags =
        PFD_DRAW_TO_WINDOW |
        PFD_SUPPORT_OPENGL |
        PFD_GENERIC_ACCELERATED |
        PFD_DOUBLEBUFFER;
    kPFD.iPixelType = PFD_TYPE_RGBA;
    kPFD.cColorBits = 24;  // 24-bit colors for front/back buffers
    kPFD.cDepthBits = 16;  // 16-bit depth buffer
    kPFD.cStencilBits = 8; // 8-bit stencil buffer

    // set the pixel format for the rendering context
    int iPixelFormat = ChoosePixelFormat(m_hWindowDC,&kPFD);
    if ( iPixelFormat == 0 )
        return;

    BOOL bSuccess = SetPixelFormat(m_hWindowDC,iPixelFormat,&kPFD);
    if ( !bSuccess )
        return;

    // create an OpenGL context
    m_hWindowRC = wglCreateContext(m_hWindowDC);
    if ( !m_hWindowRC )
        return;

    bSuccess = wglMakeCurrent(m_hWindowDC,m_hWindowRC);
    if ( !bSuccess )
        return;

    // we require at least OpenGL 1.1
    if ( !IsMinimumVersion(1,1,0) )
    {
        assert( false );
        return;
    }

    // preload font by generating display lists for font chars
    int iFirstChar = int(' ');
    m_kDefaultFont.iNumChars = 127 - iFirstChar;
    m_kDefaultFont.iListStart = glGenLists(m_kDefaultFont.iNumChars) ;
    m_kDefaultFont.iListBase = m_kDefaultFont.iListStart - iFirstChar;

    // use the system font as default
    SelectObject(m_hWindowDC,GetStockObject(SYSTEM_FONT)); 

    // create the bitmap display lists 
    wglUseFontBitmaps(m_hWindowDC,iFirstChar,m_kDefaultFont.iNumChars,
        m_kDefaultFont.iListStart); 

    EstablishCapabilities();
    InitializeState();
}
//----------------------------------------------------------------------------
WglRenderer::~WglRenderer ()
{
    // release the bitmap display lists for default font
    glDeleteLists(m_kDefaultFont.iListStart,m_kDefaultFont.iNumChars);

    map<unsigned int,FontDesc*>::iterator pkIter;
    for (pkIter = m_kFontMap.begin(); pkIter != m_kFontMap.end(); pkIter++)
    {
        FontDesc* pkDesc = pkIter->second;
        glDeleteLists(pkDesc->iListStart,pkDesc->iNumChars);
        delete pkDesc;
    }

    if ( m_hWindowRC )
        wglDeleteContext(m_hWindowRC);

    if ( m_hWindowDC )
        ReleaseDC(m_hWnd,m_hWindowDC);
}
//----------------------------------------------------------------------------
void WglRenderer::DisplayBackBuffer ()
{
    glFlush();
    SwapBuffers(m_hWindowDC);
}
//----------------------------------------------------------------------------
bool WglRenderer::LoadFont (const RendererFont& rkFont)
{
    int iWeight;
    
    if ( rkFont.Bold() )
        iWeight = FW_BOLD;
    else
        iWeight = FW_REGULAR;
        
    HFONT hFont = ::CreateFont(rkFont.Size(),0,0,0,iWeight,
        (DWORD)rkFont.Italic(),FALSE,FALSE,DEFAULT_CHARSET,
        OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,ANTIALIASED_QUALITY,
        VARIABLE_PITCH,rkFont.Face().c_str());
    
    if ( hFont == NULL )
        return false;

    FontDesc* pkDesc = new FontDesc;
    pkDesc->kFont = rkFont;
    int iFirstChar = int(' ');
    pkDesc->iNumChars = 127 - iFirstChar;
    pkDesc->iListStart = glGenLists(pkDesc->iNumChars) ;
    pkDesc->iListBase = pkDesc->iListStart - iFirstChar;

    m_kFontMap[rkFont.GetID()] = pkDesc;

    SelectObject(m_hWindowDC,hFont); 

    // create the bitmap display lists 
    wglUseFontBitmaps(m_hWindowDC,iFirstChar,pkDesc->iNumChars,
        pkDesc->iListStart); 

    return true;
}
//----------------------------------------------------------------------------
void WglRenderer::UnloadFont (const RendererFont& rkFont)
{
    map<unsigned int,FontDesc*>::iterator pkIter =
        m_kFontMap.find(rkFont.GetID());

    if ( pkIter == m_kFontMap.end() )
        return;

    FontDesc* pkDesc = pkIter->second;
    glDeleteLists(pkDesc->iListStart,pkDesc->iNumChars);
    delete pkDesc;
    m_kFontMap.erase(pkIter);
}
//----------------------------------------------------------------------------
void WglRenderer::Draw (int iX, int iY, const ColorRGB& rkColor,
    const char* acText)
{
    assert( acText );

    // unset shaders
    SetShaderState(NULL);

    // switch to orthogonal view
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-0.5f,m_iWidth-0.5f,-0.5f,m_iHeight-0.5f,-1.0f,1.0f);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();

    // disable depth, lighting, and texturing
    GLboolean bDepthTest = glIsEnabled(GL_DEPTH_TEST);
    GLboolean bLighting = glIsEnabled(GL_LIGHTING);
    GLboolean bTexture2D = glIsEnabled(GL_TEXTURE_2D);
    if ( bDepthTest )
        glDisable(GL_DEPTH_TEST);
    if ( bLighting )
        glDisable(GL_LIGHTING);
    if ( bTexture2D )
        glDisable(GL_TEXTURE_2D);

    // set the text color
    glColor3f(rkColor.r,rkColor.g,rkColor.b);

    // draw text string (use right-handed coordinates)
    glRasterPos3i(iX,m_iHeight-1-iY,0);
    glListBase(m_kDefaultFont.iListBase);
    glCallLists((GLsizei)strlen(acText),GL_UNSIGNED_BYTE,acText);

    // restore depth, lighting, and texturing
    if ( bDepthTest )
        glEnable(GL_DEPTH_TEST);
    if ( bLighting )
        glEnable(GL_LIGHTING);
    if ( bTexture2D )
        glEnable(GL_TEXTURE_2D);

    // restore matrices
    glPopMatrix();
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);

    // restore projective view
    m_spkCamera->Update();
}
//----------------------------------------------------------------------------
void WglRenderer::Draw (int iX, int iY, const RendererFont& rkFont,
    const char* acText)
{
    assert( acText );
    
    map<unsigned int,FontDesc*>::iterator pkIter =
        m_kFontMap.find(rkFont.GetID());

    if ( pkIter == m_kFontMap.end() )
        return;

    // unset shaders
    SetShaderState(NULL);

    FontDesc* pkDesc = (FontDesc*)pkIter->second;

    // switch to orthogonal view
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-0.5f,m_iWidth-0.5f,-0.5f,m_iHeight-0.5f,-1.0f,1.0f);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();

    // disable depth, lighting, and texturing
    GLboolean bDepthTest = glIsEnabled(GL_DEPTH_TEST);
    GLboolean bLighting = glIsEnabled(GL_LIGHTING);
    GLboolean bTexture2D = glIsEnabled(GL_TEXTURE_2D);
    if ( bDepthTest )
        glDisable(GL_DEPTH_TEST);
    if ( bLighting )
        glDisable(GL_LIGHTING);
    if ( bTexture2D )
        glDisable(GL_TEXTURE_2D);

    // set the text color
    ColorRGB kColor = rkFont.Color();
    glColor3f(kColor.r,kColor.g,kColor.b);

    // draw text string (use right-handed coordinates)
    glRasterPos2i(iX,m_iHeight-1-iY);
    glListBase(pkDesc->iListBase);
    glCallLists((GLsizei)strlen(acText),GL_UNSIGNED_BYTE,acText);

    // restore depth, lighting, and texturing
    if ( bDepthTest )
        glEnable(GL_DEPTH_TEST);
    if ( bLighting )
        glEnable(GL_LIGHTING);
    if ( bTexture2D )
        glEnable(GL_TEXTURE_2D);

    // restore matrices
    glPopMatrix();
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);

    // restore projective view
    m_spkCamera->Update();
}
//----------------------------------------------------------------------------
// extension testing
//----------------------------------------------------------------------------
bool WglRenderer::ExtensionSupported (const char* acExt)
{
    if ( OpenGLRenderer::ExtensionSupported(acExt) )
        return true;

    if ( wglGetProcAddress("wglGetExtensionsStringARB") )
    {
        // This call is supported by glprocs.h.
        const char* acExtensions = 
            wglGetExtensionsStringARB(wglGetCurrentDC());

        char* acBegin = strstr(acExtensions,acExt);
        if ( acBegin )
        {
            // The extension was found, but make sure it is not a proper
            // substring of another extension.  TO DO:  Should cEnd be
            // compared to tabs or other white space characters?
            char cEnd = *(acBegin + strlen(acExt));
            return cEnd == ' ' || cEnd == 0;
        }
    }

    return false;
}
//----------------------------------------------------------------------------

#endif
