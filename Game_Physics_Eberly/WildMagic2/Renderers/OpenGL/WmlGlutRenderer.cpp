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
#ifdef WML_USE_GLUT

#include "WmlGlutRenderer.h"
using namespace Wml;

WmlImplementRTTI(GlutRenderer,OpenGLRenderer);

//----------------------------------------------------------------------------
GlutRenderer::GlutRenderer (int iWindowID, int iWidth, int iHeight)
    :
    OpenGLRenderer(iWidth,iHeight)
{
    m_iWindowID = iWindowID;
    m_iWidth = iWidth;
    m_iHeight = iHeight;
    m_iQuantity = m_iWidth*m_iHeight;

    // we require at least OpenGL 1.1
    if ( !IsMinimumVersion(1,1,0) )
    {
        assert( false );
        return;
    }

    EstablishCapabilities();
    InitializeState();
}
//----------------------------------------------------------------------------
void GlutRenderer::Activate ()
{
    glutSetWindow(m_iWindowID);
}
//----------------------------------------------------------------------------
int GlutRenderer::GetWindowID () const
{
    return m_iWindowID;
}
//----------------------------------------------------------------------------
void GlutRenderer::DisplayBackBuffer ()
{
    glutSwapBuffers();
}
//----------------------------------------------------------------------------
void GlutRenderer::Draw (int iX, int iY, const ColorRGB& rkColor,
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
    glRasterPos2i(iX,m_iHeight-1-iY);
    int iLength = (int)strlen(acText);
    for (int i = 0; i < iLength; i++)
        glutBitmapCharacter(GLUT_BITMAP_8_BY_13,acText[i]);

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
bool GlutRenderer::LoadFont (const RendererFont&)
{
    // TO DO.
    return false;
}
//----------------------------------------------------------------------------
void GlutRenderer::UnloadFont (const RendererFont&)
{
    // TO DO.
}
//----------------------------------------------------------------------------
void GlutRenderer::Draw (int iX, int iY, const RendererFont&,
    const char* acText)
{
    // TO DO.  Once font support is added, modify this to use the input font.
    Draw(iX,iY,ColorRGB::WHITE,acText);
}
//----------------------------------------------------------------------------

#endif
