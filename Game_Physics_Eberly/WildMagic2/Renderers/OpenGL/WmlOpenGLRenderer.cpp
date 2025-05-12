// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlOpenGLRenderer.h"
#include "WmlBumpMap.h"
#include "WmlParticles.h"
#include "WmlPolyline.h"
#include "WmlPolypoint.h"
#include "WmlScreenPolygon.h"
#include "WmlTriMesh.h"
using namespace Wml;

#include <string>
using namespace std;

WmlImplementRTTI(OpenGLRenderer,Renderer);

//----------------------------------------------------------------------------
OpenGLRenderer::OpenGLRenderer (int iWidth, int iHeight)
    :
    Renderer(iWidth,iHeight)
{
}
//----------------------------------------------------------------------------
void OpenGLRenderer::EstablishCapabilities ()
{
     bool bOnePointTwo = IsMinimumVersion(1,2,0);
     bool bOnePointThree = IsMinimumVersion(1,3,0);

     // specular highlights drawn untinted after the texture color
     m_bCapSpecularAfterTexture = bOnePointTwo
         || ExtensionSupported("GL_EXT_separate_specular_color");

     // initialize the number of supported texture units
     if ( bOnePointThree || ExtensionSupported("GL_ARB_multitexture") )
     {
         // This code may look strange, but Apple OpenGL 1.2 defines GLint
         // as "long" whereas Windows and Linux define it as "int".
         GLint iMaxTextureUnits;
         glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB,&iMaxTextureUnits);
         m_iMaxTextureUnits = (int)iMaxTextureUnits;
         m_bCapMultitexture = true;

         // Trap to see if more texture per object are supported than what a
         // texture state object allows.  Currently this is clamped to the
         // value of TextureState::MAX_TEXTURE.
         if ( m_iMaxTextureUnits > TextureState::MAX_TEXTURES )
             m_iMaxTextureUnits = TextureState::MAX_TEXTURES;
     }
     else
     {
         m_iMaxTextureUnits = 1;
         m_bCapMultitexture = false;
     }

     // texture clamping to border
     m_bCapTextureClampToBorder = bOnePointThree
         || ExtensionSupported("GL_ARB_texture_border_clamp");

     // texture apply add mode
     m_bCapTextureApplyAdd = bOnePointThree
         || ExtensionSupported("GL_ARB_texture_env_add");

     // texture apply combine mode
     m_bCapTextureApplyCombine = bOnePointThree
         || ExtensionSupported("GL_ARB_texture_env_combine");
     if ( m_bCapTextureApplyCombine )
     {
         m_bCapTextureApplyCombineDot3 = bOnePointThree
             || ExtensionSupported("GL_ARB_texture_env_dot3");
     }

     // Dot3 bump mapping (combine, combine dot3, and multitexturing)
     m_bCapDot3BumpMapping =
         m_bCapTextureApplyCombine &&
         m_bCapTextureApplyCombineDot3 &&
         m_iMaxTextureUnits > 1;

     // query if stencil buffer was really created
     GLint iBits = 0;
     glGetIntegerv((GLenum)GL_STENCIL_BITS,&iBits);

     // projected planar shadow (uses stencil buffer)
     m_bCapPlanarShadow = ( iBits > 0 );

     // planar reflection (uses stencil buffer and blend color)
     m_bCapPlanarReflection = ( iBits > 0 &&
         ExtensionSupported("GL_EXT_blend_color") );

     // vertex/pixel shaders
     m_iCapPixShaderVersion = ( ExtensionSupported("GL_ARB_fragment_program") 
         ? Shader::ARBFP1 : Shader::UNSUPPORTED );
     m_iCapVertShaderVersion = ( ExtensionSupported("GL_ARB_vertex_program" )
         ? Shader::ARBVP1 : Shader::UNSUPPORTED );
}
//----------------------------------------------------------------------------
bool OpenGLRenderer::SupportsShader (const VertexShader* pkVertexShader) const
{
    return ( pkVertexShader && (m_iCapVertShaderVersion >= 
        pkVertexShader->GetVersion(Shader::OPENGL)) );
}
//----------------------------------------------------------------------------
bool OpenGLRenderer::SupportsShader (const PixelShader* pkPixelShader) const
{
    return ( pkPixelShader && (m_iCapPixShaderVersion >= 
        pkPixelShader->GetVersion(Shader::OPENGL)) );
}
//----------------------------------------------------------------------------
void OpenGLRenderer::SetBackgroundColor (const ColorRGB& rkColor)
{
    Renderer::SetBackgroundColor(rkColor);
    glClearColor(rkColor.r,rkColor.g,rkColor.b,1.0f);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearBackBuffer ()
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(0,0,m_iWidth,m_iHeight);
    glClear(GL_COLOR_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearZBuffer ()
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(0,0,m_iWidth,m_iHeight);
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glClear(GL_DEPTH_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearStencilBuffer ()
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(0,0,m_iWidth,m_iHeight);
    glStencilMask(~0);
    glClear(GL_STENCIL_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearBuffers ()
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(0,0,m_iWidth,m_iHeight);
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glStencilMask(~0);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearBackBuffer (int iXPos, int iYPos, int iWidth,
    int iHeight)
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(iXPos,iYPos,iWidth,iHeight);
    glClear(GL_COLOR_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearZBuffer (int iXPos, int iYPos, int iWidth,
    int iHeight)
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(iXPos,iYPos,iWidth,iHeight);
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glClear(GL_DEPTH_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearStencilBuffer (int iXPos, int iYPos, int iWidth,
    int iHeight)
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(iXPos,iYPos,iWidth,iHeight);
    glStencilMask(~0);
    glClear(GL_STENCIL_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ClearBuffers (int iXPos, int iYPos, int iWidth,
    int iHeight)
{
    glDisable(GL_DITHER);
    glEnable(GL_SCISSOR_TEST);
    glScissor(iXPos,iYPos,iWidth,iHeight);
    glEnable(GL_DEPTH_TEST);
    glDepthMask(GL_TRUE);
    glStencilMask(~0);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
    glDisable(GL_SCISSOR_TEST);
    glEnable(GL_DITHER);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::InitializeState ()
{
    Renderer::InitializeState();

    // vertices always exist
    glEnableClientState(GL_VERTEX_ARRAY);

    // no global ambient lighting
    GLfloat afBlack[4] = { 0.0f, 0.0f, 0.0f, 1.0f };
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT,afBlack);

    // disable drawing of lines as sequences of dashes (want solid lines)
    glDisable(GL_LINE_STIPPLE);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::DrawPrimitive (const Geometry& rkPrimitive,
    bool bIs3DPrimitive, GLenum eMode, GLsizei iCount,
    const GLvoid* pvIndices)
{
    // set up the transformations
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();

    if ( bIs3DPrimitive )
    {
        // set world matrix
        const Matrix3f& rkRot = rkPrimitive.WorldRotate();
        const Vector3f& rkTrn = rkPrimitive.WorldTranslate();
        float fScale = rkPrimitive.WorldScale();

        m_kWorldMatrix[0][0] = fScale*rkRot[0][0];
        m_kWorldMatrix[0][1] = fScale*rkRot[1][0];
        m_kWorldMatrix[0][2] = fScale*rkRot[2][0];
        m_kWorldMatrix[0][3] = 0.0f;

        m_kWorldMatrix[1][0] = fScale*rkRot[0][1];
        m_kWorldMatrix[1][1] = fScale*rkRot[1][1];
        m_kWorldMatrix[1][2] = fScale*rkRot[2][1];
        m_kWorldMatrix[1][3] = 0.0f;

        m_kWorldMatrix[2][0] = fScale*rkRot[0][2];
        m_kWorldMatrix[2][1] = fScale*rkRot[1][2];
        m_kWorldMatrix[2][2] = fScale*rkRot[2][2];
        m_kWorldMatrix[2][3] = 0.0f;

        m_kWorldMatrix[3][0] = rkTrn.X();
        m_kWorldMatrix[3][1] = rkTrn.Y();
        m_kWorldMatrix[3][2] = rkTrn.Z();
        m_kWorldMatrix[3][3] = 1.0f;

        glMultMatrixf(&m_kWorldMatrix[0][0]);

        if ( fScale != 1.0f )
            glEnable(GL_RESCALE_NORMAL);
    }
    else
    {
        // screen polygon already in correct coordinates
        glLoadIdentity();
        m_kWorldMatrix = Matrix4f::IDENTITY;

        // draw polygon using an orthogonal frustum
        glMatrixMode(GL_PROJECTION);    
        glPushMatrix();
        glLoadIdentity();
        glOrtho(0.0,1.0,0.0,1.0,0.0,1.0);
    }

    // Set up of shaders needs to happen after the model->world matrix gets
    // set in case a constant needs it.
    SetShaderConstants(&rkPrimitive);

    bool bHasVertexShader = (m_pkCurVertexShader != NULL);

    // Set the vertex array.
    GLfloat* afVertices = (GLfloat*)rkPrimitive.Vertices();
    glVertexPointer(3,GL_FLOAT,0,afVertices);

    // Set the normal array. If a bump map is being drawn, the normals are
    // not needed for drawing since the bump map object has already calculated
    // vertex colors using the surface normals.
    GLfloat* afNormals = (GLfloat*)rkPrimitive.Normals();
    if ( ( afNormals && !m_pkCurrentBumpMap ) && ( !bHasVertexShader ||
        m_pkCurVertexShader->NeedsNormals() ) )
    {
        glEnableClientState(GL_NORMAL_ARRAY);
        glNormalPointer(GL_FLOAT,0,afNormals);
    }
    else
    {
        glDisableClientState(GL_NORMAL_ARRAY);
        // ensure that it isn't the case that there is a vertex shader
        // that needs normals, but the geometry just doesn't have any to send.
        assert( !bHasVertexShader || !m_pkCurVertexShader->NeedsNormals() );
    }

    // Set the color array.  If a bump map is being drawn, the colors need to
    // be calculated if the bump map object has morphed since last time it was
    // drawn.
    GLfloat* afColors = (GLfloat*)rkPrimitive.Colors();
    if ( afColors  && ( !bHasVertexShader || 
        m_pkCurVertexShader->NeedsColor() ) )
    {
        if ( m_pkCurrentBumpMap )
        {
            // Fill the color array with light direction vectors.  This uses
            // conceptual constness.  The typecast allows the color array to
            // be modified transparently by the renderer.  The typecast is
            // safe to do since the callers of DrawPrimitive make sure that
            // bump mapping is disabled for non-TriMesh objects.
            m_pkCurrentBumpMap->ComputeLightVectors((TriMesh&)rkPrimitive);
        }
        // else:  vertex colors supplied by the object

        glEnableClientState(GL_COLOR_ARRAY);
        glColorPointer(3,GL_FLOAT,0,afColors);
    }
    else
    {
        glDisableClientState(GL_COLOR_ARRAY);
        // ensure that it isn't the case that there is a vertex shader
        // that needs colors, but the geometry just doesn't have any to send.
        assert( !bHasVertexShader || !m_pkCurVertexShader->NeedsColor() ||
            afColors );
    }

    // set the textures
    GLfloat* afTextures0 = (GLfloat*)rkPrimitive.Textures();
    GLfloat* afTextures1 = (GLfloat*)rkPrimitive.Textures1();
    GLfloat* afTextures2 = (GLfloat*)rkPrimitive.Textures2();
    GLfloat* afTextures3 = (GLfloat*)rkPrimitive.Textures3();
    GLfloat* afTexturesBump = (GLfloat*)rkPrimitive.TexturesBump();

    // set the active texture if multitexturing is supported
    if ( m_iMaxTextureUnits > 1 )
        glClientActiveTextureARB(GL_TEXTURE0_ARB);

    // Set the texture coordinate arrays.  If there is a bump map active, we
    // do not do any user-requested texturing. BumpMap will allow the user
    // textures to be drawn on the second pass.
    if ( ( afTextures0 || (m_pkCurrentBumpMap && afTexturesBump) ) &&
        ( !bHasVertexShader || m_pkCurVertexShader->NeedsTexCoords(0) ) )
    {
        glEnableClientState(GL_TEXTURE_COORD_ARRAY); 
        if ( m_pkCurrentBumpMap )
            glTexCoordPointer(2,GL_FLOAT,0,afTexturesBump);
        else
            glTexCoordPointer(2,GL_FLOAT,0,afTextures0);
    }
    else if ( !TextureUnitRequested(0) )
    {
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        // ensure that it isn't the case that there is a vertex shader
        // that needs tex coords, but the geometry just doesn't have 
        // any to send.
        assert( !bHasVertexShader || 
            !m_pkCurVertexShader->NeedsTexCoords(0) );
    }

    if ( m_iMaxTextureUnits > 1 )
    {
        glClientActiveTextureARB(GL_TEXTURE1_ARB);
        if ( ( afTextures1 && !m_pkCurrentBumpMap ) &&
            ( !bHasVertexShader || m_pkCurVertexShader->NeedsTexCoords(1) ) )
        {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY); 
            glTexCoordPointer(2,GL_FLOAT,0,afTextures1);
        }
        else if ( !TextureUnitRequested(1) )
        {
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            // ensure that it isn't the case that there is a vertex shader
            // that needs tex coords, but the geometry just doesn't have 
            // any to send.
            assert( !bHasVertexShader || 
                !m_pkCurVertexShader->NeedsTexCoords(1) );
        }
    }

    if ( m_iMaxTextureUnits > 2 )
    {
        glClientActiveTextureARB(GL_TEXTURE2_ARB);
        if ( ( afTextures2 && !m_pkCurrentBumpMap ) &&
            ( !bHasVertexShader || m_pkCurVertexShader->NeedsTexCoords(2) ) )
        {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY); 
            glTexCoordPointer(2,GL_FLOAT,0,afTextures2);
        }
        else if ( !TextureUnitRequested(2) )
        {
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            // ensure that it isn't the case that there is a vertex shader
            // that needs tex coords, but the geometry just doesn't have 
            // any to send.
            assert( !bHasVertexShader || 
                !m_pkCurVertexShader->NeedsTexCoords(2) );
        }
    }

    if ( m_iMaxTextureUnits > 3 )
    {
        glClientActiveTextureARB( GL_TEXTURE3_ARB );
        if ( ( afTextures3 && !m_pkCurrentBumpMap ) &&
            ( !bHasVertexShader || m_pkCurVertexShader->NeedsTexCoords(3) ) )
        {
            glEnableClientState(GL_TEXTURE_COORD_ARRAY); 
            glTexCoordPointer(2,GL_FLOAT,0,afTextures3);
        }
        else if ( !TextureUnitRequested(3) )
        {
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
            // ensure that it isn't the case that there is a vertex shader
            // that needs tex coords, but the geometry just doesn't have 
            // any to send.
            assert( !bHasVertexShader || 
                !m_pkCurVertexShader->NeedsTexCoords(3) );
        }
    }

    // reset the active texture if multitexturing is supported
    if ( m_iMaxTextureUnits > 1 )
        glClientActiveTextureARB(GL_TEXTURE0_ARB);

    // set the texture state for the bump map directly before the draw
    if ( m_pkCurrentBumpMap )
        SetTextureState(m_pkCurrentBumpMap->GetTextureState());

    glDrawElements(eMode,iCount,GL_UNSIGNED_INT,pvIndices);

    if ( bIs3DPrimitive )
    {
        if ( rkPrimitive.WorldScale() != 1.0f )
            glDisable(GL_RESCALE_NORMAL);
    }
    else
    {
        // restore to a perspective frustum
        glPopMatrix();
    }

    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const TriMesh& rkMesh)
{
    DrawPrimitive(rkMesh,true,GL_TRIANGLES,3*rkMesh.GetTriangleQuantity(),
        rkMesh.Connectivity());
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const Wml::Polyline& rkLine)
{
    if ( rkLine.GetActiveQuantity() < 2 )
    {
        // polyline needs at least one line segment
        return;
    }

    // bump mapping not supported for lines
    BumpMap* pkSave = m_pkCurrentBumpMap;
    m_pkCurrentBumpMap = NULL;

    if ( rkLine.Contiguous() )
    {
        if ( rkLine.Closed() )
        {
            DrawPrimitive(rkLine,true,GL_LINE_LOOP,rkLine.GetActiveQuantity(),
                rkLine.Indices());
        }
        else
        {
            DrawPrimitive(rkLine,true,GL_LINE_STRIP,
                rkLine.GetActiveQuantity(),rkLine.Indices());
        }
    }
    else
    {
        DrawPrimitive(rkLine,true,GL_LINES,rkLine.GetActiveQuantity(),
            rkLine.Indices());
    }
    m_pkCurrentBumpMap = pkSave;
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const Polypoint& rkPoint)
{
    if ( rkPoint.GetActiveQuantity() == 0 )
        return;

    // bump mapping not supported for points
    BumpMap* pkSave = m_pkCurrentBumpMap;
    m_pkCurrentBumpMap = NULL;

    DrawPrimitive(rkPoint,true,GL_POINTS,rkPoint.GetActiveQuantity(),
        rkPoint.Indices());

    m_pkCurrentBumpMap = pkSave;
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const Particles& rkParticle)
{
    const TriMesh& rkMesh = *rkParticle.GetMesh();
    DrawPrimitive(rkMesh,true,GL_TRIANGLES,3*rkMesh.GetTriangleQuantity(),
        rkMesh.Connectivity());
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const ScreenPolygon& rkPolygon)
{
    const TriMesh& rkMesh = *rkPolygon.Mesh();
    DrawPrimitive(rkMesh,false,GL_TRIANGLES,3*rkMesh.GetTriangleQuantity(),
        rkMesh.Connectivity());
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const unsigned char* aucBuffer)
{
    if ( !aucBuffer )
        return;

    // turn off shaders for this
    SetShaderState(NULL);

    // disable other states that are not necessary for the buffer copy
    GLboolean bDepthTest = glIsEnabled(GL_DEPTH_TEST);
    GLboolean bLighting = glIsEnabled(GL_LIGHTING);
    GLboolean bTexture2D = glIsEnabled(GL_TEXTURE_2D);
    if ( bDepthTest )
        glDisable(GL_DEPTH_TEST);
    if ( bLighting )
        glDisable(GL_LIGHTING);
    if ( bTexture2D )
        glDisable(GL_TEXTURE_2D);

    // Set raster position to window coord (0,H-1).  The hack here avoids
    // problems with invalid raster positions which would cause glDrawPixels
    // not to execute.  OpenGL uses right-handed screen coordinates, so using
    // (0,H-1) as the raster position followed by glPixelZoom(1,-1) tells
    // OpenGL to draw the screen in left-handed coordinates starting at the
    // top row of the screen and finishing at the bottom row.
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(0.0,(double)m_iWidth,0.0,(double)m_iHeight,0.0,1.0);
    glRasterPos3f(0.0,0.0,0.0);
    GLubyte aucBitmap[1] = {0};
    glBitmap(0,0,0.0f,0.0f,0.0f,(float)m_iHeight,aucBitmap);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
    glPopMatrix();

    glPixelZoom(1.0f,-1.0f);
    glDrawPixels((GLint)m_iWidth,(GLint)m_iHeight,GL_BGR,GL_UNSIGNED_BYTE,
        (const GLvoid*)aucBuffer);
    glPixelZoom(1.0f,1.0f);

    // reenable states
    if ( bDepthTest )
        glEnable(GL_DEPTH_TEST);
    if ( bLighting )
        glEnable(GL_LIGHTING);
    if ( bTexture2D )
        glEnable(GL_TEXTURE_2D);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// driver and extension information
//----------------------------------------------------------------------------
const char* OpenGLRenderer::GetVendor ()
{
    return (const char*)glGetString(GL_VENDOR);
}
//----------------------------------------------------------------------------
const char* OpenGLRenderer::GetRenderer ()
{
    return (const char*)glGetString(GL_RENDERER);
}
//----------------------------------------------------------------------------
const char* OpenGLRenderer::GetVersion ()
{
    return (const char*)glGetString(GL_VERSION);
}
//----------------------------------------------------------------------------
const char* OpenGLRenderer::GetGluVersion ()
{
    return (const char*)glGetString((GLenum)GLU_VERSION);
}
//----------------------------------------------------------------------------
const char* OpenGLRenderer::GetExtensions ()
{
    return (const char*)glGetString(GL_EXTENSIONS);
}
//----------------------------------------------------------------------------
bool OpenGLRenderer::IsMinimumVersion (int iMajor, int iMinor, int iRelease)
{
    int iVMajor = 0, iVMinor = 0, iVRelease = 0;
    sscanf(GetVersion(),"%d.%d.%d",&iVMajor,&iVMinor,&iVRelease);

    if ( iVMajor < iMajor )
        return false;

    if ( iVMajor > iMajor )
        return true;

    if ( iVMinor < iMinor )
        return false;

    if ( iVMinor > iMinor )
        return true;

    if ( iVRelease < iRelease )
        return false;

    return true;
}
//----------------------------------------------------------------------------
bool OpenGLRenderer::ExtensionSupported (const char* acExt)
{
    // make sure a proper string has been passed
    if ( !acExt || acExt[0] == 0 )
        return false;

    char* acBegin = strstr(GetExtensions(),acExt);
    if ( acBegin )
    {
        // The extension was found, but make sure it is not a proper substring
        // of another extension.  TO DO:  Should cEnd be compared to tabs or
        // other white space characters?
        char cEnd = *(acBegin + strlen(acExt));
        return cEnd == ' ' || cEnd == 0;
    }

    return false;
}
//----------------------------------------------------------------------------
