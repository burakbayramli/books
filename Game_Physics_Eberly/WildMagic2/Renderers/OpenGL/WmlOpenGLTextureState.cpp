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
using namespace Wml;

GLenum OpenGLRenderer::ms_aeTextureCorrection[Texture::CM_QUANTITY] =
{
    GL_FASTEST,
    GL_NICEST
};

GLenum OpenGLRenderer::ms_aeTextureApply[Texture::AM_QUANTITY] =
{
    GL_REPLACE,
    GL_DECAL,
    GL_MODULATE,
    GL_BLEND,
    GL_ADD,
    GL_COMBINE
};

GLenum OpenGLRenderer::ms_aeTextureCombineFunc[Texture::ACF_QUANTITY] =
{
    GL_REPLACE,
    GL_MODULATE,
    GL_ADD,
    GL_ADD_SIGNED,
    GL_SUBTRACT,
    GL_INTERPOLATE,
    GL_DOT3_RGB,
    GL_DOT3_RGBA
};

GLenum OpenGLRenderer::ms_aeTextureCombineSrc[Texture::ACS_QUANTITY] = 
{
    GL_TEXTURE,
    GL_PRIMARY_COLOR,
    GL_CONSTANT,
    GL_PREVIOUS
};

GLenum OpenGLRenderer::ms_aeTextureCombineOperand[Texture::ACO_QUANTITY] =
{
    GL_SRC_COLOR,
    GL_ONE_MINUS_SRC_COLOR,
    GL_SRC_ALPHA,
    GL_ONE_MINUS_SRC_ALPHA
};

GLfloat OpenGLRenderer::ms_afTextureCombineScale[Texture::ACSC_QUANTITY] =
{
    1.0f,
    2.0f,
    4.0f
};

GLenum OpenGLRenderer::ms_aeTextureFilter[Texture::FM_QUANTITY] =
{
    GL_NEAREST,
    GL_LINEAR
};

GLenum OpenGLRenderer::ms_aeTextureMipmap[Texture::MM_QUANTITY] =
{
    GL_NEAREST,  // MM_NONE (no mipmap)
    GL_NEAREST,
    GL_LINEAR,
    GL_NEAREST_MIPMAP_NEAREST,
    GL_NEAREST_MIPMAP_LINEAR,
    GL_LINEAR_MIPMAP_NEAREST,
    GL_LINEAR_MIPMAP_LINEAR
};

GLenum OpenGLRenderer::ms_aeImageComponents[Image::IT_QUANTITY] =
{
    GL_RGBA4,
    GL_RGB8,
    GL_RGB5_A1,
    GL_RGBA8
};

GLenum OpenGLRenderer::ms_aeImageFormats[Image::IT_QUANTITY] =
{
    GL_RGBA,
    GL_RGB,
    GL_RGBA,
    GL_RGBA
};

//----------------------------------------------------------------------------
void OpenGLRenderer::SetTextureState (TextureState* pkState)
{
    int i, iQuantity = pkState->GetQuantity();

    if ( iQuantity > 0 )
    {
        // The index i indicates the texture unit to be used.  If the unit
        // does not exist, then that texture will not be drawn, even if a
        // lower index did not use up a unit.
        for (i = 0; i < m_iMaxTextureUnits; i++)
        {
            // set the active texture if multitexturing is supported
            if ( m_iMaxTextureUnits > 1 )
                glActiveTextureARB(GL_TEXTURE0_ARB + i);

            Texture* pkTexture = pkState->Get(i);
            if ( pkTexture )
            {
                glEnable(GL_TEXTURE_2D);
            }
            else
            {
                // Disable this unit only if we also know that a render effect
                // is not using it.
                if ( !TextureUnitRequested(i) )
                    glDisable(GL_TEXTURE_2D);
                continue;
            }

            GLuint uiUserData;
            pkTexture->GetUserData(sizeof(GLuint),&uiUserData);
            if ( uiUserData == 0 )
            {
                // texture seen first time, generate name and create data
                glGenTextures((GLsizei)1,&uiUserData);
                pkTexture->SetUserData(sizeof(GLuint),&uiUserData);

                // bind the texture
                glBindTexture(GL_TEXTURE_2D,uiUserData);

                // Get texture image data.  Not all textures have image data.
                // For example, AM_COMBINE modes can use primary colors,
                // texture output, and constants to modify fragments via the
                // texture units.
                Image* pkImage = pkTexture->GetImage();
                if ( pkImage )
                {
                    if ( pkTexture->Mipmap() == Texture::MM_NONE
                    ||   pkTexture->Mipmap() == Texture::MM_NEAREST
                    ||   pkTexture->Mipmap() == Texture::MM_LINEAR )
                    {
                        glTexImage2D(GL_TEXTURE_2D,0,
                            ms_aeImageComponents[pkImage->GetType()],
                            pkImage->GetWidth(),pkImage->GetHeight(),0,
                            ms_aeImageFormats[pkImage->GetType()],
                            GL_UNSIGNED_BYTE,pkImage->GetData());
                    }
                    else
                    {
                        gluBuild2DMipmaps(GL_TEXTURE_2D,
                            ms_aeImageComponents[pkImage->GetType()],
                            pkImage->GetWidth(),pkImage->GetHeight(),
                            ms_aeImageFormats[pkImage->GetType()],
                            GL_UNSIGNED_BYTE,pkImage->GetData());
                    }
                }

                // set parameters specific to this TEXTURE_2D target

                // set resident priority, in [0,1] with 1 the highest priority
                glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_PRIORITY,
                    pkTexture->Priority());

                // set up wrap mode
                switch ( pkTexture->Wrap() )
                {
                case Texture::WM_CLAMP_S_CLAMP_T:
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,
                        GL_CLAMP);
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,
                        GL_CLAMP);
                    break;
                case Texture::WM_CLAMP_S_WRAP_T:
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,
                        GL_CLAMP);
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,
                        GL_REPEAT);
                    break;
                case Texture::WM_WRAP_S_CLAMP_T:
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,
                        GL_REPEAT);
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,
                        GL_CLAMP);
                    break;
                case Texture::WM_WRAP_S_WRAP_T:
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,
                        GL_REPEAT);
                    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,
                        GL_REPEAT);
                    break;
                case Texture::WM_CLAMP_BORDER_S_CLAMP_BORDER_T:
                    if ( m_bCapTextureClampToBorder )
                    {
                        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S, 
                            GL_CLAMP_TO_BORDER_ARB);
                        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T, 
                            GL_CLAMP_TO_BORDER_ARB);
                    }
                    else
                    {
                        // default to WM_CLAMP_S_CLAMP_T
                        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,
                            GL_CLAMP);
                        glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,
                            GL_CLAMP);
                    }
                    break;
                default:  // Texture::WM_QUANTITY
                    break;
                }

                // set up filter mode
                glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,
                    ms_aeTextureFilter[pkTexture->Filter()]);

                // set up mipmap mode
                glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,
                    ms_aeTextureMipmap[pkTexture->Mipmap()]);

                // Set up the border color (for clamp to border).  WM only
                // has ColorRGB, but border color is RGBA.  Assume A = 0 for
                // now, but use ColorRGBA when WM2.x arrives.
                float afBorder[4] =
                {
                    pkTexture->BorderColor().r,
                    pkTexture->BorderColor().g,
                    pkTexture->BorderColor().b,
                    0.0f
                };
                glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR,
                    afBorder);
            }
            else
            {
                // texture already exists in OpenGL, just bind it
                glBindTexture(GL_TEXTURE_2D,uiUserData);
            }

            // set parameters not specific to the TEXTURE_2D target

            // set up correction mode
            glHint(GL_PERSPECTIVE_CORRECTION_HINT,
                ms_aeTextureCorrection[pkTexture->Correction()]);

            // set up apply mode
            switch ( pkTexture->Apply() )
            {
            case Texture::AM_ADD:
                if ( !m_bCapTextureApplyAdd )
                {
                    glDisable(GL_TEXTURE_2D);
                    break;
                }

                glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,
                    ms_aeTextureApply[Texture::AM_ADD]);
                break;
            case Texture::AM_COMBINE:
                if ( !m_bCapTextureApplyCombine )
                {
                    glDisable(GL_TEXTURE_2D);
                    break;
                }

                glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,
                    ms_aeTextureApply[Texture::AM_COMBINE]);

                // TO DO.  Check for the only invalid combine function
                // combination.  Ask Hans why this is here.
                if ( pkTexture->CombineFuncAlpha() == Texture::ACF_DOT3_RGB
                ||   pkTexture->CombineFuncAlpha() == Texture::ACF_DOT3_RGBA )
                {
                    glDisable(GL_TEXTURE_2D);
                    break;
                }

                if ( pkTexture->CombineFuncRGB() == Texture::ACF_DOT3_RGB
                ||   pkTexture->CombineFuncRGB() == Texture::ACF_DOT3_RGBA )
                {
                    // check if supported before proceeding
                    if ( !m_bCapTextureApplyCombineDot3 )
                    {
                        glDisable(GL_TEXTURE_2D);
                        break;
                    }
                }

                // set up all of the combine state
                glTexEnvi(GL_TEXTURE_ENV,GL_COMBINE_RGB,
                    ms_aeTextureCombineFunc[pkTexture->CombineFuncRGB()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_COMBINE_ALPHA,
                    ms_aeTextureCombineFunc[pkTexture->CombineFuncAlpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_SOURCE0_RGB,
                    ms_aeTextureCombineSrc[pkTexture->CombineSrc0RGB()]);
                    
                glTexEnvi(GL_TEXTURE_ENV,GL_SOURCE1_RGB,
                    ms_aeTextureCombineSrc[pkTexture->CombineSrc1RGB()]);  

                glTexEnvi(GL_TEXTURE_ENV,GL_SOURCE2_RGB,
                    ms_aeTextureCombineSrc[pkTexture->CombineSrc2RGB()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_SOURCE0_ALPHA,
                    ms_aeTextureCombineSrc[pkTexture->CombineSrc0Alpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_SOURCE1_ALPHA,
                    ms_aeTextureCombineSrc[pkTexture->CombineSrc1Alpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_SOURCE2_ALPHA,
                    ms_aeTextureCombineSrc[pkTexture->CombineSrc2Alpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_OPERAND0_RGB,
                    ms_aeTextureCombineOperand[pkTexture->CombineOp0RGB()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_OPERAND1_RGB,
                    ms_aeTextureCombineOperand[pkTexture->CombineOp1RGB()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_OPERAND2_RGB,
                    ms_aeTextureCombineOperand[pkTexture->CombineOp2RGB()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_OPERAND0_ALPHA,
                    ms_aeTextureCombineOperand[pkTexture->CombineOp0Alpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_OPERAND1_ALPHA,
                    ms_aeTextureCombineOperand[pkTexture->CombineOp1Alpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_OPERAND2_ALPHA,
                    ms_aeTextureCombineOperand[pkTexture->CombineOp2Alpha()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_RGB_SCALE,
                    ms_aeTextureCombineOperand[pkTexture->CombineScaleRGB()]);

                glTexEnvi(GL_TEXTURE_ENV,GL_ALPHA_SCALE,
                    ms_aeTextureCombineOperand[
                    pkTexture->CombineScaleAlpha()]);
                break;
            default:
                glTexEnvi(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,
                    ms_aeTextureApply[pkTexture->Apply()]);
            }

            // Set up the blend color.  WM only has ColorRGB, but blend
            // color is RGBA.  Assume A = 0 for now, but use ColorRGBA when
            // WM2.x arrives.
            float afBlend[4] =
            {
                pkTexture->BlendColor().r,
                pkTexture->BlendColor().g,
                pkTexture->BlendColor().b,
                0.0f
            };
            glTexEnvfv(GL_TEXTURE_ENV,GL_TEXTURE_ENV_COLOR,afBlend);

            // set up environment mapping
            switch ( pkTexture->Envmap() )
            {
            case Texture::EM_NONE:
                // turn off anything that other maps might have turned on
                glDisable(GL_TEXTURE_GEN_Q);
                glDisable(GL_TEXTURE_GEN_R);
                glDisable(GL_TEXTURE_GEN_S);
                glDisable(GL_TEXTURE_GEN_T);
                break;
            case Texture::EM_SPHERE:
                // generate texture coordinates 
                glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);
                glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_SPHERE_MAP);
                glEnable(GL_TEXTURE_GEN_S);
                glEnable(GL_TEXTURE_GEN_T);
                break;
            case Texture::EM_IGNORE:
                // Do not alter the texure generation status.  This allows
                // complex texturing outside of texture state to exist
                // peacefully.
                break;
            default:  // Texture::EM_QUANTITY
                break;
            }
        }
    }
    else
    {
        // set the active texture if multitexturing is supported
        if ( m_iMaxTextureUnits > 1 )
            glActiveTextureARB(GL_TEXTURE0_ARB);

        // Even if we do not have a texture, a render effect might be using
        // this unit, so do not disable it.
        if ( !TextureUnitRequested(0) )
            glDisable(GL_TEXTURE_2D);

        for (i = 1; i < m_iMaxTextureUnits; i++)
        {
            glActiveTextureARB(GL_TEXTURE0_ARB + i);
            if ( !TextureUnitRequested(i) )
                glDisable(GL_TEXTURE_2D);
        }
    }

    // reset the active texture if multitexturing is supported
    if ( m_iMaxTextureUnits > 1 )
        glActiveTextureARB(GL_TEXTURE0_ARB);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ReleaseTexture (Texture* pkTexture)
{
    // The Macintosh OpenGL appears to have a problem if the UserData value
    // is zero 0 (a null pointer).  The OpenGL reference on glDeleteTextures
    // indicates the zero should be ignored...
    assert( pkTexture );
    GLuint uiUserData;
    pkTexture->GetUserData(sizeof(GLuint),&uiUserData);
    if ( pkTexture && uiUserData )
    {
        glDeleteTextures((GLsizei)1,(GLuint*)&uiUserData);
        uiUserData = 0;
        pkTexture->SetUserData(sizeof(GLuint),&uiUserData);
    }
}
//----------------------------------------------------------------------------
