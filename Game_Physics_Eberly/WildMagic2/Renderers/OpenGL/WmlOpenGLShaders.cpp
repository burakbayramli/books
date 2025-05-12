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
#include "WmlOpenGLCamera.h"
#include "WmlGeometry.h"
#include "WmlDirectionalLight.h"
#include "WmlPointLight.h"
#include "WmlSpotLight.h"
using namespace Wml;

//----------------------------------------------------------------------------
void OpenGLRenderer::CompileShader (Shader* pkShader)
{
    GLuint uiType = ( pkShader->GetType() == Shader::PIXEL_SHADER ? 
        GL_FRAGMENT_PROGRAM_ARB : GL_VERTEX_PROGRAM_ARB );

    GLuint uiUserData;
    glGenProgramsARB(1,&uiUserData);
    glBindProgramARB(uiType,uiUserData);
    pkShader->SetUserData(sizeof(GLuint),&uiUserData);
    glProgramStringARB(uiType,GL_PROGRAM_FORMAT_ASCII_ARB,
        (GLsizei)strlen(pkShader->GetProgram(Shader::OPENGL)), 
        pkShader->GetProgram(Shader::OPENGL));
}
//----------------------------------------------------------------------------
void OpenGLRenderer::ReleaseShader (Shader* pkShader)
{
    GLuint uiUserData;
    pkShader->GetUserData(sizeof(GLuint),&uiUserData);
    if ( uiUserData )
    {
        glDeleteProgramsARB(1,&uiUserData);
        uiUserData = 0;
        pkShader->SetUserData(sizeof(GLuint),&uiUserData);
    }
}
//----------------------------------------------------------------------------
bool OpenGLRenderer::SetCurrentVertexShader (VertexShader* pkShader)
{
    if ( pkShader )
    {
        // if the renderer can't support this shader, just return
        if ( !SupportsShader(pkShader) )
            return false;

        // if this shader is not the current one, compile it and set it
        if ( m_pkCurVertexShader != pkShader )
        {
            GLuint uiUserData;
            pkShader->GetUserData(sizeof(GLuint),&uiUserData);
            if ( !uiUserData )
            {
                CompileShader(pkShader);

                // Side effect.  CompileShader sets the user data for the
                // shader.  We need to get the new value for the next block
                // of code.
                pkShader->GetUserData(sizeof(GLuint),&uiUserData);
            }

            glEnable(GL_VERTEX_PROGRAM_ARB);
            glBindProgramARB(GL_VERTEX_PROGRAM_ARB,uiUserData);
            m_pkCurVertexShader = pkShader;
        }
        return true;
    }

    // NULL input means disable the shaders
    if ( m_pkCurVertexShader != NULL )
    {
        glDisable(GL_VERTEX_PROGRAM_ARB);
        m_pkCurVertexShader = NULL;
    }
    return false;
}
//----------------------------------------------------------------------------
bool OpenGLRenderer::SetCurrentPixelShader (PixelShader* pkShader)
{
    if ( pkShader )
    {
        // if the renderer can't support this shader, just return
        if ( !SupportsShader(pkShader) )
            return false;

        // if this shader is not the current one, compile it and set it
        if ( m_pkCurPixelShader != pkShader )
        {
            GLuint uiUserData;
            pkShader->GetUserData(sizeof(GLuint),&uiUserData);
            if ( !uiUserData )
            {
                CompileShader(pkShader);

                // Side effect.  CompileShader sets the user data for the
                // shader.  We need to get the new value for the next block
                // of code.
                pkShader->GetUserData(sizeof(GLuint),&uiUserData);
            }

            glEnable(GL_FRAGMENT_PROGRAM_ARB);
            glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB,uiUserData);
            m_pkCurPixelShader = pkShader;
        }
        return true;
    }

    // NULL input means disable the shaders
    if ( m_pkCurPixelShader != NULL )
    {
        glDisable(GL_FRAGMENT_PROGRAM_ARB);
        m_pkCurPixelShader = NULL;
    }
    return false;
}
//----------------------------------------------------------------------------
static Matrix4f TransformMatrix (const Matrix4f& rkMat, int iTransform)
{
    // This is a convenience routine to transform a matrix by some method.
    // This method is only called by SetStateConst below.  This is just to
    // eliminate some repeated code and to eliminate a lot of case statements.
    // Also, due to the way shaders want their matrices, everything here is
    // transposed.

    switch ( iTransform )
    {
        case SC_NORMAL:
            return rkMat.Transpose();
            break;
        case SC_INVERSETRANSPOSE:
            return rkMat.Inverse();
            break;
        case SC_INVERSE:
            return rkMat.Inverse().Transpose();
            break;
        case SC_TRANSPOSE:
        default:
            // do nothing
            return rkMat;
            break;
    }
}
//----------------------------------------------------------------------------
void OpenGLRenderer::SetStateConst (float* afData, const Geometry& rkGeom,
    StateConstantType iType, int iTypeNum)
{
    // NOTE: All of this state is fairly unnecessary for the OpenGL.
    // OpenGL shaders allow for access to OpenGL state (which seems the
    // Right Way to do things).  However, if you want to have the same
    // application AND the same shader work for both OpenGL and DX, then
    // you're going to want to set the state here as well.  Otherwise,
    // if you use the shader support for it, the driver will handle this.

    // ARB fragment programs do not support glstate references (yet) and so
    // if you want state there, this is for that as well.

    // This function entails a giant case statement (from enumeration in 
    // WmlStateConstantType.h) to allow for automatic setting of renderer
    // state without intervention from (or exposure to) the user.

    Matrix4f kModView, kProj, kResult;

    switch ( iType )
    {
        // general renderer state
        case RENDERER_MODVIEW:
            glGetFloatv(GL_MODELVIEW_MATRIX,&kModView[0][0]);
            kResult = TransformMatrix(kModView,iTypeNum);
            break;
        case RENDERER_PROJ:
            glGetFloatv(GL_PROJECTION_MATRIX,&kProj[0][0]);
            kResult = TransformMatrix(kProj,iTypeNum);
            break;
        case RENDERER_MODVIEWPROJ:
            glGetFloatv(GL_PROJECTION_MATRIX,&kProj[0][0]);
            glGetFloatv(GL_MODELVIEW_MATRIX,&kModView[0][0]);
            kResult = TransformMatrix(kModView*kProj,iTypeNum);
            break;
        case RENDERER_MOD:
            kResult = TransformMatrix(m_kWorldMatrix,iTypeNum);
            break;
        default:
            // otherwise, fall through to generic renderer
            Renderer::SetStateConst(afData,rkGeom,iType,iTypeNum);
            return;
    }

    memcpy(afData,&kResult[0][0],16*sizeof(float));
}
//----------------------------------------------------------------------------
void OpenGLRenderer::SetPixelShaderConst (unsigned int uiRegister,
    float* afData, int iNumOfVector4s)
{
    for (int i = 0; i < iNumOfVector4s; i++)
    {
        glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB,uiRegister+i,
            &afData[4*i]);
    }
}
//----------------------------------------------------------------------------
void OpenGLRenderer::SetPixelShaderConst (ShaderConstants* pkShaderConsts,
    const Geometry& rkGeom)
{
    for (int i = 0; i < pkShaderConsts->GetNumConstants(); i++)
    {
        // If a state constant, update it before setting it.  This could
        // probably be done more intelligently than setting it every time.
        ShaderConst* pkConst = pkShaderConsts->GetConstant(i);
        StateConstantType iType = pkConst->GetType();
        if ( iType == NUMERICAL_CONSTANT )
            continue;

        if ( iType != USER_DEFINED )
        {
            SetStateConst(pkConst->GetData(),rkGeom,iType,
                pkConst->GetTypeOption());
        }

        SetPixelShaderConst(pkConst->GetRegister(),pkConst->GetData(),
            pkConst->GetSize());
    }
}
//----------------------------------------------------------------------------
void OpenGLRenderer::SetVertexShaderConst (unsigned int uiRegister,
    float* afData, int iNumOfVector4s)
{
    for (int i = 0; i < iNumOfVector4s; i++)
    {
        glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB,uiRegister+i,
            &afData[4*i]);
    }
}
//----------------------------------------------------------------------------
void OpenGLRenderer::SetVertexShaderConst (ShaderConstants* pkShaderConsts,
    const Geometry& rkGeom)
{
    for (int i = 0; i < pkShaderConsts->GetNumConstants(); i++)
    {
        // If a state constant, update it before setting it.  This could
        // probably be done more intelligently than setting it every time.
        ShaderConst* pkConst = pkShaderConsts->GetConstant(i);
        StateConstantType iType = pkConst->GetType();

        // numerical constants are handled automatically by openGL
        if ( iType == NUMERICAL_CONSTANT )
            continue;

        if ( iType != USER_DEFINED )
        {
            SetStateConst(pkConst->GetData(),rkGeom,iType,
                pkConst->GetTypeOption());
        }

        SetVertexShaderConst(pkConst->GetRegister(),pkConst->GetData(),
            pkConst->GetSize());
    }
}
//----------------------------------------------------------------------------
