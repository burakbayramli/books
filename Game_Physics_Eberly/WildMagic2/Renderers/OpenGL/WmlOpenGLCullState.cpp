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

GLenum OpenGLRenderer::ms_aeFrontFace[CullState::FT_QUANTITY] =
{
    GL_CCW,
    GL_CW
};

GLenum OpenGLRenderer::ms_aeCullFace[CullState::CT_QUANTITY] =
{
    GL_FRONT,
    GL_BACK
};

//----------------------------------------------------------------------------
void OpenGLRenderer::SetCullState (CullState* pkState)
{
    if ( pkState->Enabled() )
        glEnable(GL_CULL_FACE);
    else
        glDisable(GL_CULL_FACE);

    glFrontFace(ms_aeFrontFace[pkState->FrontFace()]);

    if ( m_bReverseCullState )
    {
        if ( ms_aeCullFace[pkState->CullFace()] == GL_BACK )
            glCullFace(GL_FRONT);
        else
            glCullFace(GL_BACK);
    } 
    else
    {
        glCullFace(ms_aeCullFace[pkState->CullFace()]);
    }
}
//----------------------------------------------------------------------------
