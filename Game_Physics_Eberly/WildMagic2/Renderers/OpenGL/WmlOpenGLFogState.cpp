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

GLenum OpenGLRenderer::ms_aeFogDensity[FogState::DF_QUANTITY] =
{
    GL_LINEAR,
    GL_EXP,
    GL_EXP2
};

GLenum OpenGLRenderer::ms_aeFogApply[FogState::AF_QUANTITY] =
{
    GL_FASTEST,
    GL_NICEST
};

//----------------------------------------------------------------------------
void OpenGLRenderer::SetFogState (FogState* pkState)
{
    if ( pkState->Enabled() )
    {
        glEnable(GL_FOG);
        glFogf(GL_FOG_START,pkState->Start());
        glFogf(GL_FOG_END,pkState->End());

        GLfloat afColor[4] =
        {
            pkState->Color().r,
            pkState->Color().g,
            pkState->Color().b,
            1.0f
        };
        glFogfv(GL_FOG_COLOR,afColor);

        glFogf(GL_FOG_DENSITY,pkState->Density());
        glFogi(GL_FOG_MODE,ms_aeFogDensity[pkState->DFunction()]);
        glHint(GL_FOG_HINT,ms_aeFogApply[pkState->AFunction()]);
    }
    else
    {
        glDisable(GL_FOG);
    }
}
//----------------------------------------------------------------------------
