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

GLenum OpenGLRenderer::ms_aeShade[ShadeState::SM_QUANTITY] =
{
    GL_FLAT,
    GL_SMOOTH
};

//----------------------------------------------------------------------------
void OpenGLRenderer::SetShadeState (ShadeState* pkState)
{
    glShadeModel(ms_aeShade[pkState->Shade()]);
}
//----------------------------------------------------------------------------
