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

//----------------------------------------------------------------------------
void OpenGLRenderer::SetMaterialState (MaterialState* pkState)
{
    GLfloat afColor[4];

    // emissive color does not use transparency
    afColor[3] = 1.0f;

    afColor[0] = pkState->Emissive().r;
    afColor[1] = pkState->Emissive().g;
    afColor[2] = pkState->Emissive().b;
    glMaterialfv(GL_FRONT,GL_EMISSION,afColor);

    // ambient, diffuse, and specular colors use transparency
    afColor[3] = pkState->Alpha();

    afColor[0] = pkState->Ambient().r;
    afColor[1] = pkState->Ambient().g;
    afColor[2] = pkState->Ambient().b;
    glMaterialfv(GL_FRONT,GL_AMBIENT,afColor);

    afColor[0] = pkState->Diffuse().r;
    afColor[1] = pkState->Diffuse().g;
    afColor[2] = pkState->Diffuse().b;
    glMaterialfv(GL_FRONT,GL_DIFFUSE,afColor);

    afColor[0] = pkState->Specular().r;
    afColor[1] = pkState->Specular().g;
    afColor[2] = pkState->Specular().b;
    glMaterialfv(GL_FRONT,GL_SPECULAR,afColor);

    glMaterialf(GL_FRONT,GL_SHININESS,pkState->Shininess());
}
//----------------------------------------------------------------------------
