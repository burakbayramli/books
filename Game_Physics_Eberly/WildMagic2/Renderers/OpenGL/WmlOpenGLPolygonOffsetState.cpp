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
void OpenGLRenderer::SetPolygonOffsetState (PolygonOffsetState* pkState)
{
    if ( pkState->FillEnabled() )
        glEnable( GL_POLYGON_OFFSET_FILL );
    else
        glDisable( GL_POLYGON_OFFSET_FILL );

    if ( pkState->LineEnabled() )
        glEnable( GL_POLYGON_OFFSET_LINE );
    else
        glDisable( GL_POLYGON_OFFSET_LINE );

    if ( pkState->PointEnabled() )
        glEnable( GL_POLYGON_OFFSET_POINT );
    else
        glDisable( GL_POLYGON_OFFSET_POINT );

    if ( pkState->FillEnabled()
    ||   pkState->LineEnabled()
    ||   pkState->PointEnabled() )
    {
        glPolygonOffset((GLfloat)pkState->Scale(),(GLfloat)pkState->Bias());
    }
}
//----------------------------------------------------------------------------
