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
#include "WmlProjectedTexture.h"
#include "WmlOpenGLCamera.h"
using namespace Wml;

static float gs_afIdentity[16] =
{
    1.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
};

//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const ProjectedTexture& rkPTexture)
{
    int iUnit = 0;

    if ( m_iMaxTextureUnits > 1 )
    {
        // Need to move to another texture unit so that regular textures can
        // be applied.
        iUnit = RequestTextureUnit();
        if ( iUnit < 0 )
        {
            // cannot locate available unit, no projected texture drawn
            return;
        }
        glActiveTextureARB(GL_TEXTURE0_ARB + iUnit);
    }

    // camera/view matrix is on stack already.
    glMatrixMode(GL_MODELVIEW);

    // generate texture coordinates
    glTexGenfv(GL_S,GL_EYE_PLANE,&gs_afIdentity[0]);
    glTexGenfv(GL_T,GL_EYE_PLANE,&gs_afIdentity[4]);
    glTexGenfv(GL_R,GL_EYE_PLANE,&gs_afIdentity[8]);
    glTexGenfv(GL_Q,GL_EYE_PLANE,&gs_afIdentity[12]);

    glTexGeni(GL_S,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
    glTexGeni(GL_T,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
    glTexGeni(GL_R,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);
    glTexGeni(GL_Q,GL_TEXTURE_GEN_MODE,GL_EYE_LINEAR);

    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_Q);
    glEnable(GL_TEXTURE_GEN_T);
    glEnable(GL_TEXTURE_GEN_R);

    glMatrixMode(GL_TEXTURE);
    glPushMatrix();
    glLoadIdentity();

    // bias and scale the texture so it covers the near plane
    glTranslatef(0.5f,0.5f,0.0f);
    glScalef(0.5f,0.5f,1.0f);

    float fNear, fFar, fLeft, fRight, fTop, fBottom;
    rkPTexture.GetCamera()->GetFrustum(fNear,fFar,fLeft,fRight,fTop,fBottom);
    glFrustum(fLeft,fRight,fBottom,fTop,fNear,fFar);

    Vector3f kLocation = rkPTexture.GetCamera()->GetLocation();
    Vector3f kDirection = rkPTexture.GetCamera()->GetDirection();
    Vector3f kUp = rkPTexture.GetCamera()->GetUp();
    Vector3f kLookAt = kLocation + kDirection;
    gluLookAt(kLocation.X(),kLocation.Y(),kLocation.Z(),kLookAt.X(),
        kLookAt.Y(),kLookAt.Z(),kUp.X(),kUp.Y(),kUp.Z());

    // camera/view matrix is on stack already.
    glMatrixMode(GL_MODELVIEW);

    // fill the texture state object and bind it
    TextureState* pkTS = rkPTexture.GetTextureState();
    pkTS->Set(iUnit,rkPTexture.GetTexture());
    SetTextureState(pkTS);
    pkTS->Remove(iUnit);

    // draw the objects as usual
    Renderer::Draw(rkPTexture.GetObjects());

    // reset state (for those things not usually set by InitializeState())
    if ( m_iMaxTextureUnits > 1 )
        glActiveTextureARB(GL_TEXTURE0_ARB + iUnit);
     
    glMatrixMode(GL_TEXTURE);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);

    glDisable(GL_TEXTURE_GEN_Q);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);

    if ( m_iMaxTextureUnits > 1 )
    {
        // reset the active texture unit and release the texture
        glActiveTextureARB(GL_TEXTURE0_ARB);
        ReleaseTextureUnit(iUnit);
    }
}
//----------------------------------------------------------------------------
