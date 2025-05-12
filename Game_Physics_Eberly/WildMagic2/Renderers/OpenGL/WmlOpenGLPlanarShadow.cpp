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
#include "WmlDirectionalLight.h"
#include "WmlPointLight.h"
#include "WmlPlanarShadow.h"
using namespace Wml;

//----------------------------------------------------------------------------
static void ComputeShadowMatrix (GLfloat aafShadowMatrix[4][4],
    float afPlaneEq[4], Light* pkLight)
{
    // create light vector based on light type
    float afLight[4];
    Vector3f kLightVec;
    switch ( pkLight->GetType() )
    {
    case Light::LT_AMBIENT:
        afLight[0] = 0.0f;
        afLight[1] = 0.0f;
        afLight[2] = 0.0f;
        afLight[3] = 1.0f;
        break;
    case Light::LT_DIRECTIONAL:
        kLightVec = -WmlDynamicCast(DirectionalLight,pkLight)->Direction();
        afLight[0] = kLightVec.X();
        afLight[1] = kLightVec.Y();
        afLight[2] = kLightVec.Z();
        afLight[3] = 0.0f;
        break;
    case Light::LT_POINT:
    case Light::LT_SPOT:
        kLightVec = WmlDynamicCast(PointLight,pkLight)->Location();
        afLight[0] = kLightVec.X();
        afLight[1] = kLightVec.Y();
        afLight[2] = kLightVec.Z();
        afLight[3] = 1.0f;
        break;
    default:  // Light::LT_QUANTITY
        break;
    }

    GLfloat fDot = (GLfloat)(
        afPlaneEq[0]*afLight[0] +
        afPlaneEq[1]*afLight[1] +
        afPlaneEq[2]*afLight[2] + 
        afPlaneEq[3]*afLight[3]);

    aafShadowMatrix[0][0] = (GLfloat)(fDot - afLight[0]*afPlaneEq[0]);
    aafShadowMatrix[1][0] = (GLfloat)(     - afLight[0]*afPlaneEq[1]);
    aafShadowMatrix[2][0] = (GLfloat)(     - afLight[0]*afPlaneEq[2]);
    aafShadowMatrix[3][0] = (GLfloat)(     - afLight[0]*afPlaneEq[3]);

    aafShadowMatrix[0][1] = (GLfloat)(     - afLight[1]*afPlaneEq[0]);
    aafShadowMatrix[1][1] = (GLfloat)(fDot - afLight[1]*afPlaneEq[1]);
    aafShadowMatrix[2][1] = (GLfloat)(     - afLight[1]*afPlaneEq[2]);
    aafShadowMatrix[3][1] = (GLfloat)(     - afLight[1]*afPlaneEq[3]);

    aafShadowMatrix[0][2] = (GLfloat)(     - afLight[2]*afPlaneEq[0]);
    aafShadowMatrix[1][2] = (GLfloat)(     - afLight[2]*afPlaneEq[1]);
    aafShadowMatrix[2][2] = (GLfloat)(fDot - afLight[2]*afPlaneEq[2]);
    aafShadowMatrix[3][2] = (GLfloat)(     - afLight[2]*afPlaneEq[3]);

    aafShadowMatrix[0][3] = (GLfloat)(     - afLight[3]*afPlaneEq[0]);
    aafShadowMatrix[1][3] = (GLfloat)(     - afLight[3]*afPlaneEq[1]);
    aafShadowMatrix[2][3] = (GLfloat)(     - afLight[3]*afPlaneEq[2]);
    aafShadowMatrix[3][3] = (GLfloat)(fDot - afLight[3]*afPlaneEq[3]);
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const PlanarShadow& rkPShadow)
{
    TriMeshPtr spkPlane = rkPShadow.GetPlane();
    NodePtr spkCaster = rkPShadow.GetCaster();

    if ( !m_bCapPlanarShadow )
    {
        // The effect is not supported.  Draw normally without the shadow.
        // The OnDraw calls are necessary to handle culling and camera plane
        // state.
        spkPlane->OnDraw(*this);
        spkCaster->OnDraw(*this);
        return;
    }

    // enable depth buffering
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LEQUAL);
    glDepthMask(GL_TRUE);

    // draw the caster
    Renderer::Draw(spkCaster);

    // Enable the stencil buffer so that the shadow can be clipped by the
    // plane.
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS,rkPShadow.GetStencilValue(),~0);
    glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
    glStencilMask(~0);

    // draw the plane
    SetState(spkPlane->GetRenderStateArray());
    Draw(*spkPlane);
    glDisable(GL_STENCIL_TEST);

    // Compute the current plane equation (rotate the normal and xform the
    // point).
    Vector3f kCurrNormal = spkPlane->WorldRotate()*rkPShadow.GetPlaneNormal();
    Vector3f kCurrPoint = spkPlane->WorldTranslate() + spkPlane->WorldScale()
        * (spkPlane->WorldRotate()*rkPShadow.GetPointOnPlane());
    spkPlane = NULL;
    float afPlaneEq[4] = { kCurrNormal.X(), kCurrNormal.Y(), kCurrNormal.Z(),
        -kCurrNormal.Dot(kCurrPoint) };

    // Conservative test to see if a shadow should be cast -- this can cause
    // incorrect results if the caster is large and intersects the plane, but
    // ordinarily we are not trying to cast shadows in such situations.
    Vector3f& rkCenter = spkCaster->WorldBound().Center();
    if ( kCurrNormal.Dot(rkCenter) + afPlaneEq[3] < 0.0f )
    {
        // caster is on far side of plane
        return;
    }

    // compute the projection matrix for the light source
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    GLfloat aafShadowMatrix[4][4];
    ComputeShadowMatrix(aafShadowMatrix,afPlaneEq,rkPShadow.GetLight());
    glMultMatrixf(&aafShadowMatrix[0][0]);

    // Reset the state, and turn off lighting and z-buffering.
    InitializeState();
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);

    // Attenuate the currently stored color by 50%.  Save the current color
    // to restore it after the shadow has been drawn.
    float afCurrColor[4];
    glGetFloatv(GL_CURRENT_COLOR,afCurrColor);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
    glColor4f(0.0f,0.0f,0.0f,0.5f);

    // Only draw where the plane has been drawn, and don't allow the object
    // to alter render state.
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_EQUAL,rkPShadow.GetStencilValue(),~0);
    glStencilOp(GL_KEEP,GL_KEEP,GL_ZERO);

    // Draw the caster again.  TO DO.  Might have to disable culling to allow
    // out-of-view objects to cast shadows.
    m_bOverrideState = true;
    Renderer::Draw(spkCaster);
    m_bOverrideState = false;

    // restore current color
    glColor4f(afCurrColor[0],afCurrColor[1],afCurrColor[2],afCurrColor[3]);

    // clean up
    glDisable(GL_BLEND);
    glDisable(GL_STENCIL_TEST);
    glPopMatrix();
}
//----------------------------------------------------------------------------
