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
#include "WmlPlanarReflection.h"
using namespace Wml;

//----------------------------------------------------------------------------
void ComputeReflectionMatrix (GLfloat aafReflMatrix[4][4], 
    GLdouble adPlaneEq[4])
{
    aafReflMatrix[0][0] = (GLfloat)(1.0 - 2.0*adPlaneEq[0]*adPlaneEq[0]);
    aafReflMatrix[1][0] = (GLfloat)(    - 2.0*adPlaneEq[0]*adPlaneEq[1]);
    aafReflMatrix[2][0] = (GLfloat)(    - 2.0*adPlaneEq[0]*adPlaneEq[2]);
    aafReflMatrix[3][0] = (GLfloat)(    - 2.0*adPlaneEq[3]*adPlaneEq[0]);

    aafReflMatrix[0][1] = (GLfloat)(    - 2.0*adPlaneEq[1]*adPlaneEq[0]);
    aafReflMatrix[1][1] = (GLfloat)(1.0 - 2.0*adPlaneEq[1]*adPlaneEq[1]);
    aafReflMatrix[2][1] = (GLfloat)(    - 2.0*adPlaneEq[1]*adPlaneEq[2]);
    aafReflMatrix[3][1] = (GLfloat)(    - 2.0*adPlaneEq[3]*adPlaneEq[1]);

    aafReflMatrix[0][2] = (GLfloat)(    - 2.0*adPlaneEq[2]*adPlaneEq[0]);
    aafReflMatrix[1][2] = (GLfloat)(    - 2.0*adPlaneEq[2]*adPlaneEq[1]);
    aafReflMatrix[2][2] = (GLfloat)(1.0 - 2.0*adPlaneEq[2]*adPlaneEq[2]);
    aafReflMatrix[3][2] = (GLfloat)(    - 2.0*adPlaneEq[3]*adPlaneEq[2]);

    aafReflMatrix[0][3] = 0.0f;
    aafReflMatrix[1][3] = 0.0f;
    aafReflMatrix[2][3] = 0.0f;
    aafReflMatrix[3][3] = 1.0f;
}
//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const PlanarReflection& rkPReflection)
{
    TriMeshPtr spkPlane = rkPReflection.GetPlane();
    NodePtr spkCaster = rkPReflection.GetCaster();

    if ( !m_bCapPlanarReflection )
    {
        // The effect is not supported.  Draw normally without the mirror.
        // The OnDraw calls are necessary to handle culling and camera plane
        // state.
        spkPlane->OnDraw(*this);
        spkCaster->OnDraw(*this);
        return;
    }

    if ( m_bDrawingReflected )
    {
        // Some other object is currently doing a planar reflection.  Do not
        // allow the recursion and just draw normally.
        Renderer::Draw(spkCaster);
        SetState(spkPlane->GetRenderStateArray());
        Draw(*spkPlane);
        return;
    }

    // TO DO:  Support for multiple mirrors could be added here by iterating
    // over the section delimited by START PER-MIRROR and END PER-MIRROR.
    // None of the OpenGL code needs to change, just the mirror-plane data.

    // START PER-MIRROR

    // enable depth buffering
    glEnable(GL_DEPTH_TEST);
    glDepthFunc(GL_LESS);
    glDepthMask(GL_TRUE);

    // Step 1 setup and render.
    // Render the mirror into the stencil plane (but no color).  All visible
    // mirror pixels will have the stencil value of the mirror.
    // Make sure that no pixels are written to the depth buffer or color
    // buffer, but use depth buffer testing so that the stencil will not
    // be written where the plane is behind something already in the
    // depth buffer.
    glEnable(GL_STENCIL_TEST);
    glStencilFunc(GL_ALWAYS,rkPReflection.GetStencilValue(),~0);
    glStencilOp(GL_KEEP,GL_KEEP,GL_REPLACE);
    glStencilMask(~0);
    glColorMask(GL_FALSE,GL_FALSE,GL_FALSE,GL_FALSE);
    glDepthMask(GL_FALSE);
    Draw(*spkPlane);


    // Step 2 setup and render.
    // Render the mirror plane again by only processing pixels where
    // the stencil buffer contains the reference value.  This time
    // there is no changes to the stencil buffer and the depth buffer value
    // is reset to the far view clipping plane (this is done by setting the
    // range of depth values in the viewport volume to be [1,1].  Since the
    // mirror plane cannot also be semi-transparent, then there we do not
    // care what is behind the mirror plane in the depth buffer.  We need
    // to move the depth buffer values back where the mirror plane will
    // be rendered so that when the reflected caster is rendered
    // it can be depth buffered correctly (note that the rendering
    // of the reflected caster will cause depth value to be written
    // which will appear to be behind the mirror plane).  Enable writes
    // to the color buffer.  Later when we want to render the reflecting
    // plane and have it blend with the background (which should contain
    // the reflected caster), we want to use the same blending function
    // so that the pixels where the reflected caster was not rendered
    // will contain the reflecting plane and in that case, the blending
    // result will have the reflecting plane appear to be opaque when
    // in reality it was blended with blending coefficients adding to one.
    SetState(spkPlane->GetRenderStateArray());
    glDepthRange(1.0,1.0);
    glDepthFunc(GL_ALWAYS);
    glStencilFunc(GL_EQUAL,rkPReflection.GetStencilValue(),~0);
    glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
    glStencilMask(~0);
    glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
    glDepthMask(GL_TRUE);
    Draw(*spkPlane);


    // Step 2 cleanup.
    // Restore the depth range and depth testing function.
    glDepthFunc(GL_LESS);
    glDepthRange(0.0,1.0);


    // Step 3 setup.
    // We are about to render the reflected caster.  For that, we
    // will need to compute the reflection viewing matrix.
    Vector3f kCurrNormal = spkPlane->WorldRotate()*
        rkPReflection.GetPlaneNormal();
    Vector3f kCurrPoint = spkPlane->WorldTranslate()+spkPlane->WorldScale()*
      (spkPlane->WorldRotate()*rkPReflection.GetPointOnPlane());
    GLdouble adPlaneEq[4] = { -kCurrNormal.X(), -kCurrNormal.Y(),
        -kCurrNormal.Z(), kCurrNormal.Dot(kCurrPoint) };
    adPlaneEq[0] = -adPlaneEq[0];
    adPlaneEq[1] = -adPlaneEq[1];
    adPlaneEq[2] = -adPlaneEq[2];
    adPlaneEq[3] = -adPlaneEq[3];
    GLfloat aafReflectionMatrix[4][4];
    ComputeReflectionMatrix(aafReflectionMatrix,adPlaneEq);

    // Save the modelview transform before replacing it with
    // the viewing transform which will handle the reflection.
    glPushMatrix();
    glMultMatrixf(&aafReflectionMatrix[0][0]);

    // Setup a clip plane so that only objects above the mirror plane
    // get reflected.
    glClipPlane(GL_CLIP_PLANE0,adPlaneEq);
    glEnable(GL_CLIP_PLANE0);


    // Reverse the cull direction.  Allow for models that are not necessarily
    // set up with front or back face culling.
    m_bReverseCullState = true;

    // We do not support mirrors reflecting mirrors.  They just appear as the
    // base color in a reflection.
    m_bDrawingReflected = true;


    // Step 3 render.
    // Render the reflected caster.  Only render where the stencil buffer
    // contains the reference value.  Enable depth testing.  This time
    // allow writes to the color buffer.
    glStencilFunc(GL_EQUAL,rkPReflection.GetStencilValue(),~0);
    glStencilOp(GL_KEEP,GL_KEEP,GL_KEEP);
    glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
    Renderer::Draw(spkCaster);


    // Step 3 cleanup.
    // Restore state.
    m_bDrawingReflected = false;
    m_bReverseCullState = false;
    glDisable(GL_CLIP_PLANE0);
    glPopMatrix();


    // Step 4 setup.
    // We are about to render the reflecting plane again.  Reset to
    // the render state for the reflecting plane.  We want to blend
    // the reflecting plane with what is already in the color buffer
    // where the reflecting plane will be rendered, particularly
    // either the image of the reflected caster or the reflecting
    // plane.  All we want to change about the rendering of the
    // reflecting plane at this stage is to force the alpha channel
    // to always be the reflectance value for the reflecting plane.
    // Render the reflecting plane wherever the stencil buffer is set
    // to the reference value.  This time clear the stencil buffer
    // reference value where it is set.  Perform the normal depth
    // buffer testing and writes.  Allow the color buffer to be
    // written to, but this time blend the relecting plane with
    // the values in the color buffer based on the reflectance value.
    // Note that where the stencil buffer is set, the color buffer
    // has either color values from the reflecting plane or the
    // reflected caster.  Blending will use src=1-alpha (reflecting plane)
    // and dest=alpha background (reflecting plane or reflected caster).
    SetState(spkPlane->GetRenderStateArray());
    glEnable(GL_BLEND);
    glBlendColorEXT(0.0f,0.0f,0.0f,rkPReflection.GetReflectance());
    glBlendFunc(GL_ONE_MINUS_CONSTANT_ALPHA_EXT,GL_CONSTANT_ALPHA_EXT);
    glStencilFunc(GL_EQUAL,rkPReflection.GetStencilValue(),~0);
    glStencilOp(GL_KEEP,GL_KEEP,GL_INVERT);
    glColorMask(GL_TRUE,GL_TRUE,GL_TRUE,GL_TRUE);
    Draw(*spkPlane);


    // Step 4 cleanup.
    glDisable(GL_BLEND);
    glDisable(GL_STENCIL_TEST);

    // END PER-MIRROR

    // render the objects as usual
    Renderer::Draw(spkCaster);
}
//----------------------------------------------------------------------------
