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
#include "WmlGlossMap.h"
using namespace Wml;

//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const GlossMap& rkGlossMap)
{
    // have the renderer draw the objects with only specular lighting
    OverrideLightingMode eOLMTemp = m_eOverrideLightingMode;
    m_eOverrideLightingMode = OLM_ONLY_SPECULAR;

    // draw the objects in the usual manner
    Renderer::Draw(rkGlossMap.GetObjects());

    // The frame buffer now has the specular component of the lighting
    // equation in it.  Set up the blending mode so that it puts into the
    // frame buffer the sum of the new, non-specularly lit fragment (the
    // source) and the product of the source alpha and the current frame
    // buffer value (the specular solution).  As a result, whereever the
    // source alpha is 1, the surface will appear to be specular.

    // set up the blend
    rkGlossMap.GetAlphaState()->BlendEnabled() = true;
    SetAlphaState(rkGlossMap.GetAlphaState());

    // Apply the gloss map.  Do not let the regular rendering disable it.
    RequestTextureUnit(rkGlossMap.GetTextureUnit());
    SetTextureState(rkGlossMap.GetTextureState());

    // have the renderer draw the objects with only non-specular lighting
    m_eOverrideLightingMode = OLM_ONLY_NON_SPECULAR;
    m_bOverrideAlphaState = true;

    // let the renderer draw as usual, setting state, etc.
    Renderer::Draw(rkGlossMap.GetObjects());

    // clean up
    m_bOverrideAlphaState = false;
    m_eOverrideLightingMode = eOLMTemp;

    ReleaseTextureUnit(rkGlossMap.GetTextureUnit());

    rkGlossMap.GetAlphaState()->BlendEnabled() = false;
    SetAlphaState(rkGlossMap.GetAlphaState());
}
//----------------------------------------------------------------------------
