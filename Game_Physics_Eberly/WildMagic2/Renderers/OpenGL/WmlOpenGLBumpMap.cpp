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
#include "WmlBumpMap.h"
using namespace Wml;

// Bump mapping uses a normal map and a light to generate N and L vectors in
// surface-local space at each vertex.  The L vector is stuffed into color,
// and so it is interpolated across the surface along with the N vector.  A
// normalization cube map could be used to renormalize the L vector if such a
// level of accuracy is required.
//
// When applied, bump mapping takes the primary color (interpolated L vector)
// and does a dot3 with the texture (normal map) to yield dot(N,L).
//
// In a single pass implementation we are somewhat restricted in terms of how
// we can use this term because we have to have the bump map on the first
// texture unit since it cannot operate on anything other than the L vector
// and the N vector; that is, it would wipe out all results from earlier
// texture units.  As a side note, bump mapping takes care of lighting a
// surface, so you don't need to attach a light state to a bump-mapped
// surface.
//
// The most practical choices for lighting models are as follows:
//
// 1) If there is no base texture(s), then we compute:
//     dot(N,L)*(diffMat*diffLight)+ambMat*ambLight 
//
// 2) If there is a base texture(s), then we compute:
//    (dot(N,L)*(diffMat*diffLight)+(ambMat*ambLight))*baseTex1*baseTex2*etc.
// 
// In either case we do not support specular lighting.  Adding in specular
// lighting would require another pass, with the same bump map, and a blend.
//
// If there are not enough texture units, we combine the diffuse and ambient
// terms as an approximation even though modulation of the ambient term by
// dot(N,L) is incorrect.  Wild Magic currently supports 4 textures per
// surface, so case 1 above is fine (only 3 textures), but case 2 is limited
// to a single base texture.  If more base textures are wanted, the
// MAX_TEXTURES value in the texture state class would have to be increased
// (this will happen in Wild Magic version 2).  The maximum number of textures
// is min(TextureState::MAX_TEXTURES,numOfPhysicalTextureUnits); that is, the
// system does not currently automatically generate multiple passes when the
// desired number of user textures exceeds the number of physical texture
// units available -- later textures are just not applied.
//
// Also, you cannot have per-vertex color in a bump map for 2 reasons:
//
// 1) Bump mapping fundamentally involves lighting, so you need a material to
//    do it properly.  In Wild Magic, materials are applied per geometry
//    object, not per vertex.
//
// 2) From a practical standpoint, the vertex color element is used by bump
//    mapping to interpolate the light vector.

//----------------------------------------------------------------------------
void OpenGLRenderer::Draw (const BumpMap& rkBumpMap)
{
    if ( !m_bCapDot3BumpMapping )
    {
        // Bump mapping is not supported, then render set of objects without
        // the bump map effect.
        Renderer::Draw(rkBumpMap.GetObjects());
        return;
    }

    if ( m_pkCurrentBumpMap )
    {
        // We do not support nested bump maps for now.  Only one bump map per
        // pass may occur since we need to use the color field to interpolate
        // the light vector.
        Renderer::Draw(rkBumpMap.GetObjects());
        return;
    }

    // Conceptual constness.  The bump map will be told to compute its
    // light vectors on the first Renderer::Draw call.
    m_pkCurrentBumpMap = (BumpMap*)&rkBumpMap;

    // no regular lighting since bump mapping does it
    OverrideLightingMode eOLMTemp = m_eOverrideLightingMode;
    m_eOverrideLightingMode = OLM_DISABLE;

    // In the first pass, we generate the dot(N,L), modulate it by
    // diffuse_light*diffuse_material and add ambient_light*ambient_material
    // (if desired).
    //
    // TO DO.  The colors for the diffuse and ambient components are set on a
    // per-geometry-object basis (Geometry stores the light vectors).  If this
    // is not desired, they could be backed out to here if a bump map was
    // restricted to a single Geometry object.
    m_bOverrideTextureState = true;

    // Ask the renderer to draw the objects.  It will make callbacks to
    // BumpMap::CreateLightVectors to compute the colors for the triangle
    // vertices.  We only bumpmap triangle meshes for now.  We require that
    // smooth shading be active here, but that is already set because it is
    // the default state.
    Renderer::Draw(rkBumpMap.GetObjects());

    // In the second pass, we blend the first pass with the user-set textures
    // in the second pass.
    m_bOverrideTextureState = false;
    m_pkCurrentBumpMap = NULL;

    // set up the blend
    m_bOverrideAlphaState = true;
    rkBumpMap.GetAlphaState()->BlendEnabled() = true;
    SetAlphaState(rkBumpMap.GetAlphaState());

    // do a regular draw 
    Renderer::Draw(rkBumpMap.GetObjects());

    // restore state
    m_bOverrideAlphaState = false;
    rkBumpMap.GetAlphaState()->BlendEnabled() = false;
    SetAlphaState(rkBumpMap.GetAlphaState());
    m_eOverrideLightingMode = eOLMTemp;
}
//----------------------------------------------------------------------------
