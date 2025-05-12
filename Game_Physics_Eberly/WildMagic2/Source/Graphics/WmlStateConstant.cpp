// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlStateConstant.h"
using namespace Wml;

const int StateConstant::ms_iNumTypes = USER_DEFINED;

const int StateConstant::ms_acSizes[StateConstant::ms_iNumTypes] = 
{
    // Camera variables
    1,      // CAMERA_POSITION
    1,      // CAMERA_UP
    1,      // CAMERA_LEFT
    1,      // CAMERA_DIRECTION

    // Renderer variables (matrices)
    4,      // RENDERER_MODVIEWPROJ
    4,      // RENDERER_MODVIEW
    4,      // RENDERER_MOD
    4,      // RENDERER_PROJ

    // Fog state variables
    1,      // FOG_COLOR
    1,      // FOG_PARAMS

    // Material variables
    1,      // MATERIAL_EMISSIVE
    1,      // MATERIAL_AMBIENT
    1,      // MATERIAL_DIFFUSE
    1,      // MATERIAL_SPECULAR
    1,      // MATERIAL_SHININESS

    // Light variables
    1,      // LIGHT_POSITION
    1,      // LIGHT_DIRECTION
    1,      // LIGHT_AMBIENT
    1,      // LIGHT_DIFFUSE
    1,      // LIGHT_SPECULAR
    1,      // LIGHT_SPOTCUTOFF
    1       // LIGHT_ATTENPARAMS
};

const bool StateConstant::ms_abOKInPixelShader[StateConstant::ms_iNumTypes] =
{
    // Camera variables
    true,   // CAMERA_POSITION
    true,   // CAMERA_UP
    true,   // CAMERA_LEFT
    true,   // CAMERA_DIRECTION

    // Renderer variables (matrices)
    false,  // RENDERER_MODVIEWPROJ
    false,  // RENDERER_MODVIEW
    false,  // RENDERER_MOD
    false,  // RENDERER_PROJ

    // Fog state variables
    true,   // FOG_COLOR
    true,   // FOG_PARAMS

    // Material variables
    true,   // MATERIAL_EMISSIVE
    true,   // MATERIAL_AMBIENT
    true,   // MATERIAL_DIFFUSE
    true,   // MATERIAL_SPECULAR
    true,   // MATERIAL_SHININESS

    // Light variables
    true,   // LIGHT_POSITION
    true,   // LIGHT_DIRECTION
    true,   // LIGHT_AMBIENT
    true,   // LIGHT_DIFFUSE
    true,   // LIGHT_SPECULAR
    true,   // LIGHT_SPOTCUTOFF
    true,   // LIGHT_ATTENPARAMS
};

const char StateConstant::ms_aacNames[StateConstant::ms_iNumTypes][24] =
{
    "WmlCameraPosition",
    "WmlCameraUp",
    "WmlCameraLeft",
    "WmlCameraDirection",

    "WmlRendererModViewProj",
    "WmlRendererModView",
    "WmlRendererMod",
    "WmlRendererProj",

    "WmlFogColor",
    "WmlFogParams",

    "WmlMaterialEmissive",
    "WmlMaterialAmbient",
    "WmlMaterialDiffuse",
    "WmlMaterialSpecular",
    "WmlMaterialShininess",

    "WmlLightPosition",
    "WmlLightDirection",
    "WmlLightAmbient",
    "WmlLightDiffuse",
    "WmlLightSpecular",
    "WmlLightSpotcutoff",
    "WmlLightAttenparams"
};
