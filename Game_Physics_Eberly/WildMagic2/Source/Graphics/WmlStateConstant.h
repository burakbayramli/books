// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// ShaderConstants is essentially a table of the information that a renderer
// will need to set user-defined constants.  There is also another piece of
// information (stored in Shader as iTypeNum), which may give some additional
// optional information about the constant (such as transformations for
// matrices and light numbers for lights).
//
// There are plenty more state variables (alpha, culling, etc...) but these
// are either better suited to using a different shader (due to shaders not
// handling conditional statements elegantly) or are not really information
// that would be used in a shader very commonly.  If more need to be added,
// simply add another constant to this giant enum list and then handle that
// case in the SetStateConst() call in the *Renderer classes for the specific
// APIs.

#ifndef WMLSTATECONSTANT_H
#define WMLSTATECONSTANT_H

namespace Wml
{

typedef enum
{
    // Camera variables
    CAMERA_POSITION,                // float4
    CAMERA_UP,                      // float4
    CAMERA_LEFT,                    // float4
    CAMERA_DIRECTION,               // float4

    // Renderer variables (matrices)
    RENDERER_MODVIEWPROJ,           // matrix4
    RENDERER_MODVIEW,               // matrix4
    RENDERER_MOD,                   // matrix4
    RENDERER_PROJ,                  // matrix4

    // Fog state variables
    FOG_COLOR,                      // float4
    FOG_PARAMS,                     // (start, end, density, enabled)

    // Material variables
    MATERIAL_EMISSIVE,              // float4
    MATERIAL_AMBIENT,               // float4
    MATERIAL_DIFFUSE,               // float4
    MATERIAL_SPECULAR,              // float4
    MATERIAL_SHININESS,             // (shiny, undef, undef, undef)

    // Light variables
    LIGHT_POSITION,                 // float4
    LIGHT_DIRECTION,                // float4
    LIGHT_AMBIENT,                  // float4
    LIGHT_DIFFUSE,                  // float4
    LIGHT_SPECULAR,                 // float4
    LIGHT_SPOTCUTOFF,               // (angle, cos, sin, exponent)
    LIGHT_ATTENPARAMS,              // (const, lin, quad, intensity)

    // This flag means that the program definition has within it the numerical
    // constant.  Not all APIs and shader versions handle constants and so you
    // may wish to conditionally handle them within the renderer
    NUMERICAL_CONSTANT,

    // Not updated automatically.  This enum member must be last in the list
    // of enums.
    USER_DEFINED                    // undefined
} StateConstantType;

// Types to pass to the type of state variable in order to change the 
// renderer variables.
enum
{
    SC_NORMAL,
    SC_TRANSPOSE,
    SC_INVERSE,
    SC_INVERSETRANSPOSE
};

class StateConstant
{
public:
    // convenience functions
    static int Size (int i);
    static int Size (StateConstantType iType);
    static const char* Name (int i);
    static const char* Name (StateConstantType iType);
    static bool AllowedInPixelShader (int i);
    static int NumTypes ();

    static const int ms_acSizes[USER_DEFINED];
    static const char ms_aacNames[USER_DEFINED][24];
    static const bool ms_abOKInPixelShader[USER_DEFINED];
    static const int ms_iNumTypes;
};

#include "WmlStateConstant.inl"

}

#endif
