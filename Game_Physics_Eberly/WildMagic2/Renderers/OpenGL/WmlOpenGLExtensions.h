// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLOPENGLEXTENSIONS_H
#define WMLOPENGLEXTENSIONS_H

// These defines are provided here because Apple OpenGL 1.2 does
// not have them in glext.h.  It would be nice to have some support
// like glprocs.{h,c} that exists for Windows and Linux.

// defines for GL_VERSION_1_3
#ifndef GL_COMBINE
#define GL_COMBINE                        0x8570
#endif

#ifndef GL_COMBINE_RGB
#define GL_COMBINE_RGB                    0x8571
#endif

#ifndef GL_COMBINE_ALPHA
#define GL_COMBINE_ALPHA                  0x8572
#endif

#ifndef GL_SOURCE0_RGB
#define GL_SOURCE0_RGB                    0x8580
#endif

#ifndef GL_SOURCE1_RGB
#define GL_SOURCE1_RGB                    0x8581
#endif

#ifndef GL_SOURCE2_RGB
#define GL_SOURCE2_RGB                    0x8582
#endif

#ifndef GL_SOURCE0_ALPHA
#define GL_SOURCE0_ALPHA                  0x8588
#endif

#ifndef GL_SOURCE1_ALPHA
#define GL_SOURCE1_ALPHA                  0x8589
#endif

#ifndef GL_SOURCE2_ALPHA
#define GL_SOURCE2_ALPHA                  0x858A
#endif

#ifndef GL_OPERAND0_RGB
#define GL_OPERAND0_RGB                   0x8590
#endif

#ifndef GL_OPERAND1_RGB
#define GL_OPERAND1_RGB                   0x8591
#endif

#ifndef GL_OPERAND2_RGB
#define GL_OPERAND2_RGB                   0x8592
#endif

#ifndef GL_OPERAND0_ALPHA
#define GL_OPERAND0_ALPHA                 0x8598
#endif

#ifndef GL_OPERAND1_ALPHA
#define GL_OPERAND1_ALPHA                 0x8599
#endif

#ifndef GL_OPERAND2_ALPHA
#define GL_OPERAND2_ALPHA                 0x859A
#endif

#ifndef GL_RGB_SCALE
#define GL_RGB_SCALE                      0x8573
#endif

#ifndef GL_ADD_SIGNED
#define GL_ADD_SIGNED                     0x8574
#endif

#ifndef GL_INTERPOLATE
#define GL_INTERPOLATE                    0x8575
#endif

#ifndef GL_SUBTRACT
#define GL_SUBTRACT                       0x84E7
#endif

#ifndef GL_CONSTANT
#define GL_CONSTANT                       0x8576
#endif

#ifndef GL_PRIMARY_COLOR
#define GL_PRIMARY_COLOR                  0x8577
#endif

#ifndef GL_PREVIOUS
#define GL_PREVIOUS                       0x8578
#endif


// defines for GL_ARB_texture_env_dot3
#ifndef GL_DOT3_RGB_EXT
#define GL_DOT3_RGB_EXT                   0x8740
#endif

#ifndef GL_DOT3_RGBA_EXT
#define GL_DOT3_RGBA_EXT                  0x8741
#endif


// defines for GL_ARB_texture_border_clamp
#ifndef GL_CLAMP_TO_BORDER_ARB
#define GL_CLAMP_TO_BORDER_ARB            0x812D
#endif

// These defines are provided here because Apple OpenGL 1.2 that goes with
// Mac OS 10.2 does not have them in glext.h.

#ifndef glBlendColorEXT
#define glBlendColorEXT                   glBlendColor
#endif

#ifndef glActiveTextureARB
#define glActiveTextureARB                glActiveTexture
#endif

#ifndef glClientActiveTextureARB
#define glClientActiveTextureARB          glClientActiveTexture
#endif

#ifndef GL_MAX_TEXTURE_UNITS_ARB
#define GL_MAX_TEXTURE_UNITS_ARB          GL_MAX_TEXTURE_UNITS
#endif

#ifndef GL_ONE_MINUS_CONSTANT_ALPHA_EXT
#define GL_ONE_MINUS_CONSTANT_ALPHA_EXT   GL_ONE_MINUS_CONSTANT_ALPHA
#endif

#ifndef GL_CONSTANT_ALPHA_EXT
#define GL_CONSTANT_ALPHA_EXT             GL_CONSTANT_ALPHA
#endif

#ifndef GL_TEXTURE0_ARB
#define GL_TEXTURE0_ARB                   GL_TEXTURE0
#endif

#ifndef GL_TEXTURE1_ARB
#define GL_TEXTURE1_ARB                   GL_TEXTURE1
#endif

#ifndef GL_TEXTURE2_ARB
#define GL_TEXTURE2_ARB                   GL_TEXTURE2
#endif

#ifndef GL_TEXTURE3_ARB
#define GL_TEXTURE3_ARB                   GL_TEXTURE3
#endif

#endif

