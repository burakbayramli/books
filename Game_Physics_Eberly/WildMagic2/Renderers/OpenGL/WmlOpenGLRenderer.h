// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLOPENGLRENDERER_H
#define WMLOPENGLRENDERER_H

// WARNING.  The order of inclusion here is important for CodeWarrior 7.0
// on the Macintosh.  In the other order, the MSL header <cmath.macos.h>
// would be included before the Carbon header <fp.h>.  Both headers define
// certain functions (like fabs), but <fp.h> first checks to see if
// <cmath.macos.h> has defined them before it redefines them, but
// <cmath.macos.h> does not check before redefining them.
#include "WmlRenderer.h"
#include "WmlOpenGLIncludes.h"
#include "WmlRendererLibType.h"

namespace Wml
{

class Geometry;

class WML_RENDERER_ITEM OpenGLRenderer : public Renderer
{
    WmlDeclareRTTI;

public:
    // Call this after the rendering context has been initialized.  The order
    // is necessary since this function will call ExtensionsSupported, a call
    // that requires an active rendering context.  If certain capabilities
    // must be established that are specific to a derived class, that class
    // is responsible for implementing its own query function and calling it
    // at the correct time.
    virtual void EstablishCapabilities ();

    virtual void SetBackgroundColor (const ColorRGB& rkColor);
    virtual void DisplayBackBuffer () = 0;

    // Clear the buffer in the full window.
    virtual void ClearBackBuffer ();
    virtual void ClearZBuffer ();
    virtual void ClearStencilBuffer ();
    virtual void ClearBuffers ();

    // Clear the buffer in the specified subwindow.
    virtual void ClearBackBuffer (int iXPos, int iYPos, int iWidth,
        int iHeight);
    virtual void ClearZBuffer (int iXPos, int iYPos, int iWidth,
        int iHeight);
    virtual void ClearStencilBuffer (int iXPos, int iYPos, int iWidth,
        int iHeight);
    virtual void ClearBuffers (int iXPos, int iYPos, int iWidth,
        int iHeight);

    // object drawing
    virtual void Draw (const BumpMap& rkBumpMap);
    virtual void Draw (const GlossMap& rkGlossMap);
    virtual void Draw (const Particles& rkParticle);
    virtual void Draw (const PlanarReflection& rkPReflection);
    virtual void Draw (const PlanarShadow& rkPShadow);
    virtual void Draw (const Wml::Polyline& rkLine);
    virtual void Draw (const Polypoint& rkPoint);
    virtual void Draw (const ProjectedTexture& rkPTexture);
    virtual void Draw (const ScreenPolygon& rkPolygon);
    virtual void Draw (const TriMesh& rkMesh);
    virtual void Draw (const unsigned char* aucBuffer);

    // management of texture resources
    virtual void ReleaseTexture (Texture* pkTexture);

    // driver information
    static const char* GetVendor ();
    static const char* GetRenderer ();
    static const char* GetVersion ();
    static const char* GetGluVersion ();
    static const char* GetExtensions ();
    static bool IsMinimumVersion (int iMajor, int iMinor, int iRelease);

    // Test if a specified extension is supported.  If you need to test for
    // multiple extensions, the function must be called for each extension.
    // The extensions could vary per rendering context, so check for
    // extensions immediately after setting the context.
    static bool ExtensionSupported (const char* acExt);

    // shader management
    virtual void ReleaseShader(Shader* pkShader);

    // shader capability
    bool SupportsShader (const VertexShader* pkVertexShader) const;
    bool SupportsShader (const PixelShader* pkPixelShader) const;

protected:
    // construction
    OpenGLRenderer (int iWidth, int iHeight);

    // state management
    virtual void InitializeState ();
    virtual void SetAlphaState (AlphaState* pkState);
    virtual void SetCullState (CullState* pkState);
    virtual void SetDitherState (DitherState* pkState);
    virtual void SetFogState (FogState* pkState);
    virtual void SetLightState (LightState* pkState);
    virtual void SetMaterialState (MaterialState* pkState);
    virtual void SetPolygonOffsetState (PolygonOffsetState* pkState);
    virtual void SetShadeState (ShadeState* pkState);
    virtual void SetTextureState (TextureState* pkState);
    virtual void SetVertexColorState (VertexColorState* pkState);
    virtual void SetWireframeState (WireframeState* pkState);
    virtual void SetZBufferState (ZBufferState* pkState);

    // note: all of these calls ASSUME that SetFooShader(...) has been
    // called and returns true (i.e. current Foo shader is not null)
    virtual void SetStateConst (float* afData, const Geometry& rkGeom,
        StateConstantType iType, int iTypeNum);
    virtual void SetPixelShaderConst (unsigned int uiRegister,
        float* afData, int iNumOfVector4s);
    virtual void SetPixelShaderConst (ShaderConstants* pkShaderConsts,
        const Geometry& rkGeom);
    virtual void SetVertexShaderConst (unsigned int uiRegister,
        float* afData, int iNumOfVector4s);
    virtual void SetVertexShaderConst (ShaderConstants* pkShaderConsts,
        const Geometry& rkGeom);

    // management of shaders
    virtual void CompileShader(Shader* pkShader);

    // cached matrices
    Matrix4f m_kWorldMatrix;

    // helper routines for setting the current shader
    virtual bool SetCurrentVertexShader (VertexShader* pkShader);
    virtual bool SetCurrentPixelShader (PixelShader* pkShader);

    // generic handler for the geometric primitives
    void DrawPrimitive (const Geometry& rkPrimitive, bool bIs3DPrimitive,
        GLenum eMode, GLsizei iCount, const GLvoid* pvIndices);

    // maps from Magic enums to OpenGL enums
    static GLenum ms_aeAlphaSrcBlend[AlphaState::SBF_QUANTITY];
    static GLenum ms_aeAlphaDstBlend[AlphaState::DBF_QUANTITY];
    static GLenum ms_aeAlphaTest[AlphaState::TF_QUANTITY];
    static GLenum ms_aeFrontFace[CullState::FT_QUANTITY];
    static GLenum ms_aeCullFace[CullState::CT_QUANTITY];
    static GLenum ms_aeFogDensity[FogState::DF_QUANTITY];
    static GLenum ms_aeFogApply[FogState::AF_QUANTITY];
    static GLenum ms_aeShade[ShadeState::SM_QUANTITY];
    static GLenum ms_aeTextureCorrection[Texture::CM_QUANTITY];
    static GLenum ms_aeTextureApply[Texture::AM_QUANTITY];
    static GLenum ms_aeTextureFilter[Texture::FM_QUANTITY];
    static GLenum ms_aeTextureCombineFunc[Texture::ACF_QUANTITY];
    static GLenum ms_aeTextureCombineSrc[Texture::ACS_QUANTITY];
    static GLenum ms_aeTextureCombineOperand[Texture::ACO_QUANTITY];
    static GLfloat ms_afTextureCombineScale[Texture::ACSC_QUANTITY];
    static GLenum ms_aeTextureMipmap[Texture::MM_QUANTITY];
    static GLenum ms_aeImageComponents[Image::IT_QUANTITY];
    static GLenum ms_aeImageFormats[Image::IT_QUANTITY];
    static GLenum ms_aeZBufferCompare[ZBufferState::CF_QUANTITY];
};

WmlSmartPointer(OpenGLRenderer);

}

#endif
