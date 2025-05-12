// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRENDERER_H
#define WMLRENDERER_H

#include "WmlCamera.h"
#include "WmlColorRGB.h"
#include "WmlAlphaState.h"
#include "WmlCullState.h"
#include "WmlDitherState.h"
#include "WmlFogState.h"
#include "WmlLightState.h"
#include "WmlMaterialState.h"
#include "WmlPolygonOffsetState.h"
#include "WmlShadeState.h"
#include "WmlTextureState.h"
#include "WmlVertexColorState.h"
#include "WmlWireframeState.h"
#include "WmlZBufferState.h"
#include "WmlRendererFont.h"
#include "WmlVertexShader.h"
#include "WmlPixelShader.h"

namespace Wml
{

class BumpMap;
class Geometry;
class GlossMap;
class Node;
class Particles;
class PlanarReflection;
class PlanarShadow;
class Polyline;
class Polypoint;
class ProjectedTexture;
class ScreenPolygon;
class TriMesh;


class WML_ITEM Renderer : public Object
{
    WmlDeclareRTTI;

    // The implementation here is to *prevent* you from streaming renderers.
    // If you do try to stream a renderer in debug mode, asserts will be
    // fired.  In release mode, the renderer will not be written to disk.
    WmlDeclareStream;

public:
    virtual ~Renderer ();

    virtual int GetStringWidth (const char* acText) const;
    virtual int GetCharacterWidth (const char cCharacter) const;
    virtual int GetFontHeight () const;

    virtual void Move (int iXPos, int iYPos);
    virtual void Resize (int iWidth, int iHeight);
    virtual void Reshape (int iStartX, int iStartY, int iEndX, int iEndY);
    
    virtual void Activate ();
    virtual void Suspend ();
    virtual void Resume ();

    int GetX() const;
    int GetY() const;
    int GetWidth () const;
    int GetHeight () const;

    CameraPtr SetCamera (Camera* pkCamera);
    CameraPtr GetCamera ();

    void SetState (const RenderStatePtr aspkState[]);

    // SetShaderState(NULL) will disable all shaders.  Otherwise, it will set
    // the shader based on the shaders attached (or unset, if none attached)
    // to the piece of geometry.
    //
    // You should only call SetShaderConstants with the same geometry that
    // you called SetShaderState.  This also implies that you must call this
    // routine after SetShaderState, if you call it at all.
    void SetShaderState (const Geometry* pkGeom);
    void SetShaderConstants (const Geometry* pkGeom);

    virtual bool BeginScene ();
    virtual void EndScene ();
    virtual void ShowCursor (bool bShow);
    virtual void ToggleFullScreen (int& riNewWidth, int& riNewHeight);
    virtual bool LoadFont(const RendererFont& rkFont);
    virtual void UnloadFont(const RendererFont& rkFont);

    virtual void SetBackgroundColor (const ColorRGB& rkColor);
    const ColorRGB& GetBackgroundColor () const;
    virtual void DisplayBackBuffer () = 0;

    // Clear the buffer in the full window.
    virtual void ClearBackBuffer () = 0;
    virtual void ClearZBuffer () = 0;
    virtual void ClearStencilBuffer () = 0;
    virtual void ClearBuffers () = 0;

    // Clear the buffer in the specified subwindow.
    virtual void ClearBackBuffer (int iXPos, int iYPos, int iWidth,
        int iHeight) = 0;
    virtual void ClearZBuffer (int iXPos, int iYPos, int iWidth,
        int iHeight) = 0;
    virtual void ClearStencilBuffer (int iXPos, int iYPos, int iWidth,
        int iHeight) = 0;
    virtual void ClearBuffers (int iXPos, int iYPos, int iWidth,
        int iHeight) = 0;

    // capabilities
    bool SupportsMultitexture () const;
    bool SupportsSpecularAfterTexture () const;
    bool SupportsPlanarReflection () const;
    bool SupportsPlanarShadow () const;
    bool SupportsTextureClampToBorder () const;
    bool SupportsTextureApplyAdd () const;
    bool SupportsTextureApplyCombine () const;
    bool SupportsTextureApplyCombineDot3 () const;
    bool SupportsDot3BumpMapping () const;
    // These two are true if and only if the renderer supports that
    // kind of shader (at all)
    bool SupportsVertexShader () const;
    bool SupportsPixelShader () const;
    // These two are true if and only if the renderer supports that
    // specific shader (and its version)
    virtual bool SupportsShader (const VertexShader* pkShader) const;
    virtual bool SupportsShader (const PixelShader* pkShader) const;

    // drawing calls for applications
    virtual void Draw (Node* pkScene);
    void Draw (ScreenPolygon* pkScreenPolygon);
    virtual void Draw (int iX, int iY, const ColorRGB& rkColor,
        const char* acText);
    virtual void Draw (int iX, int iY, const RendererFont& rkFont,
        const char* acText);
    virtual void Draw (const unsigned char* aucBuffer);

    //*** object drawing (internal use)
    virtual void Draw (const BumpMap& rkBumpMap);
    virtual void Draw (const GlossMap& rkGlossMap);
    virtual void Draw (const Particles& rkParticles);
    virtual void Draw (const PlanarReflection& rkPReflection);
    virtual void Draw (const PlanarShadow& rkPShadow);
    virtual void Draw (const Polyline& rkPolyline);
    virtual void Draw (const Polypoint& rkPolypoint);
    virtual void Draw (const ProjectedTexture& rkPTexture);
    virtual void Draw (const ScreenPolygon& rkScreenPolygon);
    virtual void Draw (const TriMesh& rkTriMesh);
    //***

    // support for searching by name
    virtual Object* GetObjectByName (const char* acName);
    virtual void GetAllObjectsByName (const char* acName,
        std::vector<Object*>& rkObjects);

    // management of texture resources
    int GetMaxTextureUnits () const;
    virtual void ReleaseTexture (Texture* pkTexture);
    void ReleaseTextures (Spatial* pkScene);

    // management of shader resources
    void ReleaseShaders (Spatial* pkScene);

protected:
    // abstract base class
    Renderer (int iWidth, int iHeight);
    Renderer ();

    // state management
    virtual void InitializeState ();
    virtual void SetAlphaState (AlphaState* pkState) = 0;
    virtual void SetCullState (CullState* pkState) = 0;
    virtual void SetDitherState (DitherState* pkState) = 0;
    virtual void SetFogState (FogState* pkState) = 0;
    virtual void SetLightState (LightState* pkState) = 0;
    virtual void SetMaterialState (MaterialState* pkState) = 0;
    virtual void SetPolygonOffsetState (PolygonOffsetState* pkState) = 0;
    virtual void SetShadeState (ShadeState* pkState) = 0;
    virtual void SetTextureState (TextureState* pkState) = 0;
    virtual void SetVertexColorState (VertexColorState* pkState) = 0;
    virtual void SetWireframeState (WireframeState* pkState) = 0;
    virtual void SetZBufferState (ZBufferState* pkState) = 0;

    // Set the current shader and return true if the setting was
    // successful (capabilities met, shader is not null, shader compiled)
    // Setting the current shader to NULL will disable that type
    // of shader.
    virtual bool SetCurrentVertexShader (VertexShader* pkShader);
    virtual bool SetCurrentPixelShader (PixelShader* pkShader);

    // shader management
    virtual void ReleaseShader (Shader* pkShader);

    // shader constant setting routines.
    virtual void SetPixelShaderConst (ShaderConstants* pkShaderConsts,
        const Geometry& rkGeom);
    virtual void SetVertexShaderConst (ShaderConstants* pkShaderConsts,
        const Geometry& rkGeom);
    virtual void SetStateConst (float* afData, const Geometry& rkGeom,
        StateConstantType iType, int iTypeNum);

    // window parameters
    int m_iX, m_iY, m_iWidth, m_iHeight, m_iQuantity;
    ColorRGB m_kBackgroundColor;

    // camera for establishing view frustum
    CameraPtr m_spkCamera;

    // renderer capabilities
    bool m_bCapMultitexture;
    bool m_bCapSpecularAfterTexture;
    bool m_bCapPlanarReflection;
    bool m_bCapPlanarShadow;
    bool m_bCapTextureClampToBorder;
    bool m_bCapTextureApplyAdd;
    bool m_bCapTextureApplyCombine;
    bool m_bCapTextureApplyCombineDot3;
    bool m_bCapDot3BumpMapping;
    Shader::ShaderVersion m_iCapVertShaderVersion;
    Shader::ShaderVersion m_iCapPixShaderVersion;

    // For overriding render state setting by render effects.
    bool m_bOverrideState;  // PlanarShadow
    bool m_bReverseCullState;  // PlanarReflection
    bool m_bDrawingReflected;  // PlanarReflection
    bool m_bOverrideAlphaState;  // GlossMap, BumpMap
    bool m_bOverrideTextureState;  // BumpMap

    enum OverrideLightingMode
    {
        OLM_NONE,
        OLM_DISABLE,
        OLM_ONLY_SPECULAR,
        OLM_ONLY_NON_SPECULAR
    };
    OverrideLightingMode m_eOverrideLightingMode;  // GlossMap, BumpMap

    BumpMap* m_pkCurrentBumpMap;

    // current shaders (null if not enabled);
    PixelShader* m_pkCurPixelShader;
    VertexShader* m_pkCurVertexShader;

    // Allow render effects to request specific texture units.  This allows
    // the renderer, developer, or user to choose the precendence of model
    // textures and render effects.  Render effects request texture units from
    // highest to lowest to attempt to avoid conflicts with model textures.
    // The assumption is that user models will try to use texture units
    // starting with unit 0.  Render effects will use texture units starting
    // with unit MaxTextureUnits.
    //
    // RequestTextureUnit will return the highest available texture unit.  If
    // it returns -1, all were requested already by render effects in this
    // branch of the scene graph, and the render effect must choose to either
    // abandon or override another render effect by taking a unit.
    int RequestTextureUnit (int iUnit = -1);
    bool TextureUnitRequested (int iUnit);
    void ReleaseTextureUnit (int iUnit);

    // bit vector storing the requests for texture units
    int m_iTextureUnitRequested;

    // The default value is zero.  The derived-class renderer must set this
    // value to the actual number of supported texture units.
    int m_iMaxTextureUnits;

    // management of texture resources
    friend class Texture;
    // management of shader resources
    friend class Shader;

    class WML_ITEM List
    {
    public:
        Renderer* m_pkRenderer;
        List* m_pkNext;
    };

    static void OnDestroyTexture (Texture* pkTexture);
    static void OnDestroyShader (Shader* pkShader);
    static List* ms_pkRendererList;
};

WmlSmartPointer(Renderer);
#include "WmlRenderer.inl"

}

#endif
