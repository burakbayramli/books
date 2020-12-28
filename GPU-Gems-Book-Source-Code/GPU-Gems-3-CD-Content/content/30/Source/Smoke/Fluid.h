//----------------------------------------------------------------------------------
// File:   Fluid.h
// Author: Sarah Tariq and Ignacio Llamas
// Email:  sdkfeedback@nvidia.com
// 
// Copyright (c) 2007 NVIDIA Corporation. All rights reserved.
//
// TO  THE MAXIMUM  EXTENT PERMITTED  BY APPLICABLE  LAW, THIS SOFTWARE  IS PROVIDED
// *AS IS*  AND NVIDIA AND  ITS SUPPLIERS DISCLAIM  ALL WARRANTIES,  EITHER  EXPRESS
// OR IMPLIED, INCLUDING, BUT NOT LIMITED  TO, IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL  NVIDIA OR ITS SUPPLIERS
// BE  LIABLE  FOR  ANY  SPECIAL,  INCIDENTAL,  INDIRECT,  OR  CONSEQUENTIAL DAMAGES
// WHATSOEVER (INCLUDING, WITHOUT LIMITATION,  DAMAGES FOR LOSS OF BUSINESS PROFITS,
// BUSINESS INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR ANY OTHER PECUNIARY LOSS)
// ARISING OUT OF THE  USE OF OR INABILITY  TO USE THIS SOFTWARE, EVEN IF NVIDIA HAS
// BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
//
//
//----------------------------------------------------------------------------------

#ifndef FLUID3D_FLUID_H
#define FLUID3D_FLUID_H

#pragma warning(disable:4995)
#include <fstream>
#include "VolumeRenderer.h"
#include "Grid.h"

class Fluid
{
public:
    enum RENDER_TARGET
    {
        RENDER_TARGET_VELOCITY0,
        RENDER_TARGET_VELOCITY1,
        RENDER_TARGET_PRESSURE,
        RENDER_TARGET_COLOR0,
        RENDER_TARGET_COLOR1,
        RENDER_TARGET_OBSTACLES,
        RENDER_TARGET_OBSTVELOCITY,
        RENDER_TARGET_TEMPSCALAR,
        RENDER_TARGET_TEMPVECTOR,
        NUM_RENDER_TARGETS
    };

    Fluid                   ( ID3D10Device* pd3dDevice );
    virtual ~Fluid          ( void );


    HRESULT Initialize      ( int width, int height, int depth );

    void Update             ( float timestep );
    void Render3D           ( void );
    HRESULT Draw            ( int field );
    void Impulse            ( int x, int y, int z, float dX, float dY, float dZ );
    void Reset              ( void );
    HRESULT SetScreenSize    ( int w, int h );
    int GetTextureWidth     ( void );
    int GetTextureHeight    ( void );
    int GetGridCols         ( void );
    int GetGridRows         ( void );
    float GetMaxDim         ( void ) {return renderer->GetMaxDim();};
    ID3D10Texture3D* GetRenderTarget3D(RENDER_TARGET rt) {return pRenderTargets3D[rt];};
    void SetObstaclePositionInNormalizedGrid( float x, float y, float z );

    // Parameters that may be set from outside
    bool  useBFECC;
    bool  mouseDown;
    int   m_nIterations;

protected:
    Fluid                   ( void ) {};

    HRESULT LoadShaders           ( void );

    void UpdateObstacles          ( void );
    void AdvectColorBFECC         ( float timestep );
    void AdvectColor              ( float timestep );
    void AdvectVelocity           ( float timestep );
    void ApplyVorticityConfinement( float timestep );
    void ApplyExternalForces      ( float timestep );
    void ComputeVelocityDivergence( float timestep );
    void ComputePressure          ( float timestep );
    void ProjectVelocity          ( float timestep );

    //D3D10 helper functions
    HRESULT CreateRenderTargetTextureAndView(int rtIndex, D3D10_TEXTURE3D_DESC TexDesc);
    HRESULT CreateRTTextureAsShaderResource(RENDER_TARGET rtIndex, LPCSTR shaderTextureName,ID3D10Effect* pEffect,D3D10_SHADER_RESOURCE_VIEW_DESC *SRVDesc );
    HRESULT CreateEffect(WCHAR* name, ID3D10Effect **ppEffect, DWORD dwShaderFlags);
    void SetRenderTarget(RENDER_TARGET rtIndex, ID3D10DepthStencilView * optionalDSV = NULL );


    // Internal State
    //===============
   
    // Grid is used to draw quads for all the slices in the 3D simulation grid
    Grid                                *grid;
    
    // VolumeRenderer is used to raycast the smoke density 3D-texture compositing it on top of the scene
    VolumeRenderer                      *renderer;

    ID3D10Device                        *m_pD3DDevice;

    ID3D10Texture3D                     *pRenderTargets3D             [ NUM_RENDER_TARGETS ];
    ID3D10ShaderResourceView            *pRenderTargetShaderViews     [ NUM_RENDER_TARGETS ];
    ID3D10RenderTargetView              *pRenderTargetViews           [ NUM_RENDER_TARGETS ];
    ID3D10EffectShaderResourceVariable  *pShaderResourceVariables     [ NUM_RENDER_TARGETS ];
    DXGI_FORMAT                         RenderTargetFormats           [ NUM_RENDER_TARGETS ]; 

    ID3D10Effect                        *pEffect;
    ID3D10EffectTechnique               *TechniqueAdvect;
    ID3D10EffectTechnique               *TechniqueAdvectBFECC;
    ID3D10EffectTechnique               *TechniqueAdvectVel;
    ID3D10EffectTechnique               *TechniqueVorticity;
    ID3D10EffectTechnique               *TechniqueConfinement;
    ID3D10EffectTechnique               *TechniqueGaussian;
    ID3D10EffectTechnique               *TechniqueDivergence;
    ID3D10EffectTechnique               *TechniqueJacobi;
    ID3D10EffectTechnique               *TechniqueProject;
    ID3D10EffectTechnique               *TechniqueDrawTexture;
    ID3D10EffectTechnique               *TechniqueDrawWhiteTriangles;
    ID3D10EffectTechnique               *TechniqueDrawWhiteLines;
    ID3D10EffectTechnique               *TechniqueDrawBox;

    ID3D10EffectScalarVariable          *TextureWidthShaderVariable;
    ID3D10EffectScalarVariable          *TextureHeightShaderVariable;
    ID3D10EffectScalarVariable          *TextureDepthShaderVariable;
    ID3D10EffectScalarVariable          *DrawTextureShaderVariable;
    ID3D10EffectScalarVariable          *ModulateShaderVariable;
    ID3D10EffectScalarVariable          *ImpulseSizeShaderVariable;
    ID3D10EffectVectorVariable          *ImpulseCenterShaderVariable;
    ID3D10EffectVectorVariable          *SplatColorShaderVariable;
    ID3D10EffectScalarVariable          *EpsilonShaderVariable;
    ID3D10EffectScalarVariable          *TimeStepShaderVariable;
    ID3D10EffectScalarVariable          *ForwardShaderVariable;
    ID3D10EffectVectorVariable          *HalfVolumeDimShaderVariable;

    // Parameters
    float       impulseSize;
    float       saturation;
    float       decay;
    float       confinementScale;
    
    bool        addDensity; //whether to add smoke density or just velocity
    bool        mustUpdateObstacles;

    int         ColorTextureNumber ; // for pingponging color texture

    float       impulseX, impulseY, impulseZ;
    float       impulseDx, impulseDy, impulseDz;

    D3DXVECTOR4 obstPos;
    D3DXVECTOR4 obstPrevPos;
    D3DXVECTOR4 obstVelocity;
};

#endif

