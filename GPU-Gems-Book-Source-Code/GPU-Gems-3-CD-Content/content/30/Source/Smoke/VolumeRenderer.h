//----------------------------------------------------------------------------------
// File:   VolumeRenderer.h
// Author: Ignacio Llamas and Chris (Wei-Tae) Kim and Sarah Tariq 
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

#ifndef FLUID3D_VOLUMERENDERER_H
#define FLUID3D_VOLUMERENDERER_H

#include "Smoke.h"
#include <windows.h>

//does a volume rendering of a box using specified voxel data
class VolumeRenderer
{
public:

    VolumeRenderer(ID3D10Device* pd3dDevice);
    virtual ~VolumeRenderer(void);
    
    HRESULT Initialize(int gridWidth, int gridHeight, int gridDepth);
    HRESULT SetScreenSize(int width, int height);
    void Cleanup(void);
    void Draw(ID3D10ShaderResourceView * pSourceTexSRV);
    const float GetMaxDim(){return maxDim;};

protected:

    HRESULT initShaders(void);
    HRESULT createGridBox(void);
    HRESULT createScreenQuad(void);
    void calculateRenderTextureSize(int screenWidth, int screenHeight);
    HRESULT createRayDataResources(int width, int height);
    HRESULT createJitterTexture();

    void computeRayData(void);
    void computeEdgeTexture(void);
    void drawBox(void);
    void drawScreenQuad(void);

    ID3D10Device                *m_pD3DDevice;

    float gridDim[3];
    float maxDim;

    D3DXMATRIX m_gridMatrix;

    ID3D10Effect                *pEffect;
    ID3D10EffectTechnique       *pTechnique;

    ID3D10EffectShaderResourceVariable  *pRayDataVar;
    ID3D10EffectShaderResourceVariable  *pRayDataSmallVar;
    ID3D10EffectShaderResourceVariable  *pColorTexVar;
    ID3D10EffectShaderResourceVariable  *pRayCastVar;
    ID3D10EffectShaderResourceVariable  *pEdgeVar;

    ID3D10EffectScalarVariable  *pRTWidthVar;
    ID3D10EffectScalarVariable  *pRTHeightVar;
    ID3D10EffectMatrixVariable  *pWorldViewProjectionVar;
    ID3D10EffectMatrixVariable  *pInvWorldViewProjectionVar;
    ID3D10EffectScalarVariable  *pZNearVar;
    ID3D10EffectScalarVariable  *pZFarVar;

    ID3D10EffectScalarVariable  *pGridScaleFactorVar;
    ID3D10EffectVectorVariable  *pEyeOnGridVar;

    ID3D10InputLayout           *pGridBoxLayout;
    ID3D10Buffer                *pGridBoxVertexBuffer;
    ID3D10Buffer                *pGridBoxIndexBuffer;

    ID3D10InputLayout           *pQuadLayout;
    ID3D10Buffer                *pQuadVertexBuffer;

    ID3D10Texture2D             *pRayDataTex2D;
    ID3D10RenderTargetView      *pRayDataRTV;
    ID3D10ShaderResourceView    *pRayDataSRV;
    ID3D10Texture2D             *pRayDataSmallTex2D;
    ID3D10RenderTargetView      *pRayDataSmallRTV;
    ID3D10ShaderResourceView    *pRayDataSmallSRV;
    ID3D10Texture2D             *pRayCastTex2D;
    ID3D10RenderTargetView      *pRayCastRTV;
    ID3D10ShaderResourceView    *pRayCastSRV;
    ID3D10Texture2D             *pEdgeTex2D;
    ID3D10ShaderResourceView    *pEdgeSRV;
    ID3D10RenderTargetView      *pEdgeRTV;

    int renderTextureWidth;
    int renderTextureHeight;
};

#endif
