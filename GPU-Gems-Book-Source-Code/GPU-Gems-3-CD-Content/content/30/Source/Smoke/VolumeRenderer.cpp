//----------------------------------------------------------------------------------
// File:   VolumeRenderer.cpp
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

#include "Smoke.h"
#include "SDKmisc.h"
#include "VolumeRenderer.h"
#pragma warning(disable:4995)
#include <iostream>
#include <fstream>
#include <math.h>

using namespace std;

struct VsInput
{
    D3DXVECTOR3 pos;  
};

// constructor
VolumeRenderer::VolumeRenderer(  ID3D10Device* pd3dDevice ) : 
    m_pD3DDevice(NULL), maxDim(0), pEffect(NULL), pTechnique(NULL),
    pRayDataVar(NULL), pRayDataSmallVar(NULL), pColorTexVar(NULL), pRayCastVar(NULL), pEdgeVar(NULL),
    pRTWidthVar(NULL), pRTHeightVar(NULL),
    pWorldViewProjectionVar(NULL), pInvWorldViewProjectionVar(NULL),
    pZNearVar(NULL), pZFarVar(NULL), pGridScaleFactorVar(NULL), pEyeOnGridVar(NULL),
    pGridBoxLayout(NULL), pGridBoxVertexBuffer(NULL), pGridBoxIndexBuffer(NULL), 
    pQuadLayout(NULL), pQuadVertexBuffer(NULL), 
    pRayDataTex2D(NULL), pRayDataRTV(NULL),pRayDataSRV(NULL), 
    pRayDataSmallTex2D(NULL), pRayDataSmallRTV(NULL),pRayDataSmallSRV(NULL), 
    pRayCastTex2D(NULL), pRayCastRTV(NULL), pRayCastSRV(NULL),
    pEdgeTex2D(NULL), pEdgeSRV(NULL), pEdgeRTV(NULL),
    renderTextureWidth(0), renderTextureHeight(0)
{
    SAFE_ACQUIRE(m_pD3DDevice, pd3dDevice);
    
    memset(gridDim,0, sizeof(gridDim));

    D3DXMatrixIdentity(&m_gridMatrix);
}

// destructor
VolumeRenderer::~VolumeRenderer( void )
{ 
    SAFE_RELEASE(m_pD3DDevice);

    Cleanup();
}

void VolumeRenderer::Cleanup( void )
{
    
    SAFE_RELEASE(pEffect);
    
    SAFE_RELEASE(pGridBoxLayout);
    SAFE_RELEASE(pGridBoxVertexBuffer);
    SAFE_RELEASE(pGridBoxIndexBuffer);
    
    SAFE_RELEASE(pQuadLayout);
    SAFE_RELEASE(pQuadVertexBuffer);
    
    SAFE_RELEASE(pRayDataTex2D);
    SAFE_RELEASE(pRayDataRTV);
    SAFE_RELEASE(pRayDataSRV);
    SAFE_RELEASE(pRayDataSmallTex2D);
    SAFE_RELEASE(pRayDataSmallRTV);
    SAFE_RELEASE(pRayDataSmallSRV);
    SAFE_RELEASE(pRayCastTex2D);
    SAFE_RELEASE(pRayCastSRV);
    SAFE_RELEASE(pRayCastRTV);
    SAFE_RELEASE(pEdgeTex2D); 
    SAFE_RELEASE(pEdgeSRV);
    SAFE_RELEASE(pEdgeRTV);
    
}


HRESULT VolumeRenderer::Initialize(int gridWidth, int gridHeight, int gridDepth)
{
    HRESULT hr(S_OK);

    Cleanup();

    gridDim[0] = float(gridWidth);
    gridDim[1] = float(gridHeight);
    gridDim[2] = float(gridDepth);

    maxDim = max( max( gridDim[0], gridDim[1] ), gridDim[2] );

    // Initialize the grid offset matrix
    {
        // Make a scale matrix to scale the unit-sided box to be unit-length on the 
        //  side/s with maximum dimension 
        D3DXMATRIX scaleM;
        D3DXMatrixIdentity(&scaleM);
        D3DXMatrixScaling(&scaleM, gridDim[0] / maxDim, gridDim[1] / maxDim, gridDim[2] / maxDim);
        // offset grid to be centered at origin
        D3DXMATRIX translationM;
        D3DXMatrixTranslation(&translationM, -0.5, -0.5, -0.5);

        m_gridMatrix = translationM * scaleM;
    }

    V_RETURN(initShaders());
    V_RETURN(createGridBox());
    V_RETURN(createScreenQuad());

    V_RETURN(createJitterTexture());

    return hr;
}


void VolumeRenderer::Draw(ID3D10ShaderResourceView * pSourceTexSRV)
{
    pColorTexVar->SetResource(pSourceTexSRV);

    // Set some variables required by the shaders:
    //=========================================================================

    // The near and far planes are used to unproject the scene's z-buffer values
    pZNearVar->SetFloat(g_zNear);
    pZFarVar->SetFloat(g_zFar);

    D3DXMATRIX worldView = g_gridWorld * g_View;

    // The length of one of the axis of the worldView matrix is the length of longest side of the box
    //  in view space. This is used to convert the length of a ray from view space to grid space.
    D3DXVECTOR3 worldXaxis = D3DXVECTOR3(worldView._11, worldView._12, worldView._13);
    float worldScale = D3DXVec3Length(&worldXaxis);
    pGridScaleFactorVar->SetFloat( worldScale );

    // We prepend the current world matrix with this other matrix which adds an offset (-0.5, -0.5, -0.5)
    //  and scale factors to account for unequal number of voxels on different sides of the volume box. 
    // This is because we want to preserve the aspect ratio of the original simulation grid when 
    //  raytracing through it.
    worldView = m_gridMatrix * worldView;

    // worldViewProjection is used to transform the volume box to screen space
    D3DXMATRIX worldViewProjection;
    worldViewProjection = worldView * g_Projection;
    pWorldViewProjectionVar->SetMatrix( (float*)&worldViewProjection );

    // invWorldViewProjection is used to transform positions in the "near" plane into grid space
    D3DXMATRIX invWorldViewProjection;
    D3DXMatrixInverse(&invWorldViewProjection, NULL, &worldViewProjection);
    pInvWorldViewProjectionVar->SetMatrix((float*)&invWorldViewProjection);

    // Compute the inverse of the worldView matrix 
    D3DXMATRIX worldViewInv;
    D3DXMatrixInverse(&worldViewInv, NULL, &worldView);
    // Compute the eye's position in "grid space" (the 0-1 texture coordinate cube)
    D3DXVECTOR4 eyeInGridSpace;
    D3DXVECTOR3 origin(0,0,0);
    D3DXVec3Transform(&eyeInGridSpace, &origin, &worldViewInv);
    pEyeOnGridVar->SetFloatVector((float*)&eyeInGridSpace);

    float color[4] = {0, 0, 0, 0 };


    // Ray cast and render to a temporary buffer
    //=========================================================================

    // Partial init of viewport struct used below
    D3D10_VIEWPORT rtViewport;
    rtViewport.TopLeftX = 0;
    rtViewport.TopLeftY = 0;
    rtViewport.MinDepth = 0;
    rtViewport.MaxDepth = 1;


    // Compute the ray data required by the raycasting pass below.
    //  This function will render to a buffer of float4 vectors, where
    //  xyz is starting position of the ray in grid space
    //  w is the length of the ray in view space
    computeRayData();


    // Do edge detection on this image to find any 
    //  problematic areas where we need to raycast at higher resolution
    computeEdgeTexture();
    

    // Raycast into the temporary render target: 
    //  raycasting is done at the smaller resolution, using a fullscreen quad
    m_pD3DDevice->ClearRenderTargetView( pRayCastRTV, color );
    m_pD3DDevice->OMSetRenderTargets( 1, &pRayCastRTV , NULL ); 

    rtViewport.Width = renderTextureWidth;
    rtViewport.Height = renderTextureHeight;
    m_pD3DDevice->RSSetViewports(1,&rtViewport);

    pRTWidthVar->SetFloat((float)renderTextureWidth);
    pRTHeightVar->SetFloat((float)renderTextureHeight);

    pRayDataSmallVar->SetResource(pRayDataSmallSRV);

    pTechnique->GetPassByName("QuadRaycast")->Apply(0);
    drawScreenQuad();
   

    // Render to the back buffer sampling from the raycast texture that we just created
    //  If and edge was detected at the current pixel we will raycast again to avoid
    //  smoke aliasing artifacts at scene edges
    ID3D10RenderTargetView* pRTV = DXUTGetD3D10RenderTargetView();
    ID3D10DepthStencilView* pDSV = DXUTGetD3D10DepthStencilView();
    m_pD3DDevice->OMSetRenderTargets( 1, &pRTV , pDSV ); 

    rtViewport.Width = g_Width;
    rtViewport.Height = g_Height;
    m_pD3DDevice->RSSetViewports(1,&rtViewport);

    pRTWidthVar->SetFloat((float)g_Width);
    pRTHeightVar->SetFloat((float)g_Height);

    pRayCastVar->SetResource(pRayCastSRV);
    pEdgeVar->SetResource(pEdgeSRV);

    pTechnique->GetPassByName("QuadRaycastCopy")->Apply(0);
    drawScreenQuad();

}

void VolumeRenderer::computeRayData( void )
{
    // Clear the color buffer to 0
    float blackColor[4] = {0, 0, 0, 0 };
    m_pD3DDevice->ClearRenderTargetView(pRayDataRTV, blackColor);
    m_pD3DDevice->OMSetRenderTargets(1, &pRayDataRTV, NULL);
    pEffect->GetVariableByName("sceneDepthTex")->AsShaderResource()->SetResource(g_pSceneDepthSRV);
    
    // Setup viewport to match the window's backbuffer
    D3D10_VIEWPORT rtViewport;
    rtViewport.TopLeftX = 0;
    rtViewport.TopLeftY = 0;
    rtViewport.MinDepth = 0;
    rtViewport.MaxDepth = 1;
    rtViewport.Width = g_Width;
    rtViewport.Height = g_Height;
    m_pD3DDevice->RSSetViewports(1,&rtViewport);
    pRTWidthVar->SetFloat((float)g_Width);
    pRTHeightVar->SetFloat((float)g_Height);

    // Render volume back faces
    // We output xyz=(0,-1,0) and w=min(sceneDepth, boxDepth)
    pTechnique->GetPassByName("CompRayData_Back")->Apply(0);
    drawBox();

    // Render volume front faces using subtractive blending
    // We output xyz="position in grid space" and w=boxDepth,
    //  unless the pixel is occluded by the scene, in which case we output xyzw=(1,0,0,0)
    m_pD3DDevice->OMSetRenderTargets(1, &pRayDataRTV, NULL);
    pTechnique->GetPassByName("CompRayData_Front")->Apply(0);
    drawBox();

}

void VolumeRenderer::computeEdgeTexture(void)
{
    // First setup viewport to match the size of the destination low-res texture
    D3D10_VIEWPORT rtViewport;
    rtViewport.TopLeftX = 0;
    rtViewport.TopLeftY = 0;
    rtViewport.MinDepth = 0;
    rtViewport.MaxDepth = 1;
    rtViewport.Width = renderTextureWidth;
    rtViewport.Height = renderTextureHeight;
    m_pD3DDevice->RSSetViewports(1,&rtViewport);
    pRTWidthVar->SetFloat((float)renderTextureWidth);
    pRTHeightVar->SetFloat((float)renderTextureHeight);

    // Downsample the rayDataTexture to a new small texture, simply using point sample (no filtering)
    m_pD3DDevice->OMSetRenderTargets( 1, &pRayDataSmallRTV , NULL ); 
    pRayDataVar->SetResource(pRayDataSRV);
    pTechnique->GetPassByName("QuadDownSampleRayDataTexture")->Apply(0);
    drawScreenQuad();

    // Create an edge texture, performing edge detection on 'rayDataTexSmall'
    m_pD3DDevice->OMSetRenderTargets( 1, &pEdgeRTV , NULL ); 
    pRayDataSmallVar->SetResource(pRayDataSmallSRV);
    pTechnique->GetPassByName("QuadEdgeDetect")->Apply(0);
    drawScreenQuad();
}

void VolumeRenderer::drawBox(void)
{
    UINT stride = sizeof( VsInput );
    UINT offset = 0;
    m_pD3DDevice->IASetVertexBuffers( 0, 1, &pGridBoxVertexBuffer, &stride, &offset );
    m_pD3DDevice->IASetIndexBuffer( pGridBoxIndexBuffer, DXGI_FORMAT_R32_UINT, 0 );
    m_pD3DDevice->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST );
    m_pD3DDevice->IASetInputLayout(pGridBoxLayout);
    m_pD3DDevice->DrawIndexed(36, 0, 0);
}


void VolumeRenderer::drawScreenQuad(void)
{
    UINT strides = sizeof(VsInput);
    UINT offsets = 0;
    m_pD3DDevice->IASetInputLayout( pQuadLayout );
    m_pD3DDevice->IASetVertexBuffers( 0, 1, &pQuadVertexBuffer, &strides, &offsets );
    m_pD3DDevice->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP );
    m_pD3DDevice->Draw( 4, 0 );
}

HRESULT VolumeRenderer::SetScreenSize( int width, int height )
{
    HRESULT hr;

    V_RETURN(createRayDataResources(width, height));
    return S_OK;
}

// Resource initialization functions below

HRESULT VolumeRenderer::initShaders(  )
{
    HRESULT hr(S_OK);
    DWORD dwShaderFlags = D3D10_SHADER_ENABLE_STRICTNESS;
    WCHAR fullPath[MAX_PATH];
    V_RETURN(NVUTFindDXSDKMediaFileCch( fullPath, MAX_PATH, L"VolumeRenderer.fx" ) );
    V_RETURN(D3DX10CreateEffectFromFile(fullPath, g_pSmokeShadersMacros, NULL, dwShaderFlags, 0, m_pD3DDevice, NULL, NULL, &pEffect, NULL ));

    pTechnique = pEffect->GetTechniqueByName("VolumeRenderer");

    pColorTexVar = pEffect->GetVariableByName("colorTex")->AsShaderResource();

    pZNearVar = pEffect->GetVariableByName("ZNear")->AsScalar();
    pZFarVar = pEffect->GetVariableByName("ZFar")->AsScalar();
    pGridScaleFactorVar = pEffect->GetVariableByName( "gridScaleFactor")->AsScalar();
    pEyeOnGridVar = pEffect->GetVariableByName("eyeOnGrid")->AsVector();
    pWorldViewProjectionVar = pEffect->GetVariableByName("WorldViewProjection")->AsMatrix();
    pInvWorldViewProjectionVar = pEffect->GetVariableByName("InvWorldViewProjection")->AsMatrix();
    pRTWidthVar = pEffect->GetVariableByName("RTWidth")->AsScalar();  
    pRTHeightVar = pEffect->GetVariableByName("RTHeight")->AsScalar();

    D3DXVECTOR3 recGridDim(1.0f/gridDim[0], 1.0f/gridDim[1], 1.0f/gridDim[2]);
    pEffect->GetVariableByName("gridDim")->AsVector()->SetFloatVector(gridDim);
    pEffect->GetVariableByName("recGridDim")->AsVector()->SetFloatVector(recGridDim);
    pEffect->GetVariableByName("maxGridDim")->AsScalar()->SetFloat(maxDim);

    return S_OK;
}

HRESULT VolumeRenderer::createGridBox (void) 
{
    HRESULT hr;

    VsInput vertices[] =
    {
        { D3DXVECTOR3( 0, 0, 0 ) },
        { D3DXVECTOR3( 0, 0, 1 ) },
        { D3DXVECTOR3( 0, 1, 0 ) },
        { D3DXVECTOR3( 0, 1, 1 ) },
        { D3DXVECTOR3( 1, 0, 0 ) },
        { D3DXVECTOR3( 1, 0, 1 ) },
        { D3DXVECTOR3( 1, 1, 0 ) },
        { D3DXVECTOR3( 1, 1, 1 ) },
    };

    D3D10_BUFFER_DESC bd;
    bd.Usage = D3D10_USAGE_DEFAULT;
    bd.ByteWidth = sizeof(vertices);
    bd.BindFlags = D3D10_BIND_VERTEX_BUFFER;
    bd.CPUAccessFlags = 0;
    bd.MiscFlags = 0;
    D3D10_SUBRESOURCE_DATA InitData;
    InitData.pSysMem = vertices;
    V_RETURN( m_pD3DDevice->CreateBuffer( &bd, &InitData, &pGridBoxVertexBuffer ) );


    // Create index buffer
    DWORD indices[] =
    {
        0, 4, 1, 1, 4, 5,
        0, 1, 2, 2, 1, 3,
        4, 6, 5, 6, 7, 5,
        2, 3, 6, 3, 7, 6,
        1, 5, 3, 3, 5, 7,
        0, 2, 4, 2, 6, 4
    };

    bd.Usage = D3D10_USAGE_DEFAULT;
    bd.ByteWidth = sizeof(indices);
    bd.BindFlags = D3D10_BIND_INDEX_BUFFER;
    bd.CPUAccessFlags = 0;
    bd.MiscFlags = 0;
    InitData.pSysMem = indices;
    V_RETURN( m_pD3DDevice->CreateBuffer( &bd, &InitData, &pGridBoxIndexBuffer ) );

    // Define the input layout
    D3D10_INPUT_ELEMENT_DESC layout[] =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D10_INPUT_PER_VERTEX_DATA, 0 },  
    };
    UINT numElements = sizeof(layout)/sizeof(layout[0]);

    // Create the input layout
    D3D10_PASS_DESC PassDesc;
    pTechnique->GetPassByName("CompRayData_Back")->GetDesc( &PassDesc );
    V_RETURN( m_pD3DDevice->CreateInputLayout( layout, numElements, PassDesc.pIAInputSignature, PassDesc.IAInputSignatureSize, &pGridBoxLayout ) );

    return hr;
}


HRESULT VolumeRenderer::createScreenQuad(void ) 
{
    HRESULT hr;
    // Create our quad input layout
    const D3D10_INPUT_ELEMENT_DESC quadlayout[] =
    {
        { "POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D10_INPUT_PER_VERTEX_DATA, 0 },
    };
    UINT numElements = sizeof(quadlayout)/sizeof(quadlayout[0]);

    // Create the input layout
    D3D10_PASS_DESC PassDesc;
    V_RETURN(pTechnique->GetPassByName("QuadRaycast")->GetDesc( &PassDesc ));
    V_RETURN( m_pD3DDevice->CreateInputLayout( quadlayout, numElements, PassDesc.pIAInputSignature, PassDesc.IAInputSignatureSize, &pQuadLayout ) );

    // Create a screen quad for all render to texture operations
    VsInput svQuad[4];
    svQuad[0].pos = D3DXVECTOR3(-1.0f, 1.0f, 0.0f );
    svQuad[1].pos = D3DXVECTOR3(1.0f, 1.0f, 0.0f );
    svQuad[2].pos = D3DXVECTOR3(-1.0f, -1.0f, 0.0f );
    svQuad[3].pos = D3DXVECTOR3(1.0f, -1.0f, 0.0f );

    D3D10_BUFFER_DESC vbdesc =
    {
        4*sizeof(VsInput),
        D3D10_USAGE_DEFAULT,
        D3D10_BIND_VERTEX_BUFFER,
        0,
        0
    };

    D3D10_SUBRESOURCE_DATA InitData;
    InitData.pSysMem = svQuad;
    InitData.SysMemPitch = 0;
    InitData.SysMemSlicePitch = 0;
    V_RETURN( m_pD3DDevice->CreateBuffer( &vbdesc, &InitData, &pQuadVertexBuffer ) );

    return hr;
}

void VolumeRenderer::calculateRenderTextureSize(int screenWidth, int screenHeight)
{
    int maxProjectedSide = int(3.0 * sqrt(3.0)*maxDim);
    int maxScreenDim = max(screenWidth, screenHeight);
    
    float screenAspectRatio = ((float)screenWidth)/screenHeight;

    if( maxScreenDim > maxProjectedSide)
    {
        if(screenHeight > screenWidth)
        {
            renderTextureHeight = maxProjectedSide;
            renderTextureWidth = (int)(screenAspectRatio * maxProjectedSide);
        }
        else
        {
            renderTextureWidth = maxProjectedSide;
            renderTextureHeight = (int)((1.0f/screenAspectRatio) * maxProjectedSide);
        }
    }
    else
    {
        renderTextureWidth = screenWidth;
        renderTextureHeight = screenHeight;
    }
}

HRESULT VolumeRenderer::createRayDataResources( int width, int height )
{
    HRESULT hr;

    SAFE_RELEASE(pRayDataTex2D);
    SAFE_RELEASE(pRayDataSRV);
    SAFE_RELEASE(pRayDataRTV);
    SAFE_RELEASE(pRayDataSmallTex2D);
    SAFE_RELEASE(pRayDataSmallSRV);
    SAFE_RELEASE(pRayDataSmallRTV);
    SAFE_RELEASE(pRayCastTex2D);
    SAFE_RELEASE(pRayCastSRV);
    SAFE_RELEASE(pRayCastRTV);
    SAFE_RELEASE(pEdgeTex2D);
    SAFE_RELEASE(pEdgeSRV);
    SAFE_RELEASE(pEdgeRTV);

    // find a good resolution for raycasting purposes
    calculateRenderTextureSize(width, height);

    DXGI_FORMAT volumeDataFmt = DXGI_FORMAT_R32G32B32A32_FLOAT;

    D3D10_TEXTURE2D_DESC desc;
    desc.ArraySize = 1;
    desc.BindFlags = D3D10_BIND_SHADER_RESOURCE | D3D10_BIND_RENDER_TARGET;
    desc.CPUAccessFlags = 0;
    desc.MipLevels = 1;
    desc.MiscFlags = 0;
    desc.SampleDesc.Count = 1;
    desc.SampleDesc.Quality = 0;
    desc.Usage = D3D10_USAGE_DEFAULT;
    desc.Width = width;
    desc.Height = height;
    desc.Format = volumeDataFmt;
    V_RETURN(m_pD3DDevice->CreateTexture2D(&desc,NULL,&pRayDataTex2D));

    desc.Width = renderTextureWidth;
    desc.Height = renderTextureHeight;
    V_RETURN(m_pD3DDevice->CreateTexture2D(&desc,NULL,&pRayDataSmallTex2D));
    V_RETURN(m_pD3DDevice->CreateTexture2D(&desc,NULL,&pRayCastTex2D));
    
    desc.Format = DXGI_FORMAT_R32_FLOAT;
    V_RETURN(m_pD3DDevice->CreateTexture2D(&desc,NULL,&pEdgeTex2D));

    D3D10_RENDER_TARGET_VIEW_DESC DescRT;
    DescRT.Format = volumeDataFmt;
    DescRT.ViewDimension = D3D10_RTV_DIMENSION_TEXTURE2D;
    DescRT.Texture2D.MipSlice = 0;
    V_RETURN( m_pD3DDevice->CreateRenderTargetView(pRayDataTex2D, &DescRT, &pRayDataRTV));
    V_RETURN( m_pD3DDevice->CreateRenderTargetView(pRayDataSmallTex2D, &DescRT, &pRayDataSmallRTV));
    V_RETURN( m_pD3DDevice->CreateRenderTargetView(pRayCastTex2D, &DescRT, &pRayCastRTV));
    DescRT.Format = DXGI_FORMAT_R32_FLOAT;
    V_RETURN( m_pD3DDevice->CreateRenderTargetView(pEdgeTex2D, &DescRT, &pEdgeRTV));

    D3D10_SHADER_RESOURCE_VIEW_DESC SRVDesc;
    ZeroMemory( &SRVDesc, sizeof(SRVDesc) );
    SRVDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
    SRVDesc.Texture2D.MostDetailedMip = 0;
    SRVDesc.Texture2D.MipLevels = 1;
    SRVDesc.Format = volumeDataFmt;
    V_RETURN(m_pD3DDevice->CreateShaderResourceView(pRayDataTex2D, &SRVDesc, &pRayDataSRV));
    V_RETURN(m_pD3DDevice->CreateShaderResourceView(pRayDataSmallTex2D, &SRVDesc, &pRayDataSmallSRV));
    V_RETURN(m_pD3DDevice->CreateShaderResourceView(pRayCastTex2D, &SRVDesc, &pRayCastSRV));
    SRVDesc.Format = DXGI_FORMAT_R32_FLOAT;
    V_RETURN(m_pD3DDevice->CreateShaderResourceView(pEdgeTex2D, &SRVDesc, &pEdgeSRV));


    pRayDataVar = pEffect->GetVariableByName("rayDataTex")->AsShaderResource();
    pRayDataSmallVar = pEffect->GetVariableByName("rayDataTexSmall")->AsShaderResource();
    pRayCastVar = pEffect->GetVariableByName("rayCastTex")->AsShaderResource();
    pEdgeVar    = pEffect->GetVariableByName("edgeTex")->AsShaderResource();

    return hr;
}

HRESULT VolumeRenderer::createJitterTexture( )
{
    HRESULT hr = S_OK;

    BYTE data[256 * 256];
    for (int i = 0; i < 256 * 256; i++)
    {
        data[i] = (unsigned char) (rand()/float(RAND_MAX)*256);
    }

    D3D10_TEXTURE2D_DESC desc;
    desc.Width = 256;
    desc.Height = 256;
    desc.MipLevels = 1;
    desc.ArraySize = 1;
    desc.Format = DXGI_FORMAT_R8_TYPELESS;
    desc.SampleDesc.Count = 1;
    desc.SampleDesc.Quality = 0;
    desc.Usage = D3D10_USAGE_IMMUTABLE;
    desc.BindFlags = D3D10_BIND_SHADER_RESOURCE;
    desc.CPUAccessFlags = 0;
    desc.MiscFlags = 0;

    D3D10_SUBRESOURCE_DATA dataDesc;
    dataDesc.pSysMem = data;
    dataDesc.SysMemPitch = 256;

    ID3D10Texture2D* NoiseTexture = NULL;
    ID3D10ShaderResourceView* JitterTextureSRV = NULL;

    V( m_pD3DDevice->CreateTexture2D(&desc, &dataDesc, &NoiseTexture) );

    // Create the shader resource view for jittering
    D3D10_SHADER_RESOURCE_VIEW_DESC descSRV;

    ZeroMemory( &descSRV, sizeof(descSRV) );
    descSRV.Format = DXGI_FORMAT_R8_UNORM;
    descSRV.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
    descSRV.Texture2D.MipLevels = 1;
    descSRV.Texture2D.MostDetailedMip = 0;

    V( m_pD3DDevice->CreateShaderResourceView( NoiseTexture, &descSRV, &JitterTextureSRV ) );
    pEffect->GetVariableByName("jitterTex")->AsShaderResource() -> SetResource (JitterTextureSRV);


    SAFE_RELEASE(NoiseTexture);
    SAFE_RELEASE(JitterTextureSRV);
   return hr;
}
