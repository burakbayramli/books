//----------------------------------------------------------------------------------
// File:   Voxelizer.cpp
// Author: Ignacio Llamas
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
#include "Voxelizer.h"
#include <stddef.h>


Voxelizer::Voxelizer(void) : m_pD3DDevice(NULL), m_pDstInOutTexture3D(NULL), m_pDstInOutTexRTView(NULL),
    m_pDstVelTexture3D(NULL), m_pDstVelTexRTView(NULL),
    m_width(0), m_height(0), m_depth(0), m_cols(0), m_rows(0), m_initialized(false), 
    m_pSkinnedMesh(NULL), m_timeStep(1.0),
    m_pDSTex2D(NULL), m_pDSTex2DDSView(NULL), m_pDSTex2DSRView(NULL), 
    m_pVoxEffect(NULL), m_pNZTech(NULL), m_pResolveWithPSTech(NULL), m_pGenVelocityWireframeTech(NULL),
    m_pWorldViewProjectionVar(NULL), m_pDSTex2DSRVar(NULL),
    m_pSlicesLayout(NULL), m_pSlicesVB(NULL)
{
    D3DXMatrixIdentity(&m_objToVolumeXForm);
}

Voxelizer::~Voxelizer(void)
{
    Cleanup();
}

void Voxelizer::Cleanup(void)
{
    SAFE_RELEASE(m_pD3DDevice);

    SAFE_RELEASE(m_pDstInOutTexture3D);
    SAFE_RELEASE(m_pDstInOutTexRTView);
    SAFE_RELEASE(m_pDstVelTexture3D);
    SAFE_RELEASE(m_pDstVelTexRTView);

    m_initialized = false;

    SAFE_RELEASE(m_pDSTex2D);
    SAFE_RELEASE(m_pDSTex2DDSView);
    SAFE_RELEASE(m_pDSTex2DSRView);

    SAFE_RELEASE(m_pVoxEffect);

    m_pNZTech = NULL;
    m_pResolveWithPSTech = NULL;
    m_pGenVelocityWireframeTech = NULL;
    m_pWorldViewProjectionVar = NULL;
    m_pDSTex2DSRVar = NULL;

    SAFE_RELEASE(m_pSlicesLayout);
    SAFE_RELEASE(m_pSlicesVB);

    SAFE_RELEASE(m_pSOInputLayout);
}

HRESULT Voxelizer::SetDestination(ID3D10Device *pD3DDevice, 
        ID3D10Texture3D *pDstInOutTexture3D, ID3D10Texture3D *pDstVelocityTexture3D)
{
    SAFE_ACQUIRE(m_pD3DDevice, pD3DDevice);
    SAFE_ACQUIRE(m_pDstInOutTexture3D, pDstInOutTexture3D);
    SAFE_ACQUIRE(m_pDstVelTexture3D, pDstVelocityTexture3D);

    return Initialize();
}

HRESULT Voxelizer::Initialize(void)
{
    HRESULT hr(S_OK);

    m_initialized = false;
    SAFE_RELEASE(m_pDstInOutTexRTView);
    SAFE_RELEASE(m_pDstVelTexRTView);

    // Assert inputs are valid
    assert(m_pD3DDevice);
    assert(m_pDstInOutTexture3D != NULL);

    {
        // Create a rendertarget view for the InOut 3D texture
        D3D10_TEXTURE3D_DESC tex3Ddesc;
        m_pDstInOutTexture3D->GetDesc(&tex3Ddesc);
        D3D10_RENDER_TARGET_VIEW_DESC rtDesc;
        rtDesc.Format = tex3Ddesc.Format;
        rtDesc.ViewDimension = D3D10_RTV_DIMENSION_TEXTURE3D;
        rtDesc.Texture3D.MipSlice = 0;
        rtDesc.Texture3D.FirstWSlice = 0;
        rtDesc.Texture3D.WSize = tex3Ddesc.Depth;
        V_RETURN(m_pD3DDevice->CreateRenderTargetView(m_pDstInOutTexture3D, &rtDesc, &m_pDstInOutTexRTView));

        // Get witdh, height and depth
        m_width = tex3Ddesc.Width;
        m_height = tex3Ddesc.Height;
        m_depth = tex3Ddesc.Depth;

        ComputeRowColsForFlat3DTexture(m_depth, &m_cols, &m_rows);
    }

    assert((m_width > 0) && (m_height > 0) && (m_depth > 0));
    assert((m_cols > 0) && (m_rows > 0));
    assert((m_cols * m_rows) >= m_depth);

    if( m_pDstVelTexture3D )
    {
        // Create a rendertarget view for the Velocity 3D texture
        D3D10_TEXTURE3D_DESC velTex3Ddesc;
        m_pDstVelTexture3D->GetDesc(&velTex3Ddesc);

        // Make sure both destination textures have the same dimensions
        if((velTex3Ddesc.Width != m_width) || (velTex3Ddesc.Height != m_height) ||
            (velTex3Ddesc.Depth != m_depth))
        {
            Cleanup();
            return E_INVALIDARG;
        }

        D3D10_RENDER_TARGET_VIEW_DESC velRtDesc;
        velRtDesc.Format = velTex3Ddesc.Format;
        velRtDesc.ViewDimension = D3D10_RTV_DIMENSION_TEXTURE3D;
        velRtDesc.Texture3D.MipSlice = 0;
        velRtDesc.Texture3D.FirstWSlice = 0;
        velRtDesc.Texture3D.WSize = velTex3Ddesc.Depth;
        V_RETURN(m_pD3DDevice->CreateRenderTargetView(m_pDstVelTexture3D, &velRtDesc, &m_pDstVelTexRTView));

    }

    // Initialize internal texture resources
    hr = InitTextures();
    if(FAILED(hr))
    {
        Cleanup();
        return hr;
    }

    // Load Voxelizer.fx, and get techniques and variables to use (if needed)
    hr = InitShaders();
    if(FAILED(hr))
    {
        Cleanup();
        return hr;
    }

    // Init vertex buffer for a m_depth quads (to convert a "flat 3D texture" to a "3D texture");
    hr = InitSlices();
    if(FAILED(hr))
    {
        Cleanup();
        return hr;
    }

    // create input layout for use with streamout buffers from SkinnedMesh
    {
        const D3D10_INPUT_ELEMENT_DESC inputElemDesc[] =
        {
            { "POSITION",  0, DXGI_FORMAT_R32G32B32_FLOAT, 0, D3D10_APPEND_ALIGNED_ELEMENT, D3D10_INPUT_PER_VERTEX_DATA, 0 },
            { "POSITION",  1, DXGI_FORMAT_R32G32B32_FLOAT, 1, D3D10_APPEND_ALIGNED_ELEMENT, D3D10_INPUT_PER_VERTEX_DATA, 0 },
        };
        UINT numElements = sizeof(inputElemDesc)/sizeof(inputElemDesc[0]);

        D3D10_PASS_DESC passDesc;
        m_pGenVelocityWireframeTech->GetPassByIndex(0)->GetDesc(&passDesc);
        V_RETURN(m_pD3DDevice->CreateInputLayout(inputElemDesc, numElements, 
            passDesc.pIAInputSignature, passDesc.IAInputSignatureSize, &m_pSOInputLayout));
    }


    m_initialized = true;
    return hr;
}

HRESULT Voxelizer::InitTextures(void)
{
    HRESULT hr(S_OK);

    // release the textures if they were allocated before
    SAFE_RELEASE(m_pDSTex2D);
    SAFE_RELEASE(m_pDSTex2DDSView);
    SAFE_RELEASE(m_pDSTex2DSRView);

    // create DXGI_FORMAT_R24G8_TYPELESS depth-stencil buffer and view
    D3D10_TEXTURE2D_DESC dsTexDesc;
    dsTexDesc.Width = m_width * m_cols;
    dsTexDesc.Height = m_height * m_rows;
    dsTexDesc.MipLevels = 1;
    dsTexDesc.ArraySize = 1;
    dsTexDesc.Format = DXGI_FORMAT_R24G8_TYPELESS;
    dsTexDesc.SampleDesc.Count = 1;
    dsTexDesc.SampleDesc.Quality = 0;
    dsTexDesc.Usage = D3D10_USAGE_DEFAULT;
    dsTexDesc.BindFlags = D3D10_BIND_DEPTH_STENCIL | D3D10_BIND_SHADER_RESOURCE;
    dsTexDesc.CPUAccessFlags = 0;
    dsTexDesc.MiscFlags = 0;
    V_RETURN(m_pD3DDevice->CreateTexture2D( &dsTexDesc, NULL, &m_pDSTex2D ));

    // Create the depth stencil view
    D3D10_DEPTH_STENCIL_VIEW_DESC dsViewDesc;
    dsViewDesc.Format = DXGI_FORMAT_D24_UNORM_S8_UINT;
    dsViewDesc.ViewDimension = D3D10_DSV_DIMENSION_TEXTURE2D;
    dsViewDesc.Texture2D.MipSlice = 0;
    V_RETURN(m_pD3DDevice->CreateDepthStencilView( m_pDSTex2D, &dsViewDesc, &m_pDSTex2DDSView ));

    // Create the shader resource view for the depth-stencil buffer
    D3D10_SHADER_RESOURCE_VIEW_DESC srvDesc;
    srvDesc.Format = DXGI_FORMAT_X24_TYPELESS_G8_UINT;
    srvDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
    srvDesc.Texture2D.MipLevels = 1;
    srvDesc.Texture2D.MostDetailedMip = 0;
    
    V_RETURN(m_pD3DDevice->CreateShaderResourceView(m_pDSTex2D, &srvDesc, &m_pDSTex2DSRView));
    
    return hr;
}

HRESULT Voxelizer::InitShaders(void)
{
    HRESULT hr(S_OK);
    WCHAR fullPath[MAX_PATH];

    if(m_pVoxEffect != NULL)
        return hr;

    V_RETURN(NVUTFindDXSDKMediaFileCch( fullPath, MAX_PATH, L"Voxelizer.fx" ));
    V_RETURN(D3DX10CreateEffectFromFile(fullPath, g_pSmokeShadersMacros, NULL, NULL, 0, m_pD3DDevice, 
        NULL, NULL, &m_pVoxEffect, NULL ));

    m_pNZTech = m_pVoxEffect->GetTechniqueByName( "VoxelizeNZ" );
    m_pResolveWithPSTech = m_pVoxEffect->GetTechniqueByName( "VoxelizeResolveWithPS" );
    m_pGenVelocityWireframeTech = m_pVoxEffect->GetTechniqueByName( "GenVelocityWireframe" );

    m_pWorldViewProjectionVar = m_pVoxEffect->GetVariableByName("WorldViewProjection")->AsMatrix();
    m_pDSTex2DSRVar = m_pVoxEffect->GetVariableByName("stencilbufferTex2D")->AsShaderResource();

    assert(m_pNZTech && m_pResolveWithPSTech && m_pGenVelocityWireframeTech &&
        m_pWorldViewProjectionVar && m_pDSTex2DSRVar);
    
    return hr;
}

struct SliceVertex
{
    D3DXVECTOR3 pos;
    D3DXVECTOR3 tex;
};

HRESULT Voxelizer::InitSlices(void)
{
    HRESULT hr(S_OK);

    if( m_pSlicesVB != NULL )
    {
        assert(m_pSlicesLayout != NULL);
        return hr;
    }

    // Create full-screen quad input layout
    const D3D10_INPUT_ELEMENT_DESC slicesLayout[] =
    {
        { "Position", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0, D3D10_INPUT_PER_VERTEX_DATA, 0 },
        { "Texcoord", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12, D3D10_INPUT_PER_VERTEX_DATA, 0 },
    };
    UINT numElements = sizeof(slicesLayout)/sizeof(slicesLayout[0]);

    // Create the input layout
    D3D10_PASS_DESC passDesc;
    V_RETURN(m_pResolveWithPSTech->GetPassByIndex(0)->GetDesc( &passDesc ));

    V_RETURN(m_pD3DDevice->CreateInputLayout( slicesLayout, numElements, 
        passDesc.pIAInputSignature, passDesc.IAInputSignatureSize, &m_pSlicesLayout ));

#define SLICEQUAD_VTXCNT 6
    
    // Create a vertex buffers of quads, one per slice, wit texcoords to lookup from a flat 3D texture
    //  and with homogenous coordinates to cover a fullscreen quad
    SliceVertex *slicesVertices(NULL);
    try {
         slicesVertices = new SliceVertex[SLICEQUAD_VTXCNT*m_depth];
    }
    catch(...) {
        return E_OUTOFMEMORY;
    }

    SliceVertex sliceVtx[4];
    int row, col;
    float x, y;
    int vtxIdx = 0;
    for(UINT z= 0; z<m_depth; z++) {
        row = z / m_cols;
        col = z % m_cols;
        x = float(col) * m_width;
        y = float(row) * m_height;
        vtxIdx = z*SLICEQUAD_VTXCNT;

        sliceVtx[0].pos = D3DXVECTOR3(-1.0f, 1.0f, 0.5f);
        sliceVtx[0].tex = D3DXVECTOR3(x, y, float(z));
        
        sliceVtx[1].pos = D3DXVECTOR3(-1.0f, -1.0f, 0.5f);
        sliceVtx[1].tex = D3DXVECTOR3(x, y+m_height, float(z));
        
        sliceVtx[2].pos = D3DXVECTOR3(1.0f, -1.0f, 0.5f);
        sliceVtx[2].tex = D3DXVECTOR3(x+m_width, y+m_height, float(z));
        
        sliceVtx[3].pos = D3DXVECTOR3(1.0f, 1.0f, 0.5f);
        sliceVtx[3].tex = D3DXVECTOR3(x+m_width, y, float(z));

        slicesVertices[vtxIdx+0] = sliceVtx[0];
        slicesVertices[vtxIdx+1] = sliceVtx[1];
        slicesVertices[vtxIdx+2] = sliceVtx[2];
        slicesVertices[vtxIdx+3] = sliceVtx[0];
        slicesVertices[vtxIdx+4] = sliceVtx[2];
        slicesVertices[vtxIdx+5] = sliceVtx[3];
    }

    D3D10_BUFFER_DESC vbDesc =
    {
        SLICEQUAD_VTXCNT*m_depth*sizeof(SliceVertex),
        D3D10_USAGE_DEFAULT,
        D3D10_BIND_VERTEX_BUFFER,
        0,
        0
    };

    D3D10_SUBRESOURCE_DATA initialData;
    initialData.pSysMem = slicesVertices;
    initialData.SysMemPitch = 0;
    initialData.SysMemSlicePitch = 0;
    V_RETURN(m_pD3DDevice->CreateBuffer( &vbDesc, &initialData, &m_pSlicesVB ));

    delete []slicesVertices;

    return hr;
}

void Voxelizer::DrawSlices(void)
{
    assert(m_pSlicesLayout);
    assert(m_pSlicesVB);

    UINT strides = sizeof(SliceVertex);
    UINT offsets = 0;

    m_pD3DDevice->IASetInputLayout( m_pSlicesLayout );
    m_pD3DDevice->IASetVertexBuffers( 0, 1, &m_pSlicesVB, &strides, &offsets );
    m_pD3DDevice->IASetPrimitiveTopology( D3D10_PRIMITIVE_TOPOLOGY_TRIANGLELIST );

    m_pD3DDevice->Draw(SLICEQUAD_VTXCNT*m_depth, 0);
}


HRESULT Voxelizer::Voxelize(D3DXMATRIX& objToVolumeXForm, SkinnedMesh *pMesh,
                            int srcSoBuf, int prevSrcSoBuf, float timeStep)
{
    m_objToVolumeXForm = objToVolumeXForm;
    m_pSkinnedMesh = pMesh;
    m_srcSoBuffer = srcSoBuf;
    m_prevSrcSoBuffer = prevSrcSoBuf;
    m_timeStep = timeStep;

    HRESULT hr = DoVoxelization();

    m_pSkinnedMesh = NULL;
    
    return hr;
}

HRESULT Voxelizer::DoVoxelization(void)
{
    HRESULT hr(S_OK);

    // Do the actual voxelization
    hr = StencilClipVolume();
    if(!FAILED(hr) && m_pDstVelTexture3D) {
        hr = VoxelizeVelocity();
    }

    m_pD3DDevice->ClearState();

    return hr;
}

//
// StencilClipVolume algorithm summary:
// ====================================
// 
// function DrawClippedMesh():
//   set a vertex shader that transforms the vertices to the volume space
//   set stencil functions for NZ rule (incr on back and decr on front)
//   set the near plane to be aligned with each slice 
//   render the mesh
// done
//
// function StencilClipVolume():
//   clear depthstencil buffer
//   bind as rendertarget a NULL color buffer and the depthstencil buffer
//
//   for each slice:
//     set the viewport and cliprect to match the slice's 2D region
//     DrawClippedMesh
//   done
//
//   bind as rendertarget the 3D-texture and set viewport and scissor to match a slice
//   do a resolve pass to write out 1 for inside voxels on each slice in the 3D-texture
// done

HRESULT Voxelizer::StencilClipVolume(void)
{
    HRESULT hr(S_OK);
    int x, y;

    assert(m_initialized);
    
    // clear depthstencil buffer to 0
    m_pD3DDevice->ClearDepthStencilView(m_pDSTex2DDSView, D3D10_CLEAR_DEPTH|D3D10_CLEAR_STENCIL, 0, 0);        
    // set the depthstencil buffer as rendertarget (no color buffer)
    m_pD3DDevice->OMSetRenderTargets(0, NULL, m_pDSTex2DDSView);
    for( UINT z=0; z<m_depth; z++)
    {
        // compute x and y coordinates for the TOP-LEFT corner of the slice in the flat 3D texture
        x = (z % m_cols) * m_width;
        y = (z / m_cols) * m_height;

        // set viewport and scissor to match the size of single slice
        D3D10_VIEWPORT viewport = { x, y, m_width, m_height, 0.0f, 1.0f };
        m_pD3DDevice->RSSetViewports(1, &viewport);
        D3D10_RECT scissorRect = { x, y, x+m_width, y+m_height };
        m_pD3DDevice->RSSetScissorRects(1, &scissorRect);

        V_RETURN(RenderClippedMesh((float)z/m_depth - 0.5f, 100000.0f, m_pNZTech));
    }

    // set texture as rendertarget
    m_pD3DDevice->OMSetRenderTargets(1, &m_pDstInOutTexRTView, NULL );
    // Set a resolve PixelShader (instead of using stencil test)
    //  to resolve the stencil buffer into the final texture
    V_RETURN(m_pDSTex2DSRVar->SetResource(m_pDSTex2DSRView));
    V_RETURN(m_pResolveWithPSTech->GetPassByIndex(0)->Apply(0));

    // Set viewport and scissor to match the size of a single slice 
    D3D10_VIEWPORT viewport = { 0, 0, m_width, m_height, 0.0f, 1.0f };
    m_pD3DDevice->RSSetViewports(1, &viewport);
    D3D10_RECT scissorRect = { 0, 0, m_width, m_height };
    m_pD3DDevice->RSSetScissorRects(1, &scissorRect);

    DrawSlices();

    V_RETURN(m_pDSTex2DSRVar->SetResource(NULL));
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL );

    return hr;
}

HRESULT Voxelizer::VoxelizeVelocity(void)
{
    HRESULT hr(S_OK);

    assert(m_initialized);
    assert(m_pDstVelTexture3D);
    assert(m_pDstVelTexRTView);

    // set viewport and scissor to match the size of a single slice
    D3D10_VIEWPORT viewport = { 0, 0, m_width, m_height, 0.0f, 1.0f };
    m_pD3DDevice->RSSetViewports(1, &viewport);
    D3D10_RECT scissorRect = { 0, 0, m_width, m_height };
    m_pD3DDevice->RSSetScissorRects(1, &scissorRect);


    // clear the velocity texture to 0
    FLOAT color[4] = {0.0f, 0.0f, 0.0f, 0.0f};
    m_pD3DDevice->ClearRenderTargetView(m_pDstVelTexRTView, color);

    if(m_pSkinnedMesh == NULL)
        return hr;

    // set the velocity 3D texture as rendertarget
    ID3D10RenderTargetView  *rtViews[2] = { m_pDstVelTexRTView, m_pDstInOutTexRTView};
    m_pD3DDevice->OMSetRenderTargets(2, rtViews, NULL);


    // set some shader globals
    FLOAT projSpacePixDim[2] = { 2.0f/m_width, 2.0f/m_height };
    FLOAT gridDim[3] = { (float)m_width, (float)m_height, (float)m_depth };
    m_pVoxEffect->GetVariableByName("projSpacePixDim")->AsVector()->SetFloatVector(projSpacePixDim);
    m_pVoxEffect->GetVariableByName("gridDim")->AsVector()->SetFloatVector(gridDim);
    m_pVoxEffect->GetVariableByName("recTimeStep")->AsScalar()->SetFloat(1.0f/m_timeStep);
    
    for( UINT z=0; z<m_depth; z++)
    {
        m_pVoxEffect->GetVariableByName("sliceIdx")->AsScalar()->SetInt(z);
        float sliceZ = ((float)z)/m_depth;
        m_pVoxEffect->GetVariableByName("sliceZ")->AsScalar()->SetFloat(sliceZ);
        V_RETURN(RenderClippedMesh(-0.5, 0.5, m_pGenVelocityWireframeTech));
    }

    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);

    return hr;
}

HRESULT Voxelizer::RenderClippedMesh(float zNear, float zFar, ID3D10EffectTechnique *pTechnique)
{
    HRESULT hr(S_OK);
    D3DXMATRIX proj;
    D3DXMATRIX worldViewProj;

    D3DXMatrixOrthoOffCenterLH(&proj, -0.5, 0.5, -0.5, 0.5, zNear, zFar);
    D3DXMatrixMultiply(&worldViewProj, &m_objToVolumeXForm, &proj);
    V_RETURN(m_pWorldViewProjectionVar->SetMatrix(reinterpret_cast<float*>(&worldViewProj)));
    
    if(pTechnique == m_pGenVelocityWireframeTech)
    {
        ID3D10Buffer *buffers[] = { m_pSkinnedMesh->GetSOBuffer(m_srcSoBuffer), m_pSkinnedMesh->GetSOBuffer(m_prevSrcSoBuffer) };
        UINT strides[] = {sizeof(SkinnedMeshD3D10::SOVertex), sizeof(SkinnedMeshD3D10::SOVertex) };
        UINT offsets[] = {0, 0};
        m_pD3DDevice->IASetVertexBuffers(0, 2, buffers, strides, offsets);
        m_pSkinnedMesh->RenderFromSO(m_pSOInputLayout, pTechnique);
    }
    else
    {
        m_pSkinnedMesh->RenderFromSO(m_srcSoBuffer, pTechnique);
    }

    return hr;
}
