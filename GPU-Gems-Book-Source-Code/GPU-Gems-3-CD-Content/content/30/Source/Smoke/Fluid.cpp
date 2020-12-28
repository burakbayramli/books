//----------------------------------------------------------------------------------
// File:   Fluid.cpp
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

//#include "DXUT.h"
#include "Smoke.h"
#include "SDKmisc.h"
#include "Fluid.h"
#include <math.h>
#include <iostream>
#include <stdlib.h>
using namespace std;

#define PI 3.14159265

//---------------------------------------------------------------------------
//Fluid
//---------------------------------------------------------------------------

Fluid::Fluid( ID3D10Device* pd3dDevice )
{
    m_pD3DDevice = NULL;

    SAFE_ACQUIRE(m_pD3DDevice, pd3dDevice);

    RenderTargetFormats [RENDER_TARGET_VELOCITY0]   = DXGI_FORMAT_R16G16B16A16_FLOAT;
    RenderTargetFormats [RENDER_TARGET_VELOCITY1]   = DXGI_FORMAT_R16G16B16A16_FLOAT;
    RenderTargetFormats [RENDER_TARGET_PRESSURE]    = DXGI_FORMAT_R16_FLOAT;
    RenderTargetFormats [RENDER_TARGET_COLOR0]      = DXGI_FORMAT_R16_FLOAT;
    RenderTargetFormats [RENDER_TARGET_COLOR1]      = DXGI_FORMAT_R16_FLOAT;
    RenderTargetFormats [RENDER_TARGET_OBSTACLES]   = DXGI_FORMAT_R8_UNORM;
    RenderTargetFormats [RENDER_TARGET_OBSTVELOCITY]= DXGI_FORMAT_R16G16B16A16_FLOAT;
    // RENDER_TARGET_TEMPSCALAR: for AdvectBFECC and for Jacobi (for pressure projection)
    RenderTargetFormats [RENDER_TARGET_TEMPSCALAR]  = DXGI_FORMAT_R16_FLOAT;
    // RENDER_TARGET_TEMPVECTOR: for Advect2, Divergence, and Vorticity
    RenderTargetFormats [RENDER_TARGET_TEMPVECTOR]  = DXGI_FORMAT_R16G16B16A16_FLOAT;  

    memset(pRenderTargets3D, 0, sizeof(pRenderTargets3D));
    memset(pShaderResourceVariables, 0, sizeof(pShaderResourceVariables));
    memset(pRenderTargetShaderViews, 0, sizeof(pRenderTargetShaderViews));
    memset(pRenderTargetViews, 0, sizeof(pRenderTargetViews));
}


HRESULT Fluid::Initialize( int width, int height, int depth )
{
    HRESULT hr;

    V_RETURN(LoadShaders());

    m_nIterations = 10;

    D3D10_TEXTURE3D_DESC desc;
    desc.BindFlags = D3D10_BIND_SHADER_RESOURCE | D3D10_BIND_RENDER_TARGET;
    desc.CPUAccessFlags = 0; 
    desc.MipLevels = 1;
    desc.MiscFlags = 0;
    desc.Usage = D3D10_USAGE_DEFAULT;
    desc.Width =  width;
    desc.Height = height;
    desc.Depth =  depth;

    for(int rtIndex=0; rtIndex<NUM_RENDER_TARGETS; rtIndex++)
    {
       desc.Format = RenderTargetFormats[rtIndex];
       V_RETURN(CreateRenderTargetTextureAndView( rtIndex, desc ));
    }

    V_RETURN(TextureWidthShaderVariable->SetFloat( float(width)));
    V_RETURN(TextureHeightShaderVariable->SetFloat(float(height)));
    V_RETURN(TextureDepthShaderVariable->SetFloat(float(depth)));

    D3D10_SHADER_RESOURCE_VIEW_DESC SRVDesc;
    ZeroMemory( &SRVDesc, sizeof(SRVDesc) );
    SRVDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE3D;
    SRVDesc.Texture3D.MipLevels = 1;
    SRVDesc.Texture3D.MostDetailedMip = 0;

    D3D10_SHADER_RESOURCE_VIEW_DESC SRV2DDesc;
    ZeroMemory( &SRV2DDesc, sizeof(SRV2DDesc) );
    SRV2DDesc.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
    SRV2DDesc.Texture2D.MipLevels = 1;
    SRV2DDesc.Texture2D.MostDetailedMip = 0;

    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_VELOCITY0];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_VELOCITY0,"Texture_velocity0",pEffect,&SRVDesc));
    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_VELOCITY1];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_VELOCITY1,"Texture_velocity1",pEffect,&SRVDesc));

    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_PRESSURE];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_PRESSURE,"Texture_pressure",pEffect,&SRVDesc));

    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_COLOR0];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_COLOR0,"Texture_color",pEffect,&SRVDesc));
    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_COLOR1];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_COLOR1,"Texture_color",pEffect,&SRVDesc));

    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_OBSTACLES];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_OBSTACLES,"Texture_obstacles",pEffect,&SRVDesc));
    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_OBSTVELOCITY];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_OBSTVELOCITY,"Texture_obstvelocity",pEffect,&SRVDesc));

    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_TEMPSCALAR];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_TEMPSCALAR,"Texture_tempscalar",pEffect,&SRVDesc));
    SRVDesc.Format = RenderTargetFormats[RENDER_TARGET_TEMPVECTOR];
    V_RETURN(CreateRTTextureAsShaderResource(RENDER_TARGET_TEMPVECTOR,"Texture_tempvector",pEffect,&SRVDesc));

    Reset();

    useBFECC = true;
    saturation = 0.78f;
    mouseDown = false;
    addDensity = true;

    impulseSize = 0.15f;

    confinementScale = 0.0f;
    decay = 1.0f;

    try
    {
        grid = new Grid( m_pD3DDevice );
        renderer = new VolumeRenderer( m_pD3DDevice );
    }
    catch(...)
    {
        SAFE_DELETE(grid);
        SAFE_DELETE(renderer);
        return E_OUTOFMEMORY;
    }

    V_RETURN(grid->Initialize( width, height, depth, TechniqueAdvect ));
    V_RETURN(renderer->Initialize( grid->dim[0], grid->dim[1], grid->dim[2] ));

    impulseX = (float)(grid->dim[0] / 2);
    impulseY = 0;
    impulseZ = (float)(grid->dim[2] / 2);
    impulseDx = 0;
    impulseDy = 1;
    impulseDz = 0;

    mustUpdateObstacles = false;

    obstPos = D3DXVECTOR4(0, 0, 0, 0);
    obstPrevPos = D3DXVECTOR4(0, 0, 0, 0);
    obstVelocity = D3DXVECTOR4(0, 0, 0, 0);

    return S_OK;
}

Fluid::~Fluid( void )
{
    SAFE_DELETE(grid);
    SAFE_DELETE(renderer);
    SAFE_RELEASE(m_pD3DDevice);
    SAFE_RELEASE(pEffect);

    for(int i=0;i<NUM_RENDER_TARGETS;i++)
    {
        SAFE_RELEASE(pRenderTargets3D[i]);
        SAFE_RELEASE(pRenderTargetShaderViews[i]);
        SAFE_RELEASE(pRenderTargetViews[i]);
    }
}

void Fluid::SetObstaclePositionInNormalizedGrid( float x, float y, float z )
{
    obstPos = D3DXVECTOR4(x, y, z, 1);
    D3DXVECTOR3 pos = D3DXVECTOR3(x, y, z);

    D3DXVECTOR3 boxHDims(0.2f * 0.5f, 0.3f * 0.5f, 0.3f * 0.5f);
    D3DXVECTOR3 boxLBDcorner = (pos - boxHDims);
    boxLBDcorner.x *= grid->dim[0]; boxLBDcorner.y *= grid->dim[1]; boxLBDcorner.z *= grid->dim[2];
    D3DXVECTOR3 boxRTUcorner = (pos + boxHDims);
    boxRTUcorner.x *= grid->dim[0]; boxRTUcorner.y *= grid->dim[1]; boxRTUcorner.z *= grid->dim[2];

    pEffect->GetVariableByName("boxLBDcorner")->AsVector()->SetFloatVector(boxLBDcorner);
    pEffect->GetVariableByName("boxRTUcorner")->AsVector()->SetFloatVector(boxRTUcorner);

    mustUpdateObstacles = true;
}

void Fluid::Update( float timestep )
{
    HRESULT hr;

    // All drawing will take place to a viewport with the dimensions of a 3D texture slice
    D3D10_VIEWPORT rtViewport;
    rtViewport.TopLeftX = 0;
    rtViewport.TopLeftY = 0;
    rtViewport.MinDepth = 0;
    rtViewport.MaxDepth = 1;
    rtViewport.Width =  GetTextureWidth();  
    rtViewport.Height = GetTextureHeight(); 
    m_pD3DDevice->RSSetViewports(1,&rtViewport);

    // Update the obstacle velocity based on its movement
    {
        obstVelocity = (obstPos - obstPrevPos) / timestep;
        // Exagerate the velocity a bit to give more momentum to the smoke
        obstVelocity *= 1.5f;
        // Scale obstVelocity to voxel space
        obstVelocity.x *= grid->dim[0]; obstVelocity.y *= grid->dim[1]; obstVelocity.z *= grid->dim[2];
        pEffect->GetVariableByName("obstVelocity")->AsVector()->SetFloatVector(obstVelocity);
        obstPrevPos = obstPos;
    }

    // Hard-coded procedural obstacle box for simple testing
    if( mustUpdateObstacles ) {
        UpdateObstacles();
        mustUpdateObstacles = true;
    }

    // Set vorticity confinment and decay parameters
    if( useBFECC )
    {
       confinementScale = 0.06f;
       decay = 0.994f;
    }
    else
    {
       confinementScale = 0.12f;
       decay = 0.9995f;
    }


    // Bind the obstacle textures before starting with the simulation updates
    pShaderResourceVariables[RENDER_TARGET_OBSTACLES]->SetResource(
        pRenderTargetShaderViews[RENDER_TARGET_OBSTACLES]);
    pShaderResourceVariables[RENDER_TARGET_OBSTVELOCITY]->SetResource(
        pRenderTargetShaderViews[RENDER_TARGET_OBSTVELOCITY]);
    V(TechniqueProject->GetPassByIndex(0)->Apply(0));


    if( useBFECC )
        AdvectColorBFECC( timestep );
    else
        AdvectColor( timestep );

    AdvectVelocity( timestep );

    ApplyVorticityConfinement( timestep );

    ApplyExternalForces( timestep );

    ComputeVelocityDivergence( timestep );

    ComputePressure( timestep );

    ProjectVelocity( timestep );

    ColorTextureNumber = 1 - ColorTextureNumber;


    // Unset resources and apply technique (so that the resource is actually unbound)
    pShaderResourceVariables[RENDER_TARGET_OBSTACLES]->SetResource(NULL);
    pShaderResourceVariables[RENDER_TARGET_OBSTVELOCITY]->SetResource(NULL);
    V(TechniqueProject->GetPassByIndex(0)->Apply(0));
}

HRESULT Fluid::Draw( int field )
{
    HRESULT hr = S_OK;
    D3D10_VIEWPORT rtViewport;
    rtViewport.TopLeftX = 0;
    rtViewport.TopLeftY = 0;
    rtViewport.MinDepth = 0;
    rtViewport.MaxDepth = 1;
    rtViewport.Width = g_Width;
    rtViewport.Height = g_Height;
    m_pD3DDevice->RSSetViewports(1,&rtViewport);
    ID3D10RenderTargetView* pRTV = DXUTGetD3D10RenderTargetView();
    m_pD3DDevice->OMSetRenderTargets( 1, &pRTV , NULL ); 
    DrawTextureShaderVariable->SetInt( field );

    // Set resources and apply technique
    pShaderResourceVariables[RENDER_TARGET_OBSTACLES]->SetResource(
        pRenderTargetShaderViews[RENDER_TARGET_OBSTACLES]);
    pShaderResourceVariables[RENDER_TARGET_OBSTVELOCITY]->SetResource(
        pRenderTargetShaderViews[RENDER_TARGET_OBSTVELOCITY]);
    V_RETURN(TechniqueDrawTexture->GetPassByIndex(0)->Apply(0));

    grid->DrawSlicesToScreen();

    // Unset resources and apply technique (so that the resource is actually unbound)
    pShaderResourceVariables[RENDER_TARGET_OBSTACLES]->SetResource(NULL);
    pShaderResourceVariables[RENDER_TARGET_OBSTVELOCITY]->SetResource(NULL);
    V_RETURN(TechniqueDrawTexture->GetPassByIndex(0)->Apply(0));
 
    return hr; 
}

void Fluid::Render3D(void)
{
    ID3D10ShaderResourceView *sourceTextureSRV = pRenderTargetShaderViews[RENDER_TARGET_COLOR0];
    if(ColorTextureNumber != 0 )
    {
        sourceTextureSRV = pRenderTargetShaderViews[RENDER_TARGET_COLOR1];
    }

    renderer->Draw( sourceTextureSRV );
}


void Fluid::Impulse( int x, int y, int z, float dX, float dY, float dZ )
{
    impulseX  = (float)x;
    impulseY  = (float)y;
    impulseZ  = (float)z;
    impulseDx = (float)dX;
    impulseDy = (float)dY;
    impulseDz = (float)dZ;

}

void Fluid::Reset( void )
{
    float color[4] = {0, 0, 0, 0 };
    ColorTextureNumber = 0;
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_VELOCITY0], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_VELOCITY1], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_PRESSURE], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_COLOR0], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_COLOR1], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_OBSTACLES], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_OBSTVELOCITY], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPSCALAR], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPVECTOR], color );
}

HRESULT Fluid::SetScreenSize( int w, int h )
{
    return renderer->SetScreenSize(w, h);
}

int Fluid::GetTextureWidth( void )
{
    return grid->dim[0];
}

int Fluid::GetTextureHeight( void )
{
    return grid->dim[1];
}

void Fluid::UpdateObstacles( void )
{
    HRESULT hr;
    // Draw a box into the obstacles 3D textures.
    // In RENDER_TARGET_OBSTACLES, cells inside the obstacle will be grey 
    //   (0.5 at the boundary and 1.0 if within boundary inside), and cells outside will be black
    // In RENDER_TARGET_OBSTVELOCITY, cell at the boundary will have a defined velocity,
    //  while cells inside or outside will have undefined velocity (likely to be set to 0)

    pShaderResourceVariables[RENDER_TARGET_OBSTACLES]->SetResource(NULL);
    pShaderResourceVariables[RENDER_TARGET_OBSTVELOCITY]->SetResource(NULL);
    V(TechniqueProject->GetPassByIndex(0)->Apply(0));

    ID3D10RenderTargetView *pObstRenderTargets[2] = {
        pRenderTargetViews[RENDER_TARGET_OBSTACLES],
        pRenderTargetViews[RENDER_TARGET_OBSTVELOCITY]
    };
    m_pD3DDevice->OMSetRenderTargets( 2, pObstRenderTargets, NULL ); 
    
    TechniqueDrawWhiteTriangles->GetPassByIndex(0)->Apply(0);
    //grid->DrawBoundaryQuads();
    
    TechniqueDrawWhiteLines->GetPassByIndex(0)->Apply(0);
    //grid->DrawBoundaryLines();

    TechniqueDrawBox->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();

    m_pD3DDevice->OMSetRenderTargets( 0, NULL, NULL );
}

void Fluid::AdvectColor(float timestep)
{
    if(ColorTextureNumber == 0)
    {
        pShaderResourceVariables[RENDER_TARGET_COLOR1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR1] );
        SetRenderTarget( RENDER_TARGET_COLOR0 );
    }
    else
    {
        pShaderResourceVariables[RENDER_TARGET_COLOR0]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR0] );
        SetRenderTarget( RENDER_TARGET_COLOR1 );
    }

    TimeStepShaderVariable->SetFloat(timestep);
    ModulateShaderVariable->SetFloat(1.0f);  
    ForwardShaderVariable->SetFloat(1.0);
    ModulateShaderVariable->SetFloat(decay);
    TechniqueAdvect->GetPassByIndex(0)->Apply(0);

    grid->DrawSlices();
}

void Fluid::AdvectColorBFECC( float timestep )
{
    float color[4] = {0, 0, 0, 0 };

    if(ColorTextureNumber == 0)
        pShaderResourceVariables[RENDER_TARGET_COLOR1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR1] );
    else
        pShaderResourceVariables[RENDER_TARGET_COLOR0]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR0] );

    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPVECTOR], color );
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPSCALAR], color );

    // Advect forward to get \phi^(n+1)
    pShaderResourceVariables[RENDER_TARGET_TEMPVECTOR]->SetResource( NULL );
    TimeStepShaderVariable->SetFloat(timestep);
    ModulateShaderVariable->SetFloat(1.0f);  
    ForwardShaderVariable->SetFloat(1.0f);
    SetRenderTarget( RENDER_TARGET_TEMPVECTOR );
    TechniqueAdvect->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_TEMPVECTOR]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_TEMPVECTOR] );


    // Advect back to get \bar{\phi}
    pShaderResourceVariables[RENDER_TARGET_TEMPSCALAR]->SetResource( NULL );
    pShaderResourceVariables[RENDER_TARGET_COLOR0]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_TEMPVECTOR] );
    TimeStepShaderVariable->SetFloat(timestep);
    ModulateShaderVariable->SetFloat(1.0f);  
    ForwardShaderVariable->SetFloat(-1.0);
    SetRenderTarget( RENDER_TARGET_TEMPSCALAR );
    TechniqueAdvect->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_TEMPSCALAR]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_TEMPSCALAR] );


    // Advect forward but use the BFECC advection shader which
    //  uses both \phi and \bar{\phi} as source quantity
    //  (specifically, (3/2)\phi^n - (1/2)\bar{\phi})
    if(ColorTextureNumber == 0)
    {   
        pShaderResourceVariables[RENDER_TARGET_COLOR1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR1] );
        SetRenderTarget( RENDER_TARGET_COLOR0 );
    }
    else
    {
        pShaderResourceVariables[RENDER_TARGET_COLOR0]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR0] );
        SetRenderTarget( RENDER_TARGET_COLOR1 );
    }
    D3DXVECTOR3 halfVol( grid->dim[0]/2.0f, grid->dim[1]/2.0f, grid->dim[2]/2.0f );
    HalfVolumeDimShaderVariable->SetFloatVector( (float*)&halfVol);
    ModulateShaderVariable->SetFloat(decay);
    ForwardShaderVariable->SetFloat(1.0);
    TechniqueAdvectBFECC->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_TEMPSCALAR]->SetResource( NULL );
    // Apply the technique again so that the RENDER_TARGET_TEMPSCALAR shader resource is unbound
    TechniqueAdvectBFECC->GetPassByIndex(0)->Apply(0);
}

void Fluid::AdvectVelocity( float timestep )
{
    pShaderResourceVariables[RENDER_TARGET_VELOCITY1]->SetResource( NULL );
    SetRenderTarget(RENDER_TARGET_VELOCITY1);
    // Advect velocity by the fluid velocity
    TimeStepShaderVariable->SetFloat(timestep);
    ModulateShaderVariable->SetFloat(1.0 );
    ForwardShaderVariable->SetFloat(1.0);
    TechniqueAdvectVel->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_VELOCITY1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_VELOCITY1] );
    
}


void Fluid::ApplyVorticityConfinement( float timestep )
{
    // Compute vorticity
    float color[4] = {0, 0, 0, 0 };
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPVECTOR], color );

    pShaderResourceVariables[RENDER_TARGET_TEMPVECTOR]->SetResource( NULL );
    SetRenderTarget( RENDER_TARGET_TEMPVECTOR );
    TechniqueVorticity->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices(); 
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_TEMPVECTOR]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_TEMPVECTOR] );

    // Compute and apply vorticity confinement force
    pShaderResourceVariables[RENDER_TARGET_VELOCITY1]->SetResource( NULL );
    EpsilonShaderVariable->SetFloat(confinementScale);
    TimeStepShaderVariable->SetFloat(timestep);
    TechniqueConfinement->GetPassByIndex(0)->Apply(0);
    SetRenderTarget( RENDER_TARGET_VELOCITY1 );
    // Add the confinement force to the rest of the forces
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_VELOCITY1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_VELOCITY1] );

}


static float lilrand()
{
    return (rand()/float(RAND_MAX) - 0.5f)*5.0f;
}

void Fluid::ApplyExternalForces( float timestep )
{
    bool applyForces = mouseDown;
    float impulseX = this->impulseX;
    float impulseY = this->impulseY;
    float impulseZ = this->impulseZ;
    float impulseDx = this->impulseDx;
    float impulseDy = this->impulseDy;
    float impulseDz = this->impulseDz;

//#define MOUSE_DRIVEN_STATIC_DENSITY
#ifdef MOUSE_DRIVEN_STATIC_DENSITY
    if( mouseDown )
    {
        // If there is no user interaction with the mouse
        //  emit smoke with some initial velocity from a fixed location in the volume
        applyForces = true;
        // the emitter location
        impulseX = float(grid->dim[0]) / 2.0f;
        impulseY = float(grid->dim[1]) / 2.0f;
        impulseZ = float(grid->dim[2]) / 2.0f;
        // the emitter direction and initial velocity
        float impulseStrength = 0.0f;
        impulseDx = 0.5f * impulseStrength;
        impulseDy = 0.5f * impulseStrength;
        impulseDz = 0.5f * impulseStrength;
    }
#else
    if( !mouseDown )
    {
        // If there is no user interaction with the mouse
        //  emit smoke with some initial velocity from a fixed location in the volume
        applyForces = true;
        // the emitter location
        impulseX = float(grid->dim[0]) / 10.0f;
        impulseY = float(grid->dim[1]) / 10.0f;
        impulseZ = float(grid->dim[2]) * 0.25f;
        // the emitter direction and initial velocity
        float impulseStrength = 0.8f;
        impulseDx = 0.5f * impulseStrength;
        impulseDy = 0.5f * impulseStrength;
        impulseDz = 0.8f * impulseStrength;
    }
#endif


    // Draw gaussian ball of color
    if( applyForces && ((mouseDown && addDensity) || (!mouseDown)) )
    {
        if( ColorTextureNumber == 0)
        {
            pShaderResourceVariables[RENDER_TARGET_COLOR0]->SetResource( NULL );
            SetRenderTarget( RENDER_TARGET_COLOR0 );
        }
        else 
        {
            pShaderResourceVariables[RENDER_TARGET_COLOR1]->SetResource( NULL );
            SetRenderTarget( RENDER_TARGET_COLOR1 );
        }

        ImpulseSizeShaderVariable->SetFloat(impulseSize);
        
        D3DXVECTOR3 center(impulseX, impulseY, impulseZ);
        ImpulseCenterShaderVariable->SetFloatVector((float*)&center );
        
        // Color is the density of the smoke. We use a sinusoidal function of 't' to make it more interesting.
        static float t = 0.0f;
        t += 0.05f;
        FLOAT density = 1.5f*(((sin( t + 2.0f*float(PI)/3.0f )*0.5f + 0.5f))*saturation + (1.0f-saturation));
        D3DXVECTOR4 color(density, density, density, 1.0f);
        SplatColorShaderVariable->SetFloatVector((float*)&color );
        
        TechniqueGaussian->GetPassByIndex(0)->Apply(0);
        grid->DrawSlices();
        
        m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
        if(ColorTextureNumber == 0)
            pShaderResourceVariables[RENDER_TARGET_COLOR0]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR0] );
        else
            pShaderResourceVariables[RENDER_TARGET_COLOR1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_COLOR1] );

    }
    

    // Draw gaussian ball of velocity
    if( applyForces )
    {
        pShaderResourceVariables[RENDER_TARGET_VELOCITY1]->SetResource( NULL );
        SetRenderTarget( RENDER_TARGET_VELOCITY1 );
        
        ImpulseSizeShaderVariable->SetFloat(impulseSize);
        
        // Color in this case is the initial velocity given to the emitted smoke
        D3DXVECTOR4 color( impulseDx, impulseDy, impulseDz, 1.0f );
        SplatColorShaderVariable->SetFloatVector((float*)&color);
        
        D3DXVECTOR3 center(  impulseX+lilrand(), impulseY+lilrand(), impulseZ+lilrand());
        ImpulseCenterShaderVariable->SetFloatVector((float*)&center);
        
        TechniqueGaussian->GetPassByIndex(0)->Apply(0);
        grid->DrawSlices();
        
        m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
        pShaderResourceVariables[RENDER_TARGET_VELOCITY1]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_VELOCITY1] );

    }
}

void Fluid::ComputeVelocityDivergence( float timestep )
{
    float color[4] = {0, 0, 0, 0 };
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPVECTOR], color );
    pShaderResourceVariables[RENDER_TARGET_TEMPVECTOR]->SetResource( NULL );
    SetRenderTarget( RENDER_TARGET_TEMPVECTOR );
    TechniqueDivergence->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_TEMPVECTOR]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_TEMPVECTOR] );

}

void Fluid::ComputePressure( float timestep )
{
    float color[4] = {0, 0, 0, 0 };
    m_pD3DDevice->ClearRenderTargetView( pRenderTargetViews[RENDER_TARGET_TEMPSCALAR], color );

    // unbind this variable from the other technique that may have used it
    pShaderResourceVariables[RENDER_TARGET_TEMPSCALAR]->SetResource( NULL );
    TechniqueAdvectBFECC->GetPassByIndex(0)->Apply(0);

    for( int iteration = 0; iteration < m_nIterations/2.0; iteration++ )
    {
        pShaderResourceVariables[RENDER_TARGET_PRESSURE]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_PRESSURE] );
        TechniqueJacobi->GetPassByIndex(0)->Apply(0);
        SetRenderTarget( RENDER_TARGET_TEMPSCALAR );
        grid->DrawSlices();
        m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);

        pShaderResourceVariables[RENDER_TARGET_PRESSURE]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_TEMPSCALAR] );
        TechniqueJacobi->GetPassByIndex(0)->Apply(0);
        SetRenderTarget( RENDER_TARGET_PRESSURE );
        grid->DrawSlices();
        m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    }

    pShaderResourceVariables[RENDER_TARGET_PRESSURE]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_PRESSURE] );
    TechniqueJacobi->GetPassByIndex(0)->Apply(0);

}

void Fluid::ProjectVelocity( float timestep )
{
    pShaderResourceVariables[RENDER_TARGET_VELOCITY0]->SetResource( NULL );
    SetRenderTarget( RENDER_TARGET_VELOCITY0 );
    ModulateShaderVariable->SetFloat(1.0f);  
    TechniqueProject->GetPassByIndex(0)->Apply(0);
    grid->DrawSlices();
    m_pD3DDevice->OMSetRenderTargets(0, NULL, NULL);
    pShaderResourceVariables[RENDER_TARGET_VELOCITY0]->SetResource( pRenderTargetShaderViews[RENDER_TARGET_VELOCITY0] );

}


HRESULT Fluid::LoadShaders( )
{
    HRESULT hr;
    DWORD dwShaderFlags = D3D10_SHADER_ENABLE_STRICTNESS;
    WCHAR fullPath[MAX_PATH];
    V_RETURN(NVUTFindDXSDKMediaFileCch( fullPath, MAX_PATH, L"FluidSim.fx" ));
    V_RETURN(CreateEffect( fullPath, &pEffect, dwShaderFlags ));

    // Get handles to all the techiques
    TechniqueAdvect = pEffect->GetTechniqueByName( "Advect" );
    TechniqueAdvectBFECC = pEffect->GetTechniqueByName( "AdvectBFECC" );
    TechniqueAdvectVel = pEffect->GetTechniqueByName( "AdvectVel" );
    TechniqueVorticity = pEffect->GetTechniqueByName( "Vorticity" );
    TechniqueConfinement = pEffect->GetTechniqueByName( "Confinement" );
    TechniqueGaussian = pEffect->GetTechniqueByName( "Gaussian" );
    TechniqueDivergence = pEffect->GetTechniqueByName( "Divergence" );
    TechniqueJacobi = pEffect->GetTechniqueByName( "Jacobi" );
    TechniqueProject = pEffect->GetTechniqueByName( "Project" );
    TechniqueDrawTexture = pEffect->GetTechniqueByName( "DrawTexture" );
    TechniqueDrawWhiteTriangles = pEffect->GetTechniqueByName( "DrawWhiteTriangles" );
    TechniqueDrawWhiteLines = pEffect->GetTechniqueByName( "DrawWhiteLines" );
    TechniqueDrawBox = pEffect->GetTechniqueByName( "DrawBox" );

    TextureWidthShaderVariable = pEffect->GetVariableByName( "textureWidth")->AsScalar();
    TextureHeightShaderVariable = pEffect->GetVariableByName( "textureHeight")->AsScalar();
    TextureDepthShaderVariable = pEffect->GetVariableByName( "textureDepth")->AsScalar();

    // For render call
    DrawTextureShaderVariable = pEffect->GetVariableByName( "textureNumber")->AsScalar();
    // For project, advect
    ModulateShaderVariable = pEffect->GetVariableByName( "modulate")->AsScalar();
    // For gaussian
    ImpulseSizeShaderVariable = pEffect->GetVariableByName( "size")->AsScalar();
    ImpulseCenterShaderVariable = pEffect->GetVariableByName( "center")->AsVector();
    SplatColorShaderVariable = pEffect->GetVariableByName( "splatColor")->AsVector();
    // For confinement
    EpsilonShaderVariable = pEffect->GetVariableByName( "epsilon")->AsScalar();
    // For confinement, advect
    TimeStepShaderVariable = pEffect->GetVariableByName( "timestep")->AsScalar();
    // For advect BFECC
    ForwardShaderVariable = pEffect->GetVariableByName( "forward")->AsScalar();
    HalfVolumeDimShaderVariable = pEffect->GetVariableByName( "halfVolumeDim")->AsVector();

    return S_OK;
}

int Fluid::GetGridCols()
{
    return grid->GetCols();
}

int Fluid ::GetGridRows()
{
    return grid->GetRows();
}

void Fluid::SetRenderTarget(RENDER_TARGET rtIndex, ID3D10DepthStencilView * optionalDSV )
{
    m_pD3DDevice->OMSetRenderTargets( 1, &pRenderTargetViews[rtIndex] , optionalDSV ); 
}


HRESULT Fluid::CreateRenderTargetTextureAndView(int rtIndex, D3D10_TEXTURE3D_DESC TexDesc)
{

    HRESULT hr;

    // Release resources in case they exist
    SAFE_RELEASE( pRenderTargets3D[rtIndex] );
    SAFE_RELEASE( pRenderTargetViews[rtIndex] );

    // Create the texture
    V_RETURN( m_pD3DDevice->CreateTexture3D(&TexDesc,NULL,&pRenderTargets3D[rtIndex]));
    // Create the render target view
    D3D10_RENDER_TARGET_VIEW_DESC DescRT;
    DescRT.Format = TexDesc.Format;
    DescRT.ViewDimension =  D3D10_RTV_DIMENSION_TEXTURE3D;
    DescRT.Texture3D.FirstWSlice = 0;
    DescRT.Texture3D.MipSlice = 0;
    DescRT.Texture3D.WSize = TexDesc.Depth;

    V_RETURN( m_pD3DDevice->CreateRenderTargetView( pRenderTargets3D[rtIndex], &DescRT, &pRenderTargetViews[rtIndex]) );

    return S_OK;
}


HRESULT Fluid::CreateEffect(WCHAR* name, ID3D10Effect **ppEffect, DWORD dwShaderFlags)
{
    HRESULT hr;
    WCHAR str[MAX_PATH];
    V_RETURN( NVUTFindDXSDKMediaFileCch( str, MAX_PATH,name ) );
    V_RETURN( D3DX10CreateEffectFromFile(str, g_pSmokeShadersMacros, NULL, dwShaderFlags, 0, m_pD3DDevice, NULL, NULL, ppEffect, NULL ) );
    return S_OK;
}

HRESULT Fluid::CreateRTTextureAsShaderResource(RENDER_TARGET rtIndex, LPCSTR shaderTextureName,
                                            ID3D10Effect* pEffect,D3D10_SHADER_RESOURCE_VIEW_DESC *SRVDesc )
{
    HRESULT hr;

    // Create the "shader resource view" (SRView) and "shader resource variable" (SRVar) for the given texture 
    SAFE_RELEASE(pRenderTargetShaderViews[rtIndex]);
    V_RETURN(m_pD3DDevice->CreateShaderResourceView( pRenderTargets3D[rtIndex], 
        SRVDesc, &pRenderTargetShaderViews[rtIndex]));
    pShaderResourceVariables[rtIndex] = pEffect->GetVariableByName( shaderTextureName )->AsShaderResource();
    assert(pShaderResourceVariables[rtIndex]->IsValid());

    // Then we bind the texture SRView to the SRVar
    V_RETURN(pShaderResourceVariables[rtIndex]->SetResource( pRenderTargetShaderViews[rtIndex] ));
    
    return S_OK;
}
