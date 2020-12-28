//----------------------------------------------------------------------------------
// File:   Main.cpp
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

#include "resource.h"

#include "Smoke.h"
#include "DXUTcamera.h"
#include "DXUTgui.h"
#include "DXUTsettingsDlg.h"
#include "SDKmisc.h"
#include "sdkmesh_old.h"

#include "Fluid.h"
#include "Voxelizer.h"
#include "SkinnedMesh.h"

//--------------------------------------------------------------------------------------
// Global Variables
//--------------------------------------------------------------------------------------

#define MAX_SO_BUFFERS 3
int                         g_dstSOBuffer = 0;
int                         g_srcSOBuffer = -1;
int                         g_numWrittenSOBuffers = 0;

// Window Dimensions
int                         g_Width  = 640;
int                         g_Height = 480; 

// Misc options
bool                        g_bPause = false;
bool                        g_bRenderSmoke = true;
bool                        g_bRenderMesh = true;
int                         g_currentRenderChoice = 0;
int                         g_gridWidth = 70;
int                         g_gridHeight = 70;
int                         g_gridDepth = 100;
int                         g_numJacobi = 6;
float                       g_smokeTimestep = 2.0;
bool                        g_reinitializeFluid = false;
bool                        g_voxelizeTestMesh = true;
bool                        g_pauseAnimation = false;

WCHAR* renderLabels[] = 
{ 
    L"Scene", 
    L"Color Simulation",
    L"Velocity Simulation",
    L"Obstacles"
};

// Fluid Simulation state
Fluid*                      g_fluid = NULL;
Voxelizer*                  g_voxelizer = NULL;

// Camera state
CModelViewerCamera          g_Camera;
float                       g_zNear = 0.05f;
float                       g_zFar = 1000.0f;
D3DXVECTOR3                 g_Eye = D3DXVECTOR3( 0.0f, 0.0f, -20.0f );
D3DXVECTOR3                 g_At = D3DXVECTOR3( 0.0f, 0.0f, 0.0f );
D3DXVECTOR3                 g_Up = D3DXVECTOR3( 0.0f, 1.0f, 0.0f );
D3DXMATRIX                  g_View;
D3DXMATRIX                  g_Projection;

// Scene state
//   World transforms
D3DXMATRIX                  g_gridWorld;
D3DXMATRIX                  g_meshWorld;
D3DXVECTOR3                 g_obstaclePos = D3DXVECTOR3(0.5, 0.5, 0.5);
SkinnedMesh*                g_skinnedMesh = NULL;

// Additional depth buffer (to render the scene into) 
//   so that we can sample from it in VolumeRenderer
ID3D10Texture2D*            g_pSceneDepthTex2D      = NULL;
ID3D10Texture2D*            g_pSceneDepthTex2DNonMS = NULL;
ID3D10ShaderResourceView*   g_pSceneDepthSRV        = NULL;
ID3D10RenderTargetView*     g_pSceneDepthRTV        = NULL;


// Any macros to be passed to the shaders used in this sample (for conditional compilation)
D3D10_SHADER_MACRO g_smokeShadersMacros[] = 
{
    NULL
};
D3D10_SHADER_MACRO *g_pSmokeShadersMacros = g_smokeShadersMacros;


// Mouse State for user-interaction with the smoke
bool                        g_mouseControlsSmoke = false;
bool                        leftIsPressed = false;
int                         mouse_x = g_gridWidth/2;
int                         mouse_y = g_gridHeight/2;
int                         mouse_z = g_gridDepth/2;
int                         last_x  = g_gridWidth/2;
int                         last_y  = g_gridHeight/2;
int                         last_z  = g_gridDepth/2;
bool                        last_mouse = false;
bool                        mouse_active = false;
bool                        justClicked = false;

// UI state
ID3DX10Font*                g_pFont = NULL;         // Font for drawing text
ID3DX10Sprite*              g_pSprite = NULL;       // Sprite for batching text drawing
CDXUTTextHelper*            g_pTxtHelper = NULL;
bool                        g_bShowHelp = false;    // show help menu
CDXUTDialogResourceManager  g_DialogResourceManager;// manager for shared resources of dialogs
CD3DSettingsDlg             g_D3DSettingsDlg;       // Device settings dialog
CDXUTDialog                 g_HUD;                  // manages the 3D UI
CDXUTDialog                 g_SampleUI;             // dialog for sample specific controls

//--------------------------------------------------------------------------------------
// UI control IDs
//--------------------------------------------------------------------------------------
#define IDC_TOGGLEFULLSCREEN         1
#define IDC_TOGGLEREF                2
#define IDC_CHANGEDEVICE             3
#define IDC_CHANGERENDER             4
#define IDC_PAUSESMOKE               5
#define IDC_SHOWSMOKE                6
#define IDC_SHOWMODEL                7
#define IDC_USEBFECC                 8
#define IDC_GRIDWIDTH_STATIC         9
#define IDC_GRIDWIDTH_SCALE         10
#define IDC_GRIDHEIGHT_STATIC       11
#define IDC_GRIDHEIGHT_SCALE        12
#define IDC_GRIDDEPTH_STATIC        13
#define IDC_GRIDDEPTH_SCALE         14
#define IDC_GRIDAPPLY               15
#define IDC_NUMJACOBI_STATIC        16
#define IDC_NUMJACOBI_SCALE         17
#define IDC_PAUSEANIMATION          18

//--------------------------------------------------------------------------------------
// Forward declarations 
//--------------------------------------------------------------------------------------
bool    CALLBACK ModifyDeviceSettings( DXUTDeviceSettings* pDeviceSettings, void* pUserContext );
bool    CALLBACK IsD3D10DeviceAcceptable( UINT Adapter, UINT Output, D3D10_DRIVER_TYPE DeviceType, DXGI_FORMAT BufferFormat, bool bWindowed, void* pUserContext );
HRESULT CALLBACK OnD3D10CreateDevice( ID3D10Device* pd3dDevice, const DXGI_SURFACE_DESC* pBufferSurfaceDesc, void* pUserContext );
void    CALLBACK OnD3D10DestroyDevice( void* pUserContext );
HRESULT CALLBACK OnD3D10ResizedSwapChain( ID3D10Device* pd3dDevice, IDXGISwapChain *pSwapChain, const DXGI_SURFACE_DESC* pBufferSurfaceDesc, void* pUserContext );
void    CALLBACK OnD3D10ReleasingSwapChain( void* pUserContext );

void    CALLBACK OnFrameMove( double fTime, float fElapsedTime, void* pUserContext );
void    CALLBACK OnD3D10FrameRender( ID3D10Device* pd3dDevice, double fTime, float fElapsedTime, void* pUserContext );
void    RenderText();

void    InitApp();
HRESULT CreateMesh(ID3D10Device* pd3dDevice);
HRESULT InitializeFluidState(ID3D10Device* pd3dDevice);
HRESULT ReinitWindowSizeDependentRenderTargets(ID3D10Device* pd3dDevice);

LRESULT CALLBACK MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, bool* pbNoFurtherProcessing, void* pUserContext );
void    CALLBACK OnGUIEvent( UINT nEvent, int nControlID, CDXUTControl* pControl, void* pUserContext );
void    CALLBACK OnKeyboard( UINT nChar, bool bKeyDown, bool bAltDown, void* pUserContext );
void    CALLBACK OnMouse( bool bLeftButtonDown, bool bRightButtonDown, bool bMiddleButtonDown,
                       bool bSideButton1Down, bool bSideButton2Down, int nMouseWheelDelta,
                       int xPos, int yPos, void* pUserContext );


//--------------------------------------------------------------------------------------
// Entry point to the program. Initializes everything and goes into a message processing 
// loop. Idle time is used to render the scene.
//--------------------------------------------------------------------------------------
int WINAPI wWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPWSTR lpCmdLine, int nCmdShow )
{
    // Enable run-time memory check for debug builds.
#if defined(DEBUG) | defined(_DEBUG)
    _CrtSetDbgFlag( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#endif

    // DXUT will create and use the best device (either D3D9 or D3D10) 
    // that is available system depending on which D3D callbacks set below

    // Set DXUT callbacks
    DXUTSetCallbackD3D10DeviceAcceptable( IsD3D10DeviceAcceptable );
    DXUTSetCallbackD3D10DeviceCreated( OnD3D10CreateDevice );
    DXUTSetCallbackD3D10SwapChainResized( OnD3D10ResizedSwapChain );
    DXUTSetCallbackD3D10SwapChainReleasing( OnD3D10ReleasingSwapChain );
    DXUTSetCallbackD3D10DeviceDestroyed( OnD3D10DestroyDevice );
    DXUTSetCallbackD3D10FrameRender( OnD3D10FrameRender );

    DXUTSetCallbackKeyboard( OnKeyboard );
    DXUTSetCallbackMouse( OnMouse, true );
    DXUTSetCallbackMsgProc( MsgProc );
    DXUTSetCallbackFrameMove( OnFrameMove );
    DXUTSetCallbackDeviceChanging( ModifyDeviceSettings );

    DXUTInit( true, true, NULL ); // Parse the command line, show msgboxes on error, no extra command line params
    DXUTSetCursorSettings( true, true ); // Show the cursor and clip it when in full screen
    
    InitApp();

    DXUTCreateWindow( L"Smoke" );
    DXUTCreateDevice( true, g_Width, g_Height );
    DXUTMainLoop(); // Enter into the DXUT render loop

    return DXUTGetExitCode();
}


//--------------------------------------------------------------------------------------
// Initialize the app 
//--------------------------------------------------------------------------------------
void InitApp()
{
    WCHAR sz[100];

    g_D3DSettingsDlg.Init( &g_DialogResourceManager );
    g_HUD.Init( &g_DialogResourceManager );
    g_SampleUI.Init( &g_DialogResourceManager );

    g_HUD.SetCallback( OnGUIEvent ); int iY = 10; 
    g_HUD.AddButton( IDC_TOGGLEFULLSCREEN, L"Toggle full screen", 15, iY, 125, 22 );
    g_HUD.AddButton( IDC_TOGGLEREF, L"Toggle REF (F3)", 15, iY += 24, 125, 22, VK_F3 );
    g_HUD.AddButton( IDC_CHANGEDEVICE, L"Change device (F2)", 15, iY += 24, 125, 22, VK_F2 );

    g_SampleUI.SetCallback( OnGUIEvent ); iY = 10; 

    g_SampleUI.AddComboBox( IDC_CHANGERENDER, 15, iY += 24, 150, 22 );
    for(int i = 0; i < 4; i++)
        g_SampleUI.GetComboBox( IDC_CHANGERENDER )->AddItem( renderLabels[i], NULL );
    g_SampleUI.GetComboBox( IDC_CHANGERENDER )->SetSelectedByIndex(0);

    g_SampleUI.AddCheckBox( IDC_PAUSESMOKE, L"Pause Smoke", 15, iY += 24, 125, 22, g_bPause );
    g_SampleUI.AddCheckBox( IDC_SHOWSMOKE, L"Render Smoke", 15, iY += 24, 125, 22, g_bRenderSmoke );
    g_SampleUI.AddCheckBox( IDC_SHOWMODEL, L"Render Model", 15, iY += 24, 125, 22, g_bRenderMesh );
    g_SampleUI.AddCheckBox( IDC_USEBFECC, L"Use BFECC", 15, iY += 24, 125, 22, true );
    g_SampleUI.AddCheckBox( IDC_PAUSEANIMATION, L"Pause Animation", 15, iY += 24, 125, 22, g_pauseAnimation );


    iY += 50;
    
    g_SampleUI.AddButton( IDC_GRIDAPPLY, L"Change Grid Dimensions", 15, iY, 125, 22 );

    StringCchPrintf( sz, 100, L"Grid Width: %0.2f", float(g_gridWidth) ); 
    g_SampleUI.AddStatic( IDC_GRIDWIDTH_STATIC, sz, 15, iY += 24, 125, 22 );
    g_SampleUI.AddSlider( IDC_GRIDWIDTH_SCALE, 15, iY += 20, 100, 22, 32, 128, g_gridWidth );

    StringCchPrintf( sz, 100, L"Grid Height: %0.2f", float(g_gridHeight) ); 
    g_SampleUI.AddStatic( IDC_GRIDHEIGHT_STATIC, sz, 15, iY += 24, 125, 22 );
    g_SampleUI.AddSlider( IDC_GRIDHEIGHT_SCALE, 15, iY += 20, 100, 22, 32, 128, g_gridHeight );

    StringCchPrintf( sz, 100, L"Grid Depth: %0.2f", float(g_gridDepth) ); 
    g_SampleUI.AddStatic( IDC_GRIDDEPTH_STATIC, sz, 15, iY += 24, 125, 22 );
    g_SampleUI.AddSlider( IDC_GRIDDEPTH_SCALE, 15, iY += 20, 100, 22, 32, 128, g_gridDepth );

    iY += 25;
    StringCchPrintf( sz, 100, L"Jacobi Iterations: %0.2f", float(g_numJacobi) ); 
    g_SampleUI.AddStatic( IDC_NUMJACOBI_STATIC, sz, 15, iY += 24, 125, 22 );
    g_SampleUI.AddSlider( IDC_NUMJACOBI_SCALE, 15, iY += 20, 100, 22, 1, 60, g_numJacobi );

}


//--------------------------------------------------------------------------------------
// Reject any D3D10 devices that aren't acceptable by returning false
//--------------------------------------------------------------------------------------
bool CALLBACK IsD3D10DeviceAcceptable( UINT Adapter, UINT Output, D3D10_DRIVER_TYPE DeviceType, DXGI_FORMAT BufferFormat, bool bWindowed, void* pUserContext )
{
    return true;
}


//--------------------------------------------------------------------------------------
// Create any D3D10 resources that aren't dependant on the back buffer
//--------------------------------------------------------------------------------------
HRESULT CALLBACK OnD3D10CreateDevice( ID3D10Device* pd3dDevice, const DXGI_SURFACE_DESC *pBufferSurfaceDesc, void* pUserContext )
{
    HRESULT hr;

    V_RETURN(DXUTSetMediaSearchPath(L"..\\Source\\Smoke"));

    V_RETURN(g_DialogResourceManager.OnD3D10CreateDevice( pd3dDevice ));
    V_RETURN(g_D3DSettingsDlg.OnD3D10CreateDevice( pd3dDevice ));

    V_RETURN(D3DX10CreateFont( pd3dDevice, 15, 0, FW_BOLD, 1, FALSE, DEFAULT_CHARSET, 
                                OUT_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH | FF_DONTCARE, 
                                L"Arial", &g_pFont ));
    V_RETURN(D3DX10CreateSprite( pd3dDevice, 512, &g_pSprite ));
    g_pTxtHelper = new CDXUTTextHelper( NULL, NULL, g_pFont, g_pSprite, 15 );
    if(!g_pTxtHelper)
        return E_OUTOFMEMORY;


    V_RETURN(CreateMesh(pd3dDevice));

    V_RETURN(InitializeFluidState(pd3dDevice));

    // Initialize the view matrix
    g_Camera.SetViewParams( &g_Eye, &g_At );
    g_Camera.SetEnablePositionMovement(true);
    g_Camera.SetScalers(0.004f, 20.0f);
    g_View = *g_Camera.GetViewMatrix();
    g_Projection = *g_Camera.GetProjMatrix();

    return S_OK;
}

//--------------------------------------------------------------------------------------
// Load a mesh to put inside the smoke
//--------------------------------------------------------------------------------------
HRESULT CreateMesh(ID3D10Device* pd3dDevice)
{
    HRESULT hr;

    // try loading skinned mesh
    WCHAR fullMeshPath[MAX_PATH];
    V_RETURN(NVUTFindDXSDKMediaFileCch( fullMeshPath, MAX_PATH, L"..\\..\\Media\\Gargoyle\\gargoyle.dae" ));
    g_skinnedMesh = SkinnedMesh::CreateD3D10FromColladaFile(fullMeshPath, pd3dDevice, MAX_SO_BUFFERS);
    if(!g_skinnedMesh)
        return E_FAIL;

    return S_OK;
}


//--------------------------------------------------------------------------------------
// Initialize fluid and voxelizer states
//--------------------------------------------------------------------------------------
HRESULT InitializeFluidState(ID3D10Device* pd3dDevice)
{
    HRESULT hr(S_OK);

    SAFE_DELETE(g_fluid);
    SAFE_DELETE(g_voxelizer);

    // Initialize fluid state
    g_fluid = new Fluid(pd3dDevice);
    if( !g_fluid ) 
        return E_OUTOFMEMORY;

    V_RETURN(g_fluid->Initialize(g_gridWidth, g_gridHeight, g_gridDepth));
    
    V_RETURN(g_fluid->SetScreenSize(g_Width, g_Height));

    // Initialize voxelizer state
    g_voxelizer = new Voxelizer();
    if( !g_voxelizer )
        return E_OUTOFMEMORY;

    V_RETURN(g_voxelizer->SetDestination(pd3dDevice, 
        g_fluid->GetRenderTarget3D(Fluid::RENDER_TARGET_OBSTACLES),
        g_fluid->GetRenderTarget3D(Fluid::RENDER_TARGET_OBSTVELOCITY)));

    g_voxelizeTestMesh = true;

    g_reinitializeFluid = false;

    return hr;
}


//--------------------------------------------------------------------------------------
// Create any D3D10 resources that depend on the back buffer
//--------------------------------------------------------------------------------------
HRESULT CALLBACK OnD3D10ResizedSwapChain( ID3D10Device* pd3dDevice, IDXGISwapChain *pSwapChain, const DXGI_SURFACE_DESC* pBufferSurfaceDesc, void* pUserContext )
{
    HRESULT hr;
    
    pd3dDevice->OMSetRenderTargets( 0, NULL, NULL );

    V_RETURN( g_DialogResourceManager.OnD3D10ResizedSwapChain( pd3dDevice, pBufferSurfaceDesc ) );
    V_RETURN( g_D3DSettingsDlg.OnD3D10ResizedSwapChain( pd3dDevice, pBufferSurfaceDesc ) );

    g_Width = pBufferSurfaceDesc->Width;
    g_Height = pBufferSurfaceDesc->Height;

    // Setup the projection parameters again
    float fAspect = static_cast<float>( pBufferSurfaceDesc->Width )/static_cast<float>( pBufferSurfaceDesc->Height );
    g_Camera.SetProjParams( D3DX_PI * 0.25f , fAspect, g_zNear, g_zFar);
    g_Camera.SetWindow( pBufferSurfaceDesc->Width, pBufferSurfaceDesc->Height );
    g_Camera.SetButtonMasks(NULL, MOUSE_WHEEL, MOUSE_RIGHT_BUTTON );

    g_HUD.SetLocation( pBufferSurfaceDesc->Width-170, 0 );
    g_HUD.SetSize( 170, 170 );
    g_SampleUI.SetLocation( pBufferSurfaceDesc->Width-170, 80 );
    g_SampleUI.SetSize( 170, 300 );

    V_RETURN( ReinitWindowSizeDependentRenderTargets(pd3dDevice));
    if( g_fluid ) 
       V_RETURN( g_fluid->SetScreenSize(g_Width, g_Height) );

    return S_OK;
}

//--------------------------------------------------------------------------------------
// Initialize any textures that must match the window size
//--------------------------------------------------------------------------------------
HRESULT ReinitWindowSizeDependentRenderTargets(ID3D10Device* pd3dDevice)
{
    HRESULT hr;

    // Create resources to enable writing the scene depth using MRT, as well as to 
    //  enable reading as a shader resource
    ID3D10RenderTargetView *pRTV = DXUTGetD3D10RenderTargetView();
    ID3D10Resource *pRTVResource;
    pRTV->GetResource(&pRTVResource);
    ID3D10Texture2D *pRTVTex2D = static_cast<ID3D10Texture2D*>(pRTVResource);
    assert(pRTVTex2D);
    D3D10_TEXTURE2D_DESC pRTVTex2DDesc;
    pRTVTex2D->GetDesc(&pRTVTex2DDesc);
    pRTVResource->Release();    

    SAFE_RELEASE(g_pSceneDepthTex2DNonMS);
    SAFE_RELEASE(g_pSceneDepthTex2D);
    SAFE_RELEASE(g_pSceneDepthRTV);
    SAFE_RELEASE(g_pSceneDepthSRV);

    D3D10_TEXTURE2D_DESC desc;
    desc.ArraySize = 1;
    desc.BindFlags = D3D10_BIND_SHADER_RESOURCE | D3D10_BIND_RENDER_TARGET;
    desc.CPUAccessFlags = 0;
    desc.MipLevels = 1;
    desc.MiscFlags = 0;
    desc.SampleDesc = pRTVTex2DDesc.SampleDesc;
    desc.Usage = D3D10_USAGE_DEFAULT;
    desc.Width = g_Width;
    desc.Height = g_Height;
    desc.Format = DXGI_FORMAT_R32_FLOAT;
    V_RETURN(pd3dDevice->CreateTexture2D(&desc,NULL,&g_pSceneDepthTex2D));

    // We need a Non-Multisampled texture2D resource of the same dimensions to read from in shaders
    if(pRTVTex2DDesc.SampleDesc.Count > 1)
    {
        desc.BindFlags = D3D10_BIND_SHADER_RESOURCE | D3D10_BIND_RENDER_TARGET;
        desc.SampleDesc.Count = 1;
        desc.SampleDesc.Quality = 0;
        V_RETURN(pd3dDevice->CreateTexture2D(&desc,NULL,&g_pSceneDepthTex2DNonMS));
    }
    else
    {
        g_pSceneDepthTex2DNonMS = g_pSceneDepthTex2D;
        g_pSceneDepthTex2DNonMS->AddRef();
    }

    // Create the render target view for the potentially Multisampled texture2D resource
    D3D10_RENDER_TARGET_VIEW_DESC descRTV;
    descRTV.Format = DXGI_FORMAT_R32_FLOAT;
    if( pRTVTex2DDesc.SampleDesc.Count <= 1 )
    {
        descRTV.ViewDimension = D3D10_RTV_DIMENSION_TEXTURE2D;
        descRTV.Texture2D.MipSlice = 0;
    }
    else
    {
        descRTV.ViewDimension = D3D10_RTV_DIMENSION_TEXTURE2DMS;
    }
    V_RETURN( pd3dDevice->CreateRenderTargetView( g_pSceneDepthTex2D, &descRTV, &g_pSceneDepthRTV ) );

    // Create a shader resource view for a Non-MS texture
    D3D10_SHADER_RESOURCE_VIEW_DESC descSRV;
    descSRV.Format = DXGI_FORMAT_R32_FLOAT;
    descSRV.ViewDimension = D3D10_SRV_DIMENSION_TEXTURE2D;
    descSRV.Texture2D.MipLevels = 1;
    descSRV.Texture2D.MostDetailedMip = 0;
    V_RETURN( pd3dDevice->CreateShaderResourceView(g_pSceneDepthTex2DNonMS, &descSRV, &g_pSceneDepthSRV) );

    return S_OK;

}

//--------------------------------------------------------------------------------------
// Render the scene using the D3D10 device
//--------------------------------------------------------------------------------------
void CALLBACK OnD3D10FrameRender( ID3D10Device* pd3dDevice, double fTime, float fElapsedTime, void* pUserContext )
{

    // Get the render target and depth buffer, and clear them
    ID3D10RenderTargetView *pRTV = DXUTGetD3D10RenderTargetView();
    ID3D10DepthStencilView *pDSV = DXUTGetD3D10DepthStencilView();
    float color[4] = {0, 0, 0, 0 };
    float color2[4] = { g_zFar,0,0,0};
    pd3dDevice->ClearRenderTargetView( pRTV, color );
    pd3dDevice->ClearDepthStencilView( pDSV, D3D10_CLEAR_DEPTH , 1.0, 0 );
    pd3dDevice->ClearRenderTargetView( g_pSceneDepthRTV, color2 );
    
    // Create a viewport to match the screen size
    D3D10_VIEWPORT rtViewport;
    rtViewport.TopLeftX = 0;
    rtViewport.TopLeftY = 0;
    rtViewport.MinDepth = 0;
    rtViewport.MaxDepth = 1;
    rtViewport.Width = g_Width;
    rtViewport.Height = g_Height;

    // If the settings dialog is being shown, then
    //  render it instead of rendering the app's scene
    if( g_D3DSettingsDlg.IsActive() )
    {
        // Set the viewport
        pd3dDevice->RSSetViewports(1,&rtViewport);
        // Render the scene to the screen
        pd3dDevice->OMSetRenderTargets( 1, &pRTV , pDSV ); 
        g_D3DSettingsDlg.OnRender( fElapsedTime );
        pd3dDevice->OMSetRenderTargets( 0, NULL, NULL );
        return;
    }

    // If the grid size changed we need to reinitialize the fluid
    if(g_reinitializeFluid)
    {
        HRESULT hr;
        V(InitializeFluidState(pd3dDevice));
    }

    if(g_fluid == NULL)
    {
        pd3dDevice->OMSetRenderTargets( 0, NULL, NULL );
        return;
    }
    
    
    // Update the smoke simulation one time step
    if(!g_bPause)
    {
        g_fluid->m_nIterations = g_numJacobi;
        g_fluid->Update( g_smokeTimestep);
    }

    // Initialize the world matrices for the simulation Grid and the obstacle Mesh
    //   Grid
    D3DXMATRIX gridScale, gridRotate;
    D3DXMatrixScaling(&gridScale, 20.0f, 20.0f, 20.0f );
    D3DXMatrixRotationX(&gridRotate, 3.0f*3.1416f/2.0f);
    g_gridWorld = gridScale * gridRotate;
    //   Mesh
    D3DXMATRIX meshScale, meshRotate;
    D3DXMatrixScaling(&meshScale, 0.006f, 0.006f, 0.006f);
    D3DXMatrixRotationY(&meshRotate, 3.146f);
    g_meshWorld = meshScale * meshRotate;
    

    // Streamout the skinned mesh
    //   we do it here because it's needed also for voxelization
    D3DXMATRIX identity;
    D3DXMatrixIdentity(&identity);
    g_skinnedMesh->SetRenderFilterExclude(NULL);
    g_skinnedMesh->SetRenderFilterInclude(NULL);
    g_skinnedMesh->RenderToSO((float*)&identity, g_dstSOBuffer);

    // Voxelize the mesh if needed
    if(g_voxelizeTestMesh && (g_numWrittenSOBuffers > 1))
    {
        // Compute mesh-to-grid xform
        D3DXMATRIX gridWorldInv;
        D3DXMatrixInverse(&gridWorldInv, NULL, &g_gridWorld);
        D3DXMATRIX meshToGridXForm = g_meshWorld * gridWorldInv;

        int prevSrcSOBuffer = g_srcSOBuffer-1;
        if(prevSrcSOBuffer < 0) prevSrcSOBuffer = MAX_SO_BUFFERS-1;

        SkinnedMesh::stdStringVector includeNodes;
        includeNodes.push_back("GargoyleCollision");
        g_skinnedMesh->SetRenderFilterInclude(&includeNodes);

        g_voxelizer->Voxelize(meshToGridXForm, g_skinnedMesh, 
            g_srcSOBuffer, prevSrcSOBuffer, g_smokeTimestep);

        g_skinnedMesh->SetRenderFilterInclude(NULL);
    }

    if(g_currentRenderChoice > 0) 
    {
        // Draw one of the simulation textures
        g_fluid->Draw( g_currentRenderChoice );
    } 
    else
    {
        // Compute and set modelview matrix
        g_Projection = *(g_Camera.GetProjMatrix());
        g_View = *(g_Camera.GetViewMatrix());


        // Render the mesh
        if(g_bRenderMesh && (g_srcSOBuffer>=0))
        {
            ID3D10RenderTargetView *pCnDRTs[2] = { pRTV, g_pSceneDepthRTV };

            assert(g_srcSOBuffer < MAX_SO_BUFFERS);
            pd3dDevice->RSSetViewports(1,&rtViewport);

            SkinnedMesh::stdStringVector excludeNodes;
            excludeNodes.push_back("GargoyleCollision");
            g_skinnedMesh->SetRenderFilterExclude(&excludeNodes);

            // Render the scene color and depth (so that we can sample from it in VolumeRenderer)
            pd3dDevice->OMSetRenderTargets( 2, pCnDRTs, pDSV );
            g_skinnedMesh->Render((float*)&g_meshWorld, (float*)&g_View, (float*)&g_Projection);

            g_skinnedMesh->SetRenderFilterExclude(NULL);
        }
        
        if(g_pSceneDepthTex2DNonMS != g_pSceneDepthTex2D)
        {
            pd3dDevice->ResolveSubresource(g_pSceneDepthTex2DNonMS, 0, 
                g_pSceneDepthTex2D, 0, DXGI_FORMAT_R32_FLOAT);
        }

        // Render the smoke
        if(g_bRenderSmoke)
            g_fluid->Render3D();
    }

    // Advance the streamout buffer counters
    g_dstSOBuffer++;
    g_dstSOBuffer %= MAX_SO_BUFFERS;
    g_numWrittenSOBuffers++;
    if(g_numWrittenSOBuffers > 0)
    {
        g_srcSOBuffer = (g_dstSOBuffer-1);
        if(g_srcSOBuffer < 0) g_srcSOBuffer = MAX_SO_BUFFERS-1;
    }


    //-------------------------------------------------------------------------------------
    // HUD
    //-------------------------------------------------------------------------------------
    pd3dDevice->RSSetViewports(1,&rtViewport);
    pd3dDevice->OMSetRenderTargets( 1, &pRTV , pDSV ); 
    DXUT_BeginPerfEvent( DXUT_PERFEVENTCOLOR, L"HUD / Stats" );
    RenderText();
    g_SampleUI.OnRender( fElapsedTime ); 
    g_HUD.OnRender( fElapsedTime );
    DXUT_EndPerfEvent();

    pd3dDevice->OMSetRenderTargets( 0, NULL, NULL );

}


void RenderText()
{
    g_pTxtHelper->Begin();
    g_pTxtHelper->SetInsertionPos( 2, 0 );
    g_pTxtHelper->SetForegroundColor( D3DXCOLOR( 1.0f, 1.0f, 0.0f, 1.0f ) );
    g_pTxtHelper->DrawTextLine( DXUTGetFrameStats(true) );
    g_pTxtHelper->DrawTextLine( DXUTGetDeviceStats() );

    // Draw help
    if( g_bShowHelp )
    {
        UINT nBackBufferHeight = ( DXUTIsAppRenderingWithD3D9() ) ? DXUTGetD3D9BackBufferSurfaceDesc()->Height : DXUTGetDXGIBackBufferSurfaceDesc()->Height;
        g_pTxtHelper->SetInsertionPos( 2, nBackBufferHeight-30*6 );
        g_pTxtHelper->SetForegroundColor( D3DXCOLOR( 1.0f, 1.0f, 0.0f, 1.0f ) );
        g_pTxtHelper->DrawTextLine( L"Controls:" );

        g_pTxtHelper->SetInsertionPos( 20, nBackBufferHeight-30*5 );
        g_pTxtHelper->DrawTextLine( L"Rotate camera:  Right mouse button\n"
                                    L"Zoom camera:  Mouse wheel scroll\n"
                                    );

        g_pTxtHelper->SetInsertionPos( 20, nBackBufferHeight-30*2 );
        g_pTxtHelper->DrawTextLine( L"Hide help: F1\n" 
                                    L"Quit: ESC\n" );
    }
    else
    {
        g_pTxtHelper->SetForegroundColor( D3DXCOLOR( 1.0f, 1.0f, 1.0f, 1.0f ) );
        g_pTxtHelper->DrawTextLine( L"Press F1 for help" );
    }

    g_pTxtHelper->End();
}



//--------------------------------------------------------------------------------------
// Release D3D10 resources created in OnD3D10ResizedSwapChain 
//--------------------------------------------------------------------------------------
void CALLBACK OnD3D10ReleasingSwapChain( void* pUserContext )
{
    g_DialogResourceManager.OnD3D10ReleasingSwapChain();
}


//--------------------------------------------------------------------------------------
// Release D3D10 resources created in OnD3D10CreateDevice 
//--------------------------------------------------------------------------------------
void CALLBACK OnD3D10DestroyDevice( void* pUserContext )
{
    g_DialogResourceManager.OnD3D10DestroyDevice();
    g_D3DSettingsDlg.OnD3D10DestroyDevice();

    SAFE_RELEASE( g_pFont );
    SAFE_RELEASE( g_pSprite );
    SAFE_DELETE( g_pTxtHelper );


    SAFE_DELETE(g_skinnedMesh);

    SAFE_RELEASE(g_pSceneDepthTex2DNonMS);
    SAFE_RELEASE(g_pSceneDepthTex2D);
    SAFE_RELEASE(g_pSceneDepthRTV);
    SAFE_RELEASE(g_pSceneDepthSRV);


    SAFE_DELETE(g_fluid);
    SAFE_DELETE(g_voxelizer);
}


//--------------------------------------------------------------------------------------
// Called right before creating a D3D9 or D3D10 device, allowing the app to modify the device settings as needed
//--------------------------------------------------------------------------------------
bool CALLBACK ModifyDeviceSettings( DXUTDeviceSettings* pDeviceSettings, void* pUserContext )
{
    //pDeviceSettings->d3d10.SyncInterval = 0; //turn off vsync
    return true;
}


//--------------------------------------------------------------------------------------
// Handle updates to the scene.  This is called regardless of which D3D API is used
//--------------------------------------------------------------------------------------
void CALLBACK OnFrameMove( double fTime, float fElapsedTime, void* pUserContext )
{
    if(!g_mouseControlsSmoke) 
        g_Camera.FrameMove( fElapsedTime );

    if( g_skinnedMesh && !g_pauseAnimation)
        g_skinnedMesh->Update(fElapsedTime);
}


//--------------------------------------------------------------------------------------
// Before handling window messages, DXUT passes incoming windows 
// messages to the application through this callback function. If the application sets 
// *pbNoFurtherProcessing to TRUE, then DXUT will not process this message.
//--------------------------------------------------------------------------------------
LRESULT CALLBACK MsgProc( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam, bool* pbNoFurtherProcessing, void* pUserContext )
{
    // Pass messages to dialog resource manager calls so GUI state is updated correctly
    *pbNoFurtherProcessing = g_DialogResourceManager.MsgProc( hWnd, uMsg, wParam, lParam );
    if( *pbNoFurtherProcessing )
        return 0;

    // Pass messages to settings dialog if its active
    if( g_D3DSettingsDlg.IsActive() )
    {
        g_D3DSettingsDlg.MsgProc( hWnd, uMsg, wParam, lParam );
        return 0;
    }

    // Give the dialogs a chance to handle the message first
    *pbNoFurtherProcessing = g_HUD.MsgProc( hWnd, uMsg, wParam, lParam );
    if( *pbNoFurtherProcessing )
        return 0;
    *pbNoFurtherProcessing = g_SampleUI.MsgProc( hWnd, uMsg, wParam, lParam );
    if( *pbNoFurtherProcessing )
        return 0;
    

    // Pass all remaining windows messages to camera so it can respond to user input
    if(!g_mouseControlsSmoke) 
        g_Camera.HandleMessages( hWnd, uMsg, wParam, lParam );

    return 0;
}


//--------------------------------------------------------------------------------------
// Handle key presses
//--------------------------------------------------------------------------------------
void CALLBACK OnKeyboard( UINT nChar, bool bKeyDown, bool bAltDown, void* pUserContext )
{

    float moveStep = 1.0f / (g_gridWidth * g_smokeTimestep);

    switch(nChar) {
        case 'y':
        case 'Y':
            g_obstaclePos.x += moveStep;
            g_obstaclePos.y += moveStep;
            if( g_obstaclePos.x > 1.0 ) g_obstaclePos.x = 1.0;
            if( g_obstaclePos.y > 1.0 ) g_obstaclePos.y = 1.0;
            g_fluid->SetObstaclePositionInNormalizedGrid( 
                g_obstaclePos.x, g_obstaclePos.y, g_obstaclePos.z);
            break;
        case 'h':
        case 'H':
            g_obstaclePos.x -= moveStep;
            g_obstaclePos.y -= moveStep;
            if( g_obstaclePos.x < 0.0 ) g_obstaclePos.x = 0.0;
            if( g_obstaclePos.y < 0.0 ) g_obstaclePos.y = 0.0;
            g_fluid->SetObstaclePositionInNormalizedGrid( 
                g_obstaclePos.x, g_obstaclePos.y, g_obstaclePos.z);
            break;

        case VK_CONTROL:
            if( bKeyDown )
                g_mouseControlsSmoke = true;
            else
                g_mouseControlsSmoke = false;
            break;

        default:
            if( bKeyDown )
            {
                switch( nChar )
                {
                    case VK_F1: 
                         g_bShowHelp = !g_bShowHelp; 
                         break;
                }
            }
        break;
    }

}


//--------------------------------------------------------------------------------------
// Handle mouse button presses
//--------------------------------------------------------------------------------------
void CALLBACK OnMouse( bool bLeftButtonDown, bool bRightButtonDown, bool bMiddleButtonDown, 
                       bool bSideButton1Down, bool bSideButton2Down, int nMouseWheelDelta, 
                       int x, int y, void* pUserContext )
{

    int z;

    if(!g_mouseControlsSmoke)
        return;

    // Handle left-mouse click-and-drag input to create a temporary user-driven smoke emitter
    if(bLeftButtonDown)
    {
        if( !leftIsPressed )
        {  
            leftIsPressed = true;
            justClicked = true;
        }
        else
        {
            if( !justClicked ) 
            {
               g_fluid->mouseDown = true;
            }

            x = x * g_gridWidth  / g_Width;
            y = (g_Height-y) * g_gridHeight / g_Height;
            z = last_z;
      
            if( last_mouse &&
                x >= 0 && x < g_gridWidth &&
                y >= 0 && y < g_gridHeight )
            {
               float dx, dy, dz, mag;

               dx = float( x - last_x );
               dy = float( y - last_y );
               dz = float( z - last_z );
               mag = sqrt( dx*dx + dy*dy + dz*dz );
               if( mag > 0.01f )
               {
                   dx /= 2.0f;
                   dy /= 2.0f;
                   dz /= 2.0f;
                  g_fluid->Impulse( x, z, y, dx, dz, dy );
               }
            }

            last_x = x;
            last_y = y;
            last_z = z;
            last_mouse = true;
            justClicked = false;
        }
    }
    else
    {
        leftIsPressed = false;
        last_mouse = false;
        g_fluid->mouseDown = false;
    }
}


void CALLBACK OnGUIEvent( UINT nEvent, int nControlID, CDXUTControl* pControl, void* pUserContext )
{    
    switch( nControlID )
    {
        case IDC_TOGGLEFULLSCREEN: DXUTToggleFullScreen(); break;
        case IDC_TOGGLEREF:        DXUTToggleREF(); break;
        case IDC_CHANGEDEVICE:     g_D3DSettingsDlg.SetActive( !g_D3DSettingsDlg.IsActive() ); break;
        case IDC_PAUSESMOKE:       g_bPause = g_SampleUI.GetCheckBox( IDC_PAUSESMOKE )->GetChecked(); break;
        case IDC_SHOWSMOKE :       g_bRenderSmoke = g_SampleUI.GetCheckBox( IDC_SHOWSMOKE )->GetChecked(); break;
        case IDC_SHOWMODEL :       g_bRenderMesh = g_SampleUI.GetCheckBox( IDC_SHOWMODEL )->GetChecked(); break;
        case IDC_USEBFECC :        g_fluid->useBFECC = g_SampleUI.GetCheckBox( IDC_USEBFECC )->GetChecked(); break;
        case IDC_PAUSEANIMATION:   g_pauseAnimation = g_SampleUI.GetCheckBox( IDC_PAUSEANIMATION )->GetChecked(); break;
        
        case IDC_CHANGERENDER:
        {
            // Get the new RT format
            g_currentRenderChoice = g_SampleUI.GetComboBox( IDC_CHANGERENDER )->GetSelectedIndex();
            g_SampleUI.GetComboBox( IDC_CHANGERENDER )->SetSelectedByIndex( g_currentRenderChoice );
            break;
        }
        case IDC_GRIDAPPLY:
        {   
            g_reinitializeFluid = true;
            break;
        }
        case IDC_GRIDWIDTH_SCALE:
        {
            WCHAR sz[100];
            g_gridWidth = g_SampleUI.GetSlider( IDC_GRIDWIDTH_SCALE )->GetValue();
            StringCchPrintf( sz, 100, L"Grid Width: %0.2f", float(g_gridWidth) ); 
            g_SampleUI.GetStatic( IDC_GRIDWIDTH_STATIC )->SetText( sz );
            break;
        }
        case IDC_GRIDHEIGHT_SCALE:
        {
            WCHAR sz[100];
            g_gridHeight = g_SampleUI.GetSlider( IDC_GRIDHEIGHT_SCALE )->GetValue();
            StringCchPrintf( sz, 100, L"Grid Height: %0.2f", float(g_gridHeight) ); 
            g_SampleUI.GetStatic( IDC_GRIDHEIGHT_STATIC )->SetText( sz );
            break;
        }
        case IDC_GRIDDEPTH_SCALE:
        {
            WCHAR sz[100];
            g_gridDepth = g_SampleUI.GetSlider( IDC_GRIDDEPTH_SCALE )->GetValue();
            StringCchPrintf( sz, 100, L"Grid Depth: %0.2f", float(g_gridDepth) ); 
            g_SampleUI.GetStatic( IDC_GRIDDEPTH_STATIC )->SetText( sz );
            break;
        }
        case IDC_NUMJACOBI_SCALE:
        {
            WCHAR sz[100];
            g_numJacobi = g_SampleUI.GetSlider( IDC_NUMJACOBI_SCALE )->GetValue();
            StringCchPrintf( sz, 100, L"Jacobi Iterations: %0.2f", float(g_numJacobi) );
            g_SampleUI.GetStatic( IDC_NUMJACOBI_STATIC )->SetText( sz );
            break;
        }
    }

}
