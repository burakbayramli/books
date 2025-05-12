// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Terrain.h"

Terrain g_kTheApp;

//----------------------------------------------------------------------------
Terrain::Terrain ()
    :
    Application("Terrain",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_fHeightAboveTerrain = 2.0f;
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool Terrain::CreateTerrain ()
{
    // This file contains a 129-by-129 array of unsigned short.  The values
    // were generated from a 256-by-256 gray scale image where the image
    // intensities were treated as heights.
    FILE* pkHeightFile = fopen("height129x129.raw","rb");
    if ( !pkHeightFile ) return false;
    unsigned short usSize = 129, usQuantity = usSize*usSize;
    unsigned short* ausHeight = new unsigned short[usQuantity];
    fread(ausHeight,sizeof(unsigned short),usQuantity,pkHeightFile);
#ifdef WML_BIG_ENDIAN
    StreamSwapBytes(usQuantity,ausHeight);
#endif
    fclose(pkHeightFile);

    // create the terrain
    Vector2f kOrigin = Vector2f::ZERO;
    float fMinElevation = 0.0f;
    float fMaxElevation = 255.0f;
    float fSpacing = 1.0f;
    m_spkPage = new TerrainPage(usSize,ausHeight,kOrigin,fMinElevation,
        fMaxElevation,fSpacing);
    m_spkScene->AttachChild(m_spkPage);

    // Create the terrain texture.  This is a 256-by-256 RGB image that was
    // the original image used to generate the height field, but with colors
    // set to simulate variations in altitude.
    Texture* pkTexture = new Texture;
    Image* pkImage = Image::Load("texture.mif");
    if ( !pkImage ) return false;
    pkTexture->SetImage(pkImage);
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    TextureState* pkTextureState = new TextureState;
    pkTextureState->Set(0,pkTexture);
    m_spkScene->SetRenderState(pkTextureState);

    return true;
}
//----------------------------------------------------------------------------
bool Terrain::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);

    // create root of scene graph
    m_spkScene = new Node(1);
    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);
    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    // create terrain heights and textures
    if ( !CreateTerrain() )
        return true;

    // position the camera in the middle of the page
    float fHeight = m_spkPage->GetHeight(Vector2f(64.0f,64.0f));
    Vector3f kCLoc(64.0f,64.0f,fHeight + m_fHeightAboveTerrain);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);
    ms_spkCamera->Update();

    Simplify();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.05f;
    m_fRotSpeed = 0.05f;

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void Terrain::OnTerminate ()
{
    m_spkPage = NULL;
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Terrain::DrawStatistics (int iX, int iY, const ColorRGB& rkColor)
{
    if ( ms_spkRenderer )
    {
        char acMessage[256];
        sprintf(acMessage,"vertices = %d  triangles = %d",
            m_spkPage->GetVertexQuantity(),m_spkPage->GetTriangleQuantity());
        ms_spkRenderer->Draw(iX,iY,rkColor,acMessage);
    }
}
//----------------------------------------------------------------------------
void Terrain::OnIdle ()
{
    MeasureTime();

    MoveCamera();

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
            DrawStatistics(96,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of height129x129.raw or texture.mif failed.");
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void Terrain::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case '0':  // reset frame rate measurements
        ResetTime();
        return;
    case 'w':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        return;
    case 's':
        Simplify();
        return;
    case '+':
    case '=':
        m_spkPage->SetPixelTolerance(ms_spkRenderer,
            m_spkPage->GetPixelTolerance() + 1.0f);
        Simplify();
        return;
    case '-':
    case '_':
        if ( m_spkPage->GetPixelTolerance() > 1.0f )
        {
            m_spkPage->SetPixelTolerance(ms_spkRenderer,
                m_spkPage->GetPixelTolerance() - 1.0f);
            Simplify();
        }
        return;
    }
}
//----------------------------------------------------------------------------
void Terrain::MoveForward ()
{
    Application::MoveForward();

    Vector3f kLoc = ms_spkCamera->GetLocation();
    float fHeight = m_spkPage->GetHeight(Vector2f(kLoc.X(),kLoc.Y()));
    kLoc.Z() = fHeight + m_fHeightAboveTerrain;
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();

    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::MoveBackward ()
{
    Application::MoveBackward();

    Vector3f kLoc = ms_spkCamera->GetLocation();
    float fHeight = m_spkPage->GetHeight(Vector2f(kLoc.X(),kLoc.Y()));
    kLoc.Z() = fHeight + m_fHeightAboveTerrain;
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();

    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::MoveDown ()
{
    if ( m_fHeightAboveTerrain >= 1.0f )
        m_fHeightAboveTerrain -= 1.0f;

    Vector3f kLoc = ms_spkCamera->GetLocation();
    float fHeight = m_spkPage->GetHeight(Vector2f(kLoc.X(),kLoc.Y()));
    kLoc.Z() = fHeight + m_fHeightAboveTerrain;
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();

    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::MoveUp ()
{
    m_fHeightAboveTerrain += 1.0f;

    Vector3f kLoc = ms_spkCamera->GetLocation();
    float fHeight = m_spkPage->GetHeight(Vector2f(kLoc.X(),kLoc.Y()));
    kLoc.Z() = fHeight + m_fHeightAboveTerrain;
    ms_spkCamera->SetLocation(kLoc);
    ms_spkCamera->Update();

    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::TurnLeft ()
{
    Application::TurnLeft();
    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::TurnRight ()
{
    Application::TurnRight();
    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::LookUp ()
{
    Application::LookUp();
    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::LookDown ()
{
    Application::LookDown();
    Simplify();
}
//----------------------------------------------------------------------------
void Terrain::Simplify ()
{
    // initialize the page
    m_spkPage->ResetBlocks();

    // camera location in terrain model space
    const Vector3f& rkWorldEye = ms_spkCamera->GetLocation();
    Vector3f kDiff = rkWorldEye - m_spkPage->WorldTranslate();
    float fInvScale = 1.0f/m_spkPage->WorldScale();
    Vector3f kModelEye = fInvScale*(kDiff*m_spkPage->WorldRotate());

    // camera direction in terrain model space
    const Vector3f& rkWorldDir = ms_spkCamera->GetDirection();
    Vector3f kModelDir = rkWorldDir*m_spkPage->WorldRotate();

    bool bCloseAssumption = false;
    m_spkPage->Simplify(ms_spkRenderer,kModelEye,kModelDir,bCloseAssumption);
}
//----------------------------------------------------------------------------
