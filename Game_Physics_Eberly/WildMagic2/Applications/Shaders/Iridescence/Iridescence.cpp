// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Iridescence.h"
#include "WmlStandardMesh.h"

Iridescence g_kTheApp;

//----------------------------------------------------------------------------
Iridescence::Iridescence ()
    :
    Application("Iridescence",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool Iridescence::Setup ()
{
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    // Root node for a scene graph that contains a bump-mapped triangle mesh
    // square.
    m_spkModel = new Node(1);

    TriMesh* pkMesh = NULL;
    CreateTorusMesh(pkMesh,20,20,Vector3f::ZERO,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,2.0f,1.0f,true,true,true,true);
    m_spkTriMesh = pkMesh;

    Image* pkLeaf = Image::Load("leaf2.mif");
    if ( !pkLeaf )
        return false;

    Texture* pkLeafTex = new Texture;
    pkLeafTex->SetImage(pkLeaf);
    pkLeafTex->Filter() = Texture::FM_LINEAR;
    pkLeafTex->Mipmap() = Texture::MM_LINEAR;
    pkLeafTex->Apply() = Texture::AM_DECAL;
    pkLeafTex->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkLeafTex);

    Image* pkGradient = Image::Load("gradient.mif");
    if ( !pkGradient )
        return false;

    Texture* pkGradTex = new Texture;
    pkGradTex->SetImage(pkGradient);
    pkGradTex->Filter() = Texture::FM_LINEAR;
    pkGradTex->Mipmap() = Texture::MM_LINEAR;
    pkGradTex->Apply() = Texture::AM_DECAL;
    pkTS->Set(1,pkGradTex);
    m_spkTriMesh->SetRenderState(pkTS);

    ZBufferState* pkZBuf = new ZBufferState();
    pkZBuf->Enabled() = true;
    pkZBuf->Compare() = ZBufferState::CF_LEQUAL;
    pkZBuf->Writeable() = true;
    m_spkTriMesh->SetRenderState(pkZBuf);

    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    m_spkTriMesh->SetPixelShader(m_spkPixShader);

    m_spkModel->AttachChild(m_spkTriMesh);
    m_spkTrnNode->AttachChild(m_spkModel);

    return true;
}
//----------------------------------------------------------------------------
bool Iridescence::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_bVertexShader = true;
    m_spkVertShader = VertexShader::Load("Iridescence.wvs");
    m_spkPixShader = PixelShader::Load("Iridescence.wps");

    if ( !m_spkVertShader || !m_spkPixShader || !Setup() )
        return true;

    m_spkScene->UpdateGS(0.0f);

    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    Vector3f kCLoc(0,0,-8);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_fInterpolateFactor = 0.5f;

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void Iridescence::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Iridescence::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    m_spkTriMesh->GetVertexConst("InterpolateFactor")->
        SetData( m_fInterpolateFactor );

    Vector3f kAxis(1.0f,0.8f,0.2f);
    kAxis.Normalize();

    m_spkTriMesh->Rotate().FromAxisAngle(kAxis,GetTimeInSeconds());
    
    m_spkScene->UpdateGS(0.0f);

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Load of Iridescence.{wvs,wps}, leaf2.mif, or gradient.mif"\
                " failed.");
            ms_spkRenderer->Draw(8,48,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void Iridescence::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    else if ( ucKey == '+' )
    {
        m_fInterpolateFactor += 0.1f;
        if ( m_fInterpolateFactor > 1.0f )
        {
            m_fInterpolateFactor = 1.0f;
        }
    }
    else if ( ucKey == '-' )
    {
        m_fInterpolateFactor -= 0.1f;
        if ( m_fInterpolateFactor < 0.0f )
        {
            m_fInterpolateFactor = 0.0f;
        }
    }
}
//----------------------------------------------------------------------------
