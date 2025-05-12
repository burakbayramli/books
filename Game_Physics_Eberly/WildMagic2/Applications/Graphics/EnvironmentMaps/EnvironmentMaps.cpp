// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "EnvironmentMaps.h"

EnvironmentMaps g_kTheApp;

//----------------------------------------------------------------------------
EnvironmentMaps::EnvironmentMaps ()
    :
    Application("EnvironmentMaps",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool EnvironmentMaps::Setup ()
{
    // load model
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    Stream kStream;
    bool bLoaded = kStream.Load("Face.mgc");
    if ( !bLoaded )
        return false;
    m_spkModel = (Node*) kStream.GetObjectAt(0);

    m_spkTrnNode->AttachChild(m_spkModel);

    // attach environment map
    Image* pkImage = Image::Load("SphereMap.mif");
    if ( !pkImage )
        return false;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(pkImage);
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_NONE;
    pkTexture->Apply() = Texture::AM_DECAL;
    pkTexture->Envmap() = Texture::EM_SPHERE;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkModel->SetRenderState(pkTS);

    return true;
}
//----------------------------------------------------------------------------
bool EnvironmentMaps::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    if ( !Setup() )
        return true;

    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(0.0f,0.0f,-1.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(1.0f,0.0f,0.0f);
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 1.0f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void EnvironmentMaps::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void EnvironmentMaps::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
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
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of Face.mgc or SphereMap.mif failed.  ");
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
void EnvironmentMaps::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
