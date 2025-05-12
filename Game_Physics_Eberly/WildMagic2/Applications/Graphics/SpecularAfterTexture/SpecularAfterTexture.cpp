// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "SpecularAfterTexture.h"

SpecularAfterTexture g_kTheApp;

//----------------------------------------------------------------------------
SpecularAfterTexture::SpecularAfterTexture ()
    :
    Application("SpecularAfterTexture",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_fTime = 0.0f;
    m_fDeltaTime = 0.01f;
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool SpecularAfterTexture::Setup ()
{
    // load model
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    Stream kStream;
    bool bLoaded = kStream.Load("SkinnedBiped.mgc");
    if ( !bLoaded )
        return false;

    Image* pkSphereMap = Image::Load("SphereMap.mif");
    if ( !pkSphereMap )
        return false;

    m_spkModel = (Node*) kStream.GetObjectAt(0);
    m_spkTrnNode->AttachChild(m_spkModel);

    // set up specular light
    m_spkPointLight = new PointLight;
    m_spkPointLight->Location() = Vector3f(80.0f,0.0f,-23.0f);
    m_spkPointLight->Ambient() = ColorRGB(0.2f,0.2f,0.2f);
    m_spkPointLight->Diffuse() = ColorRGB(0.8f,0.8f,0.8f);
    m_spkPointLight->Specular() = ColorRGB(1.0f,1.0f,1.0f);
    m_spkPointLight->SpecularAfterTexture() = true;
    LightState* pkLS = new LightState;
    pkLS->Attach(m_spkPointLight);
    m_spkModel->SetRenderState(pkLS);

    // attach an environment map
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(pkSphereMap);
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_NONE;
    pkTexture->Apply() = Texture::AM_MODULATE;
    pkTexture->Envmap() = Texture::EM_SPHERE;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkModel->SetRenderState(pkTS);

    return true;
}
//----------------------------------------------------------------------------
bool SpecularAfterTexture::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    if ( !Setup() )
        return true;

    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(80.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
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
void SpecularAfterTexture::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;
    m_spkPointLight = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void SpecularAfterTexture::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(m_fTime);

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
                "Load of SkinnedBiped.mgc or SphereMap.mif failed. ");
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
void SpecularAfterTexture::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'l':
    case 'L':
        m_spkPointLight->SpecularAfterTexture() =
            !m_spkPointLight->SpecularAfterTexture();
        return;
    case '0':
        ResetTime();
        return;
    case 'g':
        m_fTime += m_fDeltaTime;
        break;
    case 'G':
        m_fTime = 0.0f;
        break;
    }

    m_spkScene->UpdateGS(m_fTime);
}
//----------------------------------------------------------------------------
