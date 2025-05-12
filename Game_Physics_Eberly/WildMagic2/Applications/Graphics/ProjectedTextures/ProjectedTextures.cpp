// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "ProjectedTextures.h"

ProjectedTextures g_kTheApp;

//----------------------------------------------------------------------------
ProjectedTextures::ProjectedTextures ()
    :
    Application("ProjectedTextures",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool ProjectedTextures::LoadFace ()
{
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    Stream kStream;
    bool bLoaded = kStream.Load("Face.mgc");
    if ( !bLoaded )
        return false;

    m_spkModel = (Node*) kStream.GetObjectAt(0);
    m_spkTrnNode->AttachChild(m_spkModel);
    m_spkScene->UpdateGS(0.0f);

    return true;
}
//----------------------------------------------------------------------------
bool ProjectedTextures::Setup ()
{
    if ( !LoadFace() )
        return false;

    Image* pkSunfire = Image::Load("sunfire.mif");
    if ( !pkSunfire )
        return false;

    // create a camera to project the texture
    Camera* pkPCamera = new Camera;
    pkPCamera->SetFrustum(2.0f,3.0f,-0.5f,0.5f,0.5f,-0.5f);

    Vector3f kC = m_spkModel->WorldBound().Center();
    float fR = m_spkModel->WorldBound().Radius();

    Vector3f kMLocation = Vector3f(kC.X(),kC.Y()+fR,kC.Z()-fR);
    Vector3f kMDirection = -kMLocation;
    Vector3f kMLeft = Vector3f::UNIT_X;
    Vector3f kMUp = kMDirection.Cross(kMLeft);

    Vector3f kWLocation =  m_spkScene->Rotate()*kMLocation;
    Vector3f kWLeft = m_spkScene->Rotate()*kMLeft;
    Vector3f kWUp = m_spkScene->Rotate()*kMUp;
    Vector3f kWDirection = m_spkScene->Rotate()*kMDirection;

    pkPCamera->SetFrame(kWLocation,kWLeft,kWUp,kWDirection);
    pkPCamera->Update();

    Texture* pkPTexture = new Texture;
    pkPTexture->SetImage(pkSunfire);
    pkPTexture->Filter() = Texture::FM_LINEAR;
    pkPTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkPTexture->Apply() = Texture::AM_MODULATE;
    pkPTexture->Wrap() = Texture::WM_CLAMP_S_CLAMP_T;

    m_spkPTexture = new ProjectedTexture(m_spkModel,pkPCamera,pkPTexture);
    m_spkScene->AttachChild(m_spkPTexture);

    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();
    return true;
}
//----------------------------------------------------------------------------
bool ProjectedTextures::OnInitialize ()
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
void ProjectedTextures::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;
    m_spkPTexture = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void ProjectedTextures::OnIdle ()
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
                "Load of Face.mgc or sunfire.mif failed. ");
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
void ProjectedTextures::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
