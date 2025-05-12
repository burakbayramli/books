// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "SkinnedBiped.h"

SkinnedBiped g_kTheApp;

//----------------------------------------------------------------------------
SkinnedBiped::SkinnedBiped ()
    :
    Application("SkinnedBiped",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool SkinnedBiped::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(80.0f,0.0f,23.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // load scene graph
    Stream kStream;
    bool bLoaded = kStream.Load("SkinnedBiped.mgc");
    if ( !bLoaded )
        return true;

    m_spkScene = (Node*) kStream.GetObjectAt(0);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 5.0f;
    m_fRotSpeed = 0.1f;

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void SkinnedBiped::OnTerminate ()
{
    m_spkScene = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void SkinnedBiped::OnIdle ()
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
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of SkinnedBiped.mgc failed. "
                "Make sure this file is in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void SkinnedBiped::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    // animation
    static float fTime = 0.0f;

    switch ( ucKey )
    {
    case '0':
        ResetTime();
        return;
    case 'g':
        fTime += 0.01f;
        break;
    case 'G':
        fTime = 0.0f;
        break;
    }

    m_spkScene->UpdateGS(fTime);
}
//----------------------------------------------------------------------------
