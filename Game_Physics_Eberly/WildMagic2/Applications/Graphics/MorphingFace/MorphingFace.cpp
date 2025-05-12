// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "MorphingFace.h"

MorphingFace g_kTheApp;

//----------------------------------------------------------------------------
MorphingFace::MorphingFace ()
    :
    Application("MorphingFace",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_fBaseTime = -1.0f;
    m_fCurrTime = -1.0f;
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool MorphingFace::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,0.0f,-1.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // load scene graph
    Stream kStream;
    bool bLoaded = kStream.Load("MorphingFace.mgc");
    if ( !bLoaded )
        return true;

    m_spkScene = (Node*) kStream.GetObjectAt(0);

    RenderState* pkState = m_spkScene->GetRenderState(
        RenderState::RS_WIREFRAME);
    m_spkWireframeState = (WireframeState*) pkState;

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 5.0f;

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void MorphingFace::OnTerminate ()
{
    m_spkWireframeState = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void MorphingFace::DrawStatistics (int iX, int iY, const ColorRGB& rkColor)
{
    if ( ms_spkRenderer )
    {
        char acMessage[256];
        float fDTime = m_fCurrTime - m_fBaseTime;
        sprintf(acMessage,"time = %6.2f",fDTime);
        ms_spkRenderer->Draw(iX,iY,rkColor,acMessage);
    }
}
//----------------------------------------------------------------------------
void MorphingFace::OnIdle ()
{
    MeasureTime();

    MoveCamera();

    if ( m_bInitialized )
    {
        // update the morph animation
        if ( m_fBaseTime == -1.0f )
        {
            m_fBaseTime = (float) GetTimeInSeconds();
            m_fCurrTime = m_fBaseTime;
        }
        else
        {
            m_fCurrTime = (float) GetTimeInSeconds();
        }
        m_spkScene->UpdateGS(m_fCurrTime - m_fBaseTime);
    }

    // draw the scene
    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
            DrawStatistics(128,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of Face.mgc failed.  "
                "Make sure this file is in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void MorphingFace::OnKeyDown (unsigned char ucKey, int, int)
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
    }
}
//----------------------------------------------------------------------------
