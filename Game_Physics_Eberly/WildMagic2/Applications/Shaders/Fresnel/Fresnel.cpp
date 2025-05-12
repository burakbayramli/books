// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Fresnel.h"

Fresnel g_kTheApp;

//----------------------------------------------------------------------------
Fresnel::Fresnel ()
    :
    Application("Fresnel",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool Fresnel::Setup ()
{
    Stream kStream;
    bool bLoaded = kStream.Load("Face.mgc");
    if ( !bLoaded )
        return false;

    m_spkScene = (Node*) kStream.GetObjectAt(0);
    m_spkTriMesh = WmlSmartPointerCast(TriMesh,
        m_spkScene->GetChild(0));

    AlphaState* pkAlphaState = new AlphaState();
    //pkAlphaState->BlendEnabled() = true;
    pkAlphaState->SrcBlend() = AlphaState::SBF_ONE;
    pkAlphaState->DstBlend() = AlphaState::DBF_ONE;
    m_spkTriMesh->SetRenderState(pkAlphaState);

    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    m_spkTriMesh->SetPixelShader(m_spkPixShader);

    return true;
}
//----------------------------------------------------------------------------
bool Fresnel::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_bVertexShader = true;
    m_spkVertShader = VertexShader::Load("Fresnel.wvs");
    m_spkPixShader = PixelShader::Load("Fresnel.wps");

    if ( !m_spkVertShader || !m_spkPixShader || !Setup() )
        return true;

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,0.0f,-1.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkTriMesh;
    m_fTrnSpeed = 5.0f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void Fresnel::OnTerminate ()
{
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Fresnel::OnIdle ()
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
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Load of Fresnel.{wvs,wps} or Face.mgc failed.  ");
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
void Fresnel::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    else if ( ucKey == ' ' )
    {
        m_bVertexShader = !m_bVertexShader;
        if ( m_bVertexShader )
        {
            m_spkTriMesh->SetVertexShader( m_spkVertShader );
        }
        else
        {
            m_spkTriMesh->SetVertexShader( NULL );
        }
    }
}
//----------------------------------------------------------------------------
