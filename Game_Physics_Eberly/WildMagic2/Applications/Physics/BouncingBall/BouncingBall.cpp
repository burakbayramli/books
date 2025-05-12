// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "BouncingBall.h"
BouncingBall g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
BouncingBall::BouncingBall ()
    :
    Application("BouncingBall",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_pkBall = NULL;
    m_fSimTime = 0.0f;
#ifdef SINGLE_STEP
    m_fSimDelta = 0.05f;
#else
    m_fSimDelta = 0.005f;
#endif
}
//----------------------------------------------------------------------------
BouncingBall::~BouncingBall ()
{
}
//----------------------------------------------------------------------------
bool BouncingBall::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    float fAngle = 0.1f*Mathf::PI;
    float fCos = Mathf::Cos(fAngle), fSin = Mathf::Sin(fAngle);
    Vector3f kCUp(-fSin,0.0f,fCos);
    Vector3f kCDir(-fCos,0.0f,-fSin);
    Vector3f kCLeft = kCUp.Cross(kCDir);
    Vector3f kCLoc(6.75f,0.0f,2.3f);

    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // create scene
    CreateBall();
    CreateFloor();
    CreateWall();
    m_spkScene = new Node;
    m_spkScene->AttachChild(m_spkFloor);
    m_spkScene->AttachChild(m_spkWall);
    m_spkScene->AttachChild(new PlanarReflection(m_spkBall,m_spkFloor,0.2f));
    m_spkScene->AttachChild(m_spkBall);

    // wireframe
    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);

    // depth buffer
    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // initialize ball with correct transformations
    DoPhysical();
    m_fSimTime = 0.0f;

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.1f;
    m_fRotSpeed = 0.01f;

    return true;
}
//----------------------------------------------------------------------------
void BouncingBall::OnTerminate ()
{
    delete m_pkBall;
    m_spkBall = NULL;
    m_spkFloor = NULL;
    m_spkWall = NULL;
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BouncingBall::OnIdle ()
{
    MeasureTime();
#ifndef SINGLE_STEP
    DoPhysical();
#endif
    DoVisual();
    UpdateClicks();
}
//----------------------------------------------------------------------------
void BouncingBall::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'w':  // toggle wireframe
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        return;
    case 's':  // toggle scaling
        m_pkBall->DoAffine() = !m_pkBall->DoAffine();
        return;
#ifdef SINGLE_STEP
    case 'g':
        m_fSimTime += m_fSimDelta;
        DoPhysical();
        return;
#endif
    }
}
//----------------------------------------------------------------------------
void BouncingBall::CreateBall ()
{
    m_pkBall = new DeformableBall(1.0f,2.0f);
    m_spkBall = new Node(1);
    m_spkBall->AttachChild(m_pkBall->Mesh());
}
//----------------------------------------------------------------------------
void BouncingBall::CreateFloor ()
{
    const float fXExtent = 8.0f;
    const float fYExtent = 16.0f;
    const float fZValue = 0.0f;
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-fXExtent,-fYExtent,fZValue);
    akVertex[1] = Vector3f(+fXExtent,-fYExtent,fZValue);
    akVertex[2] = Vector3f(+fXExtent,+fYExtent,fZValue);
    akVertex[3] = Vector3f(-fXExtent,+fYExtent,fZValue);

    Vector2f* akUV = new Vector2f[4];
    akUV[0] = Vector2f(0.0f,0.0f);
    akUV[1] = Vector2f(1.0f,0.0f);
    akUV[2] = Vector2f(1.0f,1.0f);
    akUV[3] = Vector2f(0.0f,1.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    m_spkFloor = new TriMesh(4,akVertex,NULL,NULL,akUV,2,aiConnect);

    // add a texture to the plane
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("Floor.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkFloor->SetRenderState(pkTS);
}
//----------------------------------------------------------------------------
void BouncingBall::CreateWall ()
{
    const float fXValue = -8.0f;
    const float fYExtent = 16.0f;
    const float fZExtent = 16.0f;
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(fXValue,-fYExtent,0.0f);
    akVertex[1] = Vector3f(fXValue,+fYExtent,0.0f);
    akVertex[2] = Vector3f(fXValue,+fYExtent,fZExtent);
    akVertex[3] = Vector3f(fXValue,-fYExtent,fZExtent);

    const float fMax = 4.0f;
    Vector2f* akUV = new Vector2f[4];
    akUV[0] = Vector2f(0.0f,0.0f);
    akUV[1] = Vector2f(fMax,0.0f);
    akUV[2] = Vector2f(fMax,fMax);
    akUV[3] = Vector2f(0.0f,fMax);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    m_spkWall = new TriMesh(4,akVertex,NULL,NULL,akUV,2,aiConnect);

    // add a texture to the wall
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("Wall.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkWall->SetRenderState(pkTS);
}
//----------------------------------------------------------------------------
void BouncingBall::DoPhysical ()
{
    // update the ball
    m_pkBall->DoSimulationStep(m_fSimTime);

    // ball parameters
    float fPer = m_pkBall->GetPeriod();
    float fTMin = m_pkBall->GetMinActive();
    float fTMax = m_pkBall->GetMaxActive();

    // translate the ball
    const float fYMax = 2.5f, fZMax = 0.75f;
    float fYTrn, fZTrn, fRatio, fAmp;
    float fTime = fmodf(m_fSimTime,2.0f*fPer);
    if ( fTime < fTMin )
    {
        fRatio = fTime/fTMin;
        fYTrn = fYMax*fRatio;
        fZTrn = fZMax*(1.0f - fRatio*fRatio);
    }
    else if ( fTime < fTMax )
    {
        fYTrn = fYMax;
        fAmp = m_pkBall->GetAmplitude(fTime);
        if ( fAmp <= 0.999f )
            fZTrn = -(1.0f-Mathf::Sqrt(1.0f-fAmp+fAmp*fAmp))/(1.0f-fAmp);
        else
            fZTrn = -0.5f;
    }
    else if ( fTime < fPer + fTMin )
    {
        fYTrn = -fYMax*(fTime-fPer)/fTMin;
        fZTrn = fZMax*(fTime-fTMax)*(fPer+fTMin-fTime)/(fTMin*(fPer-fTMax));
    }
    else if ( fTime < fPer + fTMax )
    {
        fYTrn = -fYMax;
        fAmp = m_pkBall->GetAmplitude(fTime-fPer);
        if ( fAmp <= 0.999f )
            fZTrn = -(1.0f-Mathf::Sqrt(1.0f-fAmp+fAmp*fAmp))/(1.0f-fAmp);
        else
            fZTrn = -0.5f;
    }
    else
    {
        fYTrn = fYMax*(fTime-2.0f*fPer)/(fPer-fTMax);
        fZTrn = fZMax*(fTime-(fPer+fTMax))*(2.0f*fPer+fTMin-fTime)/(fTMin*
            (fPer-fTMax));
    }
    m_spkBall->Translate() = Vector3f(0.0f,fYTrn,fZTrn);

    // rotate the ball
    float fAngle = Mathf::HALF_PI+0.5f*fYTrn*Mathf::PI/fYMax;
    m_spkBall->Rotate().FromAxisAngle(Vector3f::UNIT_Z,fAngle);

    // update the scene graph
    m_spkScene->UpdateGS(0.0f);

    // next simulation time
    m_fSimTime += m_fSimDelta;
}
//----------------------------------------------------------------------------
void BouncingBall::DoVisual ()
{
    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-8,ColorRGB::BLACK);

        char acMsg[256];
        sprintf(acMsg,"time = %6.4f",m_fSimTime);
        ms_spkRenderer->Draw(128,GetHeight()-8,ColorRGB::BLACK,acMsg);

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
