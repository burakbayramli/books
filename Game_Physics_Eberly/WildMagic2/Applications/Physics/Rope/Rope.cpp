// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "Rope.h"

Rope g_kTheApp;

//----------------------------------------------------------------------------
Rope::Rope ()
    :
    Application("Rope",0,0,640,480,ColorRGB(0.75f,0.85f,0.95f))
{
    m_pkSpline = NULL;
    m_pkModule = NULL;
    m_fLastIdle = 0.0f;
}
//----------------------------------------------------------------------------
Rope::~Rope ()
{
}
//----------------------------------------------------------------------------
bool Rope::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    CreateScene();

    // center-and-fit for camera viewing
    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(0.0f,-1.0f,0.0f);
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir - 0.5f*kCUp;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();
    return true;
}
//----------------------------------------------------------------------------
void Rope::OnTerminate ()
{
    delete m_pkModule;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Rope::OnIdle ()
{
    MeasureTime();
    MoveCamera();
    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

    float fCurrIdle = GetTimeInSeconds();
    float fDiff = fCurrIdle - m_fLastIdle;
    if ( fDiff >= 0.001f )
    {
        m_fLastIdle = fCurrIdle;

        DoPhysical();

        ms_spkRenderer->ClearBuffers();
        if ( ms_spkRenderer->BeginScene() )
        {
            ms_spkRenderer->Draw(m_spkScene);
            ms_spkRenderer->EndScene();
        }
        ms_spkRenderer->DisplayBackBuffer();
    }

    UpdateClicks();
}
//----------------------------------------------------------------------------
void Rope::OnKeyDown (unsigned char ucKey, int iX, int iY)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    int i;

    switch ( ucKey )
    {
    case 'g':
        DoPhysical();
        break;
    case 'w':  // toggle wireframe
    case 'W':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;
    case 'm':  // decrease mass
        if ( m_pkModule->GetMass(1) > 0.05f )
        {
            for (i = 1; i < m_pkModule->GetNumParticles()-1; i++)
                m_pkModule->SetMass(i,m_pkModule->GetMass(i)-0.01f);
        }
        break;
    case 'M':  // increase mass
        for (i = 1; i < m_pkModule->GetNumParticles()-1; i++)
            m_pkModule->SetMass(i,m_pkModule->GetMass(i)+0.01f);
        break;
    case 'c':  // decrease spring constant
        if ( m_pkModule->Constant(0) > 0.05f )
        {
            for (i = 0; i < m_pkModule->GetNumSprings(); i++)
                m_pkModule->Constant(i) -= 0.01f;
        }
        break;
    case 'C':  // increase spring constant
        for (i = 0; i < m_pkModule->GetNumSprings(); i++)
            m_pkModule->Constant(i) += 0.01f;
        break;
    case 'l':  // decrease spring resting length
        if ( m_pkModule->Length(0) > 0.05f )
        {
            for (i = 0; i < m_pkModule->GetNumSprings(); i++)
                m_pkModule->Length(i) -= 0.01f;
        }
        break;
    case 'L':  // increase spring resting length
        for (i = 0; i < m_pkModule->GetNumSprings(); i++)
            m_pkModule->Length(i) += 0.01f;
        break;
    case 'f':  // toggle wind force on/off
    case 'F':
        m_pkModule->EnableWind() = !m_pkModule->EnableWind();
        break;
    case 'r':  // toggle random wind direction change on/off
    case 'R':
        m_pkModule->EnableWindChange() = !m_pkModule->EnableWindChange();
        break;
    }
}
//----------------------------------------------------------------------------
void Rope::CreateSprings ()
{
    int iNumParticles = 8;
    float fStep = 0.1f;
    Vector3f kGravity(0.0f,0.0f,-1.0f);
    Vector3f kWind(0.0f,-0.25f,0.0f);
    float fWindChangeAmplitude = 0.01f;
    float fViscosity = 10.0f;
    m_pkModule = new PhysicsModule(iNumParticles,fStep,kGravity,kWind,
        fWindChangeAmplitude,fViscosity);

    // constant mass at interior points (end points are immovable)
    m_pkModule->SetMass(0,Mathf::MAX_REAL);
    m_pkModule->SetMass(iNumParticles-1,Mathf::MAX_REAL);
    int i;
    for (i = 1; i < iNumParticles-1; i++)
        m_pkModule->SetMass(i,1.0f);

    // initial position on a horizontal line segment
    float fFactor = 1.0f/(float)(iNumParticles-1);
    for (i = 0; i < iNumParticles; i++)
        m_pkModule->Position(i) = Vector3f(i*fFactor,0.0f,1.0f);

    // initial velocities are all zero
    for (i = 0; i < iNumParticles; i++)
        m_pkModule->Velocity(i) = Vector3f::ZERO;

    // springs are at rest in the initial horizontal configuration
    int iNumSprings = iNumParticles - 1;
    float fRestLength = 1.0f/(float)iNumSprings;
    for (i = 0; i < iNumSprings; i++)
    {
        m_pkModule->Constant(i) = 10.0f;
        m_pkModule->Length(i) = fRestLength;
    }
}
//----------------------------------------------------------------------------
void Rope::CreateRope ()
{
    // create quadratic spline using particles as control points
    int iNumCtrlPoints = m_pkModule->GetNumParticles();
    Vector3f* akCtrlPoint = m_pkModule->Positions();
    int iDegree = 2;
    m_pkSpline = new BSplineCurve3f(iNumCtrlPoints,akCtrlPoint,iDegree,
        false,true);

    // generate a tube surface whose medial axis is the spline
    bool bClosed = false;
    Vector3f kUpVector = Vector3f::UNIT_Z;
    int iMedialSamples = 64;
    int iSliceSamples = 8;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bSampleByArcLength = false;
    bool bInsideView = false;
    Vector2f kTextureMin(0.0f,0.0f), kTextureMax(1.0f,1.0f);

    m_spkRope = new TubeSurface(m_pkSpline,Radial,bClosed,kUpVector,
        iMedialSamples,iSliceSamples,bWantNormals,bWantColors,
        bSampleByArcLength,bInsideView,&kTextureMin,&kTextureMax);

    // attach a texture for the rope
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("rope.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkRope->SetRenderState(pkTS);

    m_spkTrnNode->AttachChild(m_spkRope);
}
//----------------------------------------------------------------------------
void Rope::CreateScene ()
{
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    CreateSprings();
    CreateRope();
}
//----------------------------------------------------------------------------
void Rope::DoPhysical ()
{
    // forces are independent of time, just pass in t=0
    m_pkModule->Update(0.0f);

    // Update spline curve.  Remember that the spline maintains its own
    // copy of the control points, so this update is necessary.
    int iNumCtrlPoints = m_pkModule->GetNumParticles();
    Vector3f* akCtrlPoint = m_pkModule->Positions();
    for (int i = 0; i < iNumCtrlPoints; i++)
        m_pkSpline->SetControlPoint(i,akCtrlPoint[i]);

    m_spkRope->UpdateSurface();
}
//----------------------------------------------------------------------------
