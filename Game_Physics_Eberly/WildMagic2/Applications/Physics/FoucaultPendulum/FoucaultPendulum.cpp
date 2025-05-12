// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "FoucaultPendulum.h"

FoucaultPendulum g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
FoucaultPendulum::FoucaultPendulum ()
    :
    Application("FoucaultPendulum",0,0,640,480,
        ColorRGB(0.819607f,0.909803f,0.713725f))
{
}
//----------------------------------------------------------------------------
FoucaultPendulum::~FoucaultPendulum ()
{
}
//----------------------------------------------------------------------------
bool FoucaultPendulum::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // initialize the physics module
    m_kModule.AngularSpeed = 0.0001;
    m_kModule.Latitude = 0.25*Mathd::PI;
    m_kModule.GDivL = 1.0;

    double dTime = 0.0;
    double dDeltaTime = 0.001;
    double dTheta = 0.0;
    double dDTheta = 0.1;
    double dPhi = 0.75;
    double dDPhi = 0.0;
    m_kModule.Initialize(dTime,dDeltaTime,dTheta,dDTheta,dPhi,dDPhi);

    // set up the scene graph
    m_spkScene = new Node(3);
    m_spkScene->AttachChild(CreateFloor());
    m_spkScene->AttachChild(CreatePath());
    m_spkScene->AttachChild(CreatePendulum());
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    float fAngle = 0.1f*Mathf::PI;
    float fCos = Mathf::Cos(fAngle), fSin = Mathf::Sin(fAngle);
    Vector3f kCUp(-fSin,0.0f,fCos);
    Vector3f kCDir(-fCos,0.0f,-fSin);
    Vector3f kCLeft = kCUp.Cross(kCDir);
    Vector3f kCLoc(23.0f,0.0f,8.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;

    DoPhysical();
    return true;
}
//----------------------------------------------------------------------------
void FoucaultPendulum::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkPendulum = NULL;
    m_spkWireframe = NULL;
    m_spkPath = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void FoucaultPendulum::OnIdle ()
{
    MeasureTime();
    MoveCamera();
    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

#ifndef SINGLE_STEP
    DoPhysical();
#endif

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void FoucaultPendulum::OnKeyDown (unsigned char ucKey, int iX, int iY)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'w':  // toggle wireframe
    case 'W':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;

#ifdef SINGLE_STEP
    case 'g':
    case 'G':
        DoPhysical();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
TriMesh* FoucaultPendulum::CreateFloor ()
{
    TriMesh* pkFloor = NULL;
    CreateRectangleMesh(pkFloor,Vector3f::ZERO,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,32.0f,32.0f,false,false,true);

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("wood.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    pkFloor->SetRenderState(pkTS);

    return pkFloor;
}
//----------------------------------------------------------------------------
Polypoint* FoucaultPendulum::CreatePath ()
{
    // particles used to display path of pendulum
    const int iPQuantity = 8192;
    Vector3f* akPVertex = new Vector3f[iPQuantity];
    ColorRGB* akPColor = new ColorRGB[iPQuantity];
    for (int i = 0; i < iPQuantity; i++)
    {
        akPVertex[i] = Vector3f::ZERO;
        akPColor[i] = ColorRGB::WHITE;
    }
    m_spkPath = new Polypoint(iPQuantity,akPVertex,NULL,akPColor,NULL);
    m_spkPath->SetActiveQuantity(0);
    m_iNextPoint = 0;
    m_fColorDiff = 1.0f/(float)iPQuantity;

    return m_spkPath;
}
//----------------------------------------------------------------------------
Node* FoucaultPendulum::CreatePendulum ()
{
    // pendulum rod
    TriMesh* pkRod = NULL;
    CreateCylinderMesh(pkRod,2,8,Vector3f(0.0f,0.0f,10.0f),Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,0.05f,12.0f,true,false,false,false);

    // pendulum bulb
    TriMesh* pkBulb = NULL;
    CreateSphereMesh(pkBulb,16,32,Vector3f(0.0f,0.0f,2.0f),2.0f,
        Vector3f::UNIT_X,Vector3f::UNIT_Y,Vector3f::UNIT_Z,true,false,false,
        true);

    // adjust to pair of joined cones
    int iVQuantity = pkBulb->GetVertexQuantity();
    Vector3f* akVertex = pkBulb->Vertices();
    int i;
    for (i = 0; i < iVQuantity; i++)
    {
        float fR = Mathf::Sqrt(akVertex[i].X()*akVertex[i].X() +
            akVertex[i].Y()*akVertex[i].Y());
        float fZ = akVertex[i].Z();

        if ( fZ >= 2.0f )
            fZ = 4.0f - fR;
        else
            fZ = fR;

        akVertex[i].Z() = fZ;
    }

    // translate pendulum joint to origin (for purposes of rotation)
    iVQuantity = pkRod->GetVertexQuantity();
    akVertex = pkRod->Vertices();
    for (i = 0; i < pkRod->GetVertexQuantity(); i++)
        akVertex[i].Z() -= 16.0f;
    pkRod->UpdateModelBound();
    pkRod->UpdateModelNormals();

    iVQuantity = pkBulb->GetVertexQuantity();
    akVertex = pkBulb->Vertices();
    for (i = 0; i < pkBulb->GetVertexQuantity(); i++)
        akVertex[i].Z() -= 16.0f;
    pkBulb->UpdateModelBound();
    pkBulb->UpdateModelNormals();

    // group the objects into a single subtree
    m_spkPendulum = new Node(2);
    m_spkPendulum->AttachChild(pkRod);
    m_spkPendulum->AttachChild(pkBulb);

    // translate back to original model position
    m_spkPendulum->Translate() = Vector3f(0.0f,0.0f,16.0f);

    // add a material for coloring purposes
    MaterialState* pkMS = new MaterialState;
    pkMS->Emissive() = ColorRGB::BLACK;
    pkMS->Ambient() = ColorRGB(0.1f,0.1f,0.1f);
    pkMS->Diffuse() = ColorRGB(0.99607f,0.83920f,0.67059f);
    pkMS->Specular() = ColorRGB::BLACK;
    pkMS->Shininess() = 1.0f;
    pkMS->Alpha() = 1.0f;
    m_spkPendulum->SetRenderState(pkMS);


    LightState* pkLS = new LightState;
    pkRod->SetRenderState(pkLS);
    pkBulb->SetRenderState(pkLS);

    DirectionalLight* pkLight = new DirectionalLight;
    pkLight->Ambient() = ColorRGB::WHITE;
    pkLight->Diffuse() = ColorRGB::WHITE;
    pkLight->Specular() = ColorRGB::BLACK;
    pkLight->On() = true;
    pkLight->Direction() = Vector3f(-1.0f,-1.0f,0.0f);
    pkLight->Direction().Normalize();
    pkLS->Attach(pkLight);

    pkLight = new DirectionalLight;
    pkLight->Ambient() = ColorRGB::WHITE;
    pkLight->Diffuse() = ColorRGB::WHITE;
    pkLight->Specular() = ColorRGB::BLACK;
    pkLight->On() = true;
    pkLight->Direction() = Vector3f(+1.0f,-1.0f,0.0f);
    pkLight->Direction().Normalize();
    pkLS->Attach(pkLight);

    // depth buffering only for this part of the scene graph
    ZBufferState* pkZBufferState = new ZBufferState;
    pkZBufferState->Enabled() = true;
    pkZBufferState->Writeable() = true;
    pkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkPendulum->SetRenderState(pkZBufferState);

    return m_spkPendulum;
}
//----------------------------------------------------------------------------
void FoucaultPendulum::DoPhysical ()
{
    m_kModule.Update();

    // Update the pendulum mechanism.  The pendulum rod is attached at
    // (x,y,z) = (0,0,16).  The update here has the 16 hard-coded.
    m_spkPendulum->Rotate() = m_kModule.GetOrientation();
    m_spkPendulum->UpdateGS(0.0f);

    // Draw only the active quantity of pendulum points for the initial
    // portion of the simulation.  Once all points are activated, then all
    // are drawn.
    int iVQuantity = m_spkPath->GetVertexQuantity();
    int iAQuantity = m_spkPath->GetActiveQuantity();
    if ( iAQuantity < iVQuantity )
        m_spkPath->SetActiveQuantity(++iAQuantity);

    // Add the new pendulum point to the point system.  The initial color is
    // white.  All previously known points have their colors decremented to
    // cause them to become dim over time.
    Vector3f kProj(0.0f,0.0f,-16.0f);
    kProj = m_spkPendulum->WorldScale()*(m_spkPendulum->WorldRotate()*kProj)
        + m_spkPendulum->WorldTranslate();
    kProj.Z() = 0.0f;
    Vector3f* akVertex = m_spkPath->Vertices();
    ColorRGB* akColor = m_spkPath->Colors();
    akVertex[m_iNextPoint] = kProj;
    akColor[m_iNextPoint] = ColorRGB::WHITE;
    int i;
    for (i = 0; i < m_iNextPoint; i++)
    {
        akColor[i].r -= m_fColorDiff;
        akColor[i].g -= m_fColorDiff;
        akColor[i].b -= m_fColorDiff;
    }
    for (i = m_iNextPoint+1; i < iVQuantity; i++)
    {
        akColor[i].r -= m_fColorDiff;
        akColor[i].g -= m_fColorDiff;
        akColor[i].b -= m_fColorDiff;
    }

    // prepare for the next pendulum point
    if ( ++m_iNextPoint == iVQuantity )
        m_iNextPoint = 0;
}
//----------------------------------------------------------------------------
