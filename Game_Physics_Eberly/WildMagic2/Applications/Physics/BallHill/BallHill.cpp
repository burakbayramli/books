// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "BallHill.h"
#include "PhysicsModule.h"
using namespace std;

BallHill g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
BallHill::BallHill ()
    :
    Application("BallHill",0,0,640,480,
        ColorRGB(0.839215f,0.894117f,0.972549f))
{
}
//----------------------------------------------------------------------------
BallHill::~BallHill ()
{
}
//----------------------------------------------------------------------------
bool BallHill::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up the physics module
    m_kModule.Gravity = 1.0;
    m_kModule.A1 = 2.0;
    m_kModule.A2 = 1.0;
    m_kModule.A3 = 1.0;
    m_kModule.Radius = 0.1;

    double dTime = 0.0;
    double dDeltaTime = 0.01;
    double dY1 = 0.0;
    double dDY1 = 0.1;
    double dY2 = 0.0;
    double dDY2 = 0.1;
    m_kModule.Initialize(dTime,dDeltaTime,dY1,dDY1,dY2,dDY2);

    // set up the scene graph
    m_spkScene = new Node(4);

    // wireframe mode
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);

    // depth buffering
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    m_spkScene->AttachChild(CreateGround());
    m_spkScene->AttachChild(CreateHill());
    m_spkScene->AttachChild(CreateBall());
    m_spkScene->AttachChild(CreatePath());

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    float fAngle = 0.1f*Mathf::PI;
    float fCos = Mathf::Cos(fAngle), fSin = Mathf::Sin(fAngle);
    Vector3f kCUp(-fSin,0.0f,fCos);
    Vector3f kCDir(-fCos,0.0f,-fSin);
    Vector3f kCLeft = kCUp.Cross(kCDir);
    Vector3f kCLoc(4.0f,0.0f,2.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.001f;
    m_fRotSpeed = 0.001f;

    return true;
}
//----------------------------------------------------------------------------
void BallHill::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkWireframe = NULL;
    m_spkBall = NULL;
    m_spkHill = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BallHill::OnIdle ()
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
void BallHill::OnKeyDown (unsigned char ucKey, int iX, int iY)
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
TriMesh* BallHill::CreateGround ()
{
    TriMesh* pkGround = NULL;
    CreateRectangleMesh(pkGround,Vector3f::ZERO,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,32.0f,32.0f,false,false,true);
    m_spkGround = pkGround;

    // change the texture repeat
    int iVQuantity = m_spkGround->GetVertexQuantity();
    Vector2f* akUV = m_spkGround->Textures();
    for (int i = 0; i < iVQuantity; i++)
        akUV[i] *= 8.0f;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("grass.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkGround->SetRenderState(pkTS);

    return pkGround;
}
//----------------------------------------------------------------------------
TriMesh* BallHill::CreateHill ()
{
    TriMesh* pkHill = NULL;
    CreateDiskMesh(pkHill,32,32,Vector3f::ZERO,2.0f,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,false,false,true);
    m_spkHill = pkHill;

    // change the texture repeat
    int iVQuantity = m_spkHill->GetVertexQuantity();
    Vector2f* akUV = m_spkHill->Textures();
    int i;
    for (i = 0; i < iVQuantity; i++)
        akUV[i] *= 8.0f;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("gravel.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkHill->SetRenderState(pkTS);

    // adjust disk vertices to form elliptical paraboloid for the hill
    Vector3f* akVertex = m_spkHill->Vertices();
    for (i = 0; i < iVQuantity; i++)
    {
        akVertex[i].Z() = m_kModule.GetHeight(akVertex[i].X(),
            akVertex[i].Y());
    }
    m_spkHill->UpdateModelBound();
    m_spkHill->UpdateModelNormals();

    return m_spkHill;
}
//----------------------------------------------------------------------------
TriMesh* BallHill::CreateBall ()
{
    TriMesh* pkBall = NULL;
    m_fRadius = (float)m_kModule.Radius;
    CreateSphereMesh(pkBall,16,16,Vector3f::ZERO,m_fRadius,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,false,false,true,true);
    m_spkBall = pkBall;
    m_spkBall->Translate().Z() = (float)m_kModule.A3 + m_fRadius;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("BallTexture.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkBall->SetRenderState(pkTS);

    UpdateBall();

    return m_spkBall;
}
//----------------------------------------------------------------------------
Polyline* BallHill::CreatePath ()
{
    // points used to display path of ball
    const int iPQuantity = 1024;
    Vector3f* akPVertex = new Vector3f[iPQuantity];
    ColorRGB* akPColor = new ColorRGB[iPQuantity];
    for (int i = 0; i < iPQuantity; i++)
    {
        akPVertex[i] = Vector3f::ZERO;
        akPColor[i] = ColorRGB::WHITE;
    }
    m_spkPath = new Polyline(iPQuantity,akPVertex,NULL,akPColor,NULL,false);
    m_spkPath->SetActiveQuantity(0);
    m_iNextPoint = 0;

    return m_spkPath;
}
//----------------------------------------------------------------------------
void BallHill::DoPhysical ()
{
    // allow motion only while ball is above the ground level
    if ( m_spkBall->Translate().Z() <= m_fRadius )
        return;

    // move the ball
    m_kModule.Update();
    Vector3f kCenter = UpdateBall();

    // Draw only the active quantity of pendulum points for the initial
    // portion of the simulation.  Once all points are activated, then all
    // are drawn.
    int iVQuantity = m_spkPath->GetVertexQuantity();
    int iAQuantity = m_spkPath->GetActiveQuantity();
    if ( iAQuantity < iVQuantity )
        m_spkPath->SetActiveQuantity(++iAQuantity);

    // Update the path that the ball has followed.  The points are stored in
    // a circular queue, so the oldest points eventually disappear and are
    // reused for the new points.
    m_spkPath->Vertices()[m_iNextPoint] = kCenter;
    if ( ++m_iNextPoint == iVQuantity )
        m_iNextPoint = 0;
}
//----------------------------------------------------------------------------
Vector3f BallHill::UpdateBall ()
{
    // Compute the location of the center of the ball and the incremental
    // rotation implied by its motion.
    Vector3f kCenter;
    Matrix3f kIncrRot;
    m_kModule.GetData(kCenter,kIncrRot);

    // update the ball position and orientation
    m_spkBall->Translate() = kCenter;
    m_spkBall->UpdateGS(0.0f);
    m_spkBall->Rotate() = kIncrRot*m_spkBall->Rotate();

    // return the new ball center for further use by application
    return kCenter;
}
//----------------------------------------------------------------------------
