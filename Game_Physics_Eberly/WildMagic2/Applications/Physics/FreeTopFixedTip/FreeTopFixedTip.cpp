// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "FreeTopFixedTip.h"
using namespace std;

FreeTopFixedTip g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
FreeTopFixedTip::FreeTopFixedTip ()
    :
    Application("FreeTopFixedTip",0,0,640,480,
        ColorRGB(0.839215f,0.894117f,0.972549f))
{
    m_fLastIdle = 0.0f;
}
//----------------------------------------------------------------------------
FreeTopFixedTip::~FreeTopFixedTip ()
{
}
//----------------------------------------------------------------------------
bool FreeTopFixedTip::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // initialize the simulation (TO DO.  Set up mass based on top geometry.)
    m_kModule.Gravity = 10.0;
    m_kModule.Mass = 1.0;
    m_kModule.Length = 8.0;
    m_kModule.Inertia1 = 1.0;
    m_kModule.Inertia3 = 2.0;

    double dTime = 0.0;
    double dDeltaTime = 0.01;
    double dTheta = 0.0;
    double dPhi = 0.001;
    double dPsi = 0.0;
    double dAngVel1 = 1.0;
    double dAngVel2 = 0.0;
    double dAngVel3 = 10.0;
    m_kModule.Initialize(dTime,dDeltaTime,dTheta,dPhi,dPsi,dAngVel1,dAngVel2,
        dAngVel3);

    m_fMaxPhi = Mathf::HALF_PI - Mathf::ATan(2.0f/3.0f);

    // set up the scene graph
    CreateScene();
    m_spkTopRoot->Rotate() = m_kModule.GetBodyAxes();

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
void FreeTopFixedTip::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkWireframe = NULL;
    m_spkFloor = NULL;
    m_spkTop = NULL;
    m_spkAxisTop = NULL;
    m_spkAxisVertical = NULL;
    m_spkTopRoot = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void FreeTopFixedTip::OnIdle ()
{
    MeasureTime();
    MoveCamera();
    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

    float fCurrIdle = GetTimeInSeconds();
    float fDiff = fCurrIdle - m_fLastIdle;
    if ( fDiff >= 1.0f/30.0f )
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
void FreeTopFixedTip::OnKeyDown (unsigned char ucKey, int iX, int iY)
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

#ifdef CONTROL_FRAME_RATE
    case 'p':
    case 'P':
        m_bDoPhysics = !m_bDoPhysics;
        break;
#endif
    }
}
//----------------------------------------------------------------------------
void FreeTopFixedTip::CreateScene ()
{
    // scene -+--- floor
    //        |
    //        +--- vertical axis
    //        |
    //        +--- top root ---+--- top
    //                         |
    //                         +--- top axis

    m_spkScene = new Node(3);
    m_spkTopRoot = new Node(2);

    // wireframe mode
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);

    // depth buffering
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    m_spkScene->AttachChild(CreateFloor());
    m_spkScene->AttachChild(CreateAxisVertical());
    m_spkScene->AttachChild(m_spkTopRoot);
    m_spkTopRoot->AttachChild(CreateTop());
    m_spkTopRoot->AttachChild(CreateAxisTop());
}
//----------------------------------------------------------------------------
TriMesh* FreeTopFixedTip::CreateFloor ()
{
    TriMesh* pkFloor = NULL;
    CreateRectangleMesh(pkFloor,Vector3f::ZERO,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,32.0f,32.0f,false,false,true);
    m_spkFloor = pkFloor;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("wood.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    pkFloor->SetRenderState(pkTS);

    return m_spkFloor;
}
//----------------------------------------------------------------------------
TriMesh* FreeTopFixedTip::CreateTop ()
{
    TriMesh* pkTop = NULL;
    CreateCylinderMesh(pkTop,32,32,Vector3f(0.0f,0.0f,0.0f),Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,1.0f,2.0f,false,false,true,true);
    m_spkTop = pkTop;

    // adjust shape
    int iVQuantity = m_spkTop->GetVertexQuantity();
    Vector3f* akVertex = m_spkTop->Vertices();
    int i;
    for (i = 0; i < iVQuantity; i++)
    {
        float fZ = akVertex[i].Z() + 1.0f;
        float fR = 0.75f*( fZ >= 1.5f ? 4.0f-2.0f*fZ : fZ/1.5f );
        float fInvLength = Mathf::InvSqrt(akVertex[i].X()*akVertex[i].X() +
            akVertex[i].Y()*akVertex[i].Y());
        akVertex[i].X() *= fR*fInvLength;
        akVertex[i].Y() *= fR*fInvLength;
    }
    m_spkTop->UpdateModelBound();
    m_spkTop->Translate() = Vector3f::UNIT_Z;

    // adjust texture repeat
    Vector2f* akUV = m_spkTop->Textures();
    for (i = 0; i < iVQuantity; i++)
        akUV[i].X() *= 4.0f;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("toptexture.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkTop->SetRenderState(pkTS);

    return m_spkTop;
}
//----------------------------------------------------------------------------
Polyline* FreeTopFixedTip::CreateAxisTop ()
{
    Vector3f* akVertex = new Vector3f[2];
    akVertex[0] = Vector3f::ZERO;
    akVertex[1] = 4.0f*Vector3f::UNIT_Z;

    ColorRGB* akColor = new ColorRGB[2];
    akColor[0] = ColorRGB::WHITE;
    akColor[1] = ColorRGB::WHITE;

    m_spkAxisTop = new Polyline(2,akVertex,NULL,akColor,NULL,false);
    return m_spkAxisTop;
}
//----------------------------------------------------------------------------
Polyline* FreeTopFixedTip::CreateAxisVertical ()
{
    Vector3f* akVertex = new Vector3f[2];
    akVertex[0] = Vector3f::ZERO;
    akVertex[1] = 4.0f*Vector3f::UNIT_Z;

    ColorRGB* akColor = new ColorRGB[2];
    akColor[0] = ColorRGB::BLACK;
    akColor[1] = ColorRGB::BLACK;

    m_spkAxisVertical = new Polyline(2,akVertex,NULL,akColor,NULL,false);
    return m_spkAxisVertical;
}
//----------------------------------------------------------------------------
void FreeTopFixedTip::DoPhysical ()
{
    // Stop the simulation when the top edge reaches the ground.
    if ( m_kModule.GetPhi() >= m_fMaxPhi )
    {
        // EXERCISE.  Instead of stopping the top, maintain its phi value
        // at m_fMaxPhi so that the top continues to roll on the ground.
        // In addition, arrange for the top to slow down while rolling on
        // the ground, eventually coming to a stop.
        return;
    }

    // move the top
    m_kModule.Update();
    m_spkTopRoot->Rotate() = m_kModule.GetBodyAxes();
    m_spkTopRoot->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
