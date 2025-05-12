// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "MassPulleySpringSystem.h"

MassPulleySpringSystem g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
MassPulleySpringSystem::MassPulleySpringSystem ()
    :
    Application("MassPulleySpringSystem",0,0,640,480,
        ColorRGB(0.819607f,0.909803f,0.713725f))
{
    m_pkHelixSpline = NULL;
    m_pkCableSpline = NULL;
    m_fLastIdle = 0.0f;
}
//----------------------------------------------------------------------------
MassPulleySpringSystem::~MassPulleySpringSystem ()
{
}
//----------------------------------------------------------------------------
bool MassPulleySpringSystem::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // initialize the physics module
    m_kModule.Gravity = 1.0;
    m_kModule.Mass1 = 1.0;
    m_kModule.Mass2 = 2.0;
    m_kModule.Mass3 = 3.0;
    m_kModule.Radius = 32.0;
    m_kModule.Inertia = Mathd::HALF_PI*Mathd::Pow(m_kModule.Radius,4.0);
    m_kModule.WireLength = 375.0 + Mathd::PI*m_kModule.Radius;
    m_kModule.SpringLength = 100.0;
    m_kModule.SpringConstant = 10.0;

    double dTime = 0.0;
    double dDeltaTime = 0.1;
    double dY1 = 200.0;
    double dDY1 = -10.0;
    double dDY3 = -20.0;
    m_kModule.Initialize(dTime,dDeltaTime,dY1,dDY1,dDY3);

    // set up the scene graph
    CreateScene();
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);
    ZBufferState* pkZBufferState = new ZBufferState;
    pkZBufferState->Enabled() = true;
    pkZBufferState->Writeable() = true;
    pkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBufferState);

    // set up the camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    float fAngle = 0.1f*Mathf::PI;
    float fCos = Mathf::Cos(fAngle), fSin = Mathf::Sin(fAngle);
    Vector3f kCUp(0.0f,-fCos,-fSin);
    Vector3f kCDir(0.0f,fSin,-fCos);
    Vector3f kCLeft = kCUp.Cross(kCDir);
    Vector3f kCLoc(0.0f,48.0f,326.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.1f;
    m_fRotSpeed = 0.001f;

    return true;
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkAssembly = NULL;
    m_spkCableRoot = NULL;
    m_spkPulley = NULL;
    m_spkSpring = NULL;
    m_spkCable = NULL;
    m_spkMass1 = NULL;
    m_spkMass2 = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::OnIdle ()
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
void MassPulleySpringSystem::OnKeyDown (unsigned char ucKey, int iX, int iY)
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
    case 'g':  // single-step simulation
    case 'G':
        DoPhysical();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::CreateScene ()
{
    // set up the scene graph
    // scene -+- floor
    //        |
    //        +- assembly -+- cableRoot -+- cable
    //                     |             |
    //                     |             +- mass0
    //                     |             |
    //                     |             +- mass1
    //                     |
    //                     +- pulleyRoot -+- pulley -+- plate0
    //                                    |          |
    //                                    |          +- plate1
    //                                    |          |
    //                                    |          +- cylinder
    //                                    |
    //                                    +- spring -+- side0
    //                                               |
    //                                               +- side1
    //                                               |
    //                                               +- top
    //                                               |
    //                                               +- wire

    m_spkScene = new Node(2);
    m_spkAssembly = new Node(2);
    m_spkCableRoot = new Node(3);
    m_spkPulleyRoot = new Node(2);
    m_spkPulley = new Node(3);
    m_spkSpring = new Node(4);
    m_spkFloor = CreateFloor();
    m_spkCable = CreateCable();
    m_spkMass1 = CreateMass(1.0f);
    m_spkMass2 = CreateMass(2.0f);
    CreatePulley();
    CreateSpring();
    m_spkHelix = CreateHelix();

    m_spkScene->AttachChild(m_spkFloor);
    m_spkScene->AttachChild(m_spkAssembly);
    m_spkAssembly->AttachChild(m_spkCableRoot);
    m_spkAssembly->AttachChild(m_spkPulleyRoot);
    m_spkCableRoot->AttachChild(m_spkCable);
    m_spkCableRoot->AttachChild(m_spkMass1);
    m_spkCableRoot->AttachChild(m_spkMass2);
    m_spkPulleyRoot->AttachChild(m_spkPulley);
    m_spkPulleyRoot->AttachChild(m_spkSpring);
    m_spkPulley->AttachChild(m_spkPlate0);
    m_spkPulley->AttachChild(m_spkPlate1);
    m_spkPulley->AttachChild(m_spkCylinder);
    m_spkSpring->AttachChild(m_spkSide0);
    m_spkSpring->AttachChild(m_spkSide1);
    m_spkSpring->AttachChild(m_spkTop);
    m_spkSpring->AttachChild(m_spkHelix);

    m_spkPulleyRoot->Translate() = Vector3f(0.0f,
        (float)m_kModule.GetCurrentY3(),0.0f);
    UpdateCable();
    UpdateHelix();
}
//----------------------------------------------------------------------------
TriMesh* MassPulleySpringSystem::CreateFloor ()
{
    TriMesh* pkFloor = NULL;
    CreateRectangleMesh(pkFloor,Vector3f(0.0f,255.0f,0.0f),-Vector3f::UNIT_Z,
        Vector3f::UNIT_X,-Vector3f::UNIT_Y,1024.0f,1024.0f,false,false,true);

    // attach a texture for the floor
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
TubeSurface* MassPulleySpringSystem::CreateCable ()
{
    // create quadratic spline for the medial axis
    const int iNumCtrlPoints = 1024;
    Vector3f* akCtrlPoint = new Vector3f[iNumCtrlPoints];
    const int iDegree = 2;
    m_pkCableSpline = new BSplineCurve3f(iNumCtrlPoints,akCtrlPoint,iDegree,
        false,true);
    delete[] akCtrlPoint;

    // generate a tube surface whose medial axis is the spline
    bool bClosed = false;
    Vector3f kUpVector = Vector3f::UNIT_Z;
    int iMedialSamples = 128;
    int iSliceSamples = 16;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bSampleByArcLength = false;
    bool bInsideView = false;
    Vector2f kTextureMin(0.0f,0.0f), kTextureMax(1.0f,1.0f);

    TubeSurface* pkCable = new TubeSurface(m_pkCableSpline,CableRadial,
        bClosed,kUpVector,iMedialSamples,iSliceSamples,bWantNormals,
        bWantColors,bSampleByArcLength,bInsideView,&kTextureMin,&kTextureMax);

    // attach a texture for the cable
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("rope.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    pkCable->SetRenderState(pkTS);

    return pkCable;
}
//----------------------------------------------------------------------------
TriMesh* MassPulleySpringSystem::CreateMass (float fRadius)
{
    TriMesh* pkMass = NULL;
    CreateSphereMesh(pkMass,8,8,Vector3f::ZERO,fRadius,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,false,true,false,true);

    int iVQuantity = pkMass->GetVertexQuantity();
    ColorRGB* akColor = pkMass->Colors();
    for (int i = 0; i < iVQuantity; i++)
        akColor[i] = 0.75f*ColorRGB::WHITE;

    return pkMass;
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::CreatePulley ()
{
    float fThickness = 4.0f;

    TriMesh* pkPlate0 = NULL;
    CreateDiskMesh(pkPlate0,4,32,Vector3f::ZERO,(float)m_kModule.Radius,
        Vector3f::UNIT_X,Vector3f::UNIT_Y,Vector3f::UNIT_Z,false,false,
        true);
    pkPlate0->Translate() = Vector3f(0.0f,0.0f,0.5f*fThickness);
    m_spkPlate0 = pkPlate0;

    TriMesh* pkPlate1 = NULL;
    CreateDiskMesh(pkPlate1,4,32,Vector3f::ZERO,(float)m_kModule.Radius,
        -Vector3f::UNIT_X,Vector3f::UNIT_Y,-Vector3f::UNIT_Z,false,false,
        true);
    pkPlate1->Translate() = Vector3f(0.0f,0.0f,-0.5f*fThickness);
    m_spkPlate1 = pkPlate1;

    TriMesh* pkCylinder = NULL;
    CreateCylinderMesh(pkCylinder,2,32,Vector3f::ZERO,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,(float)m_kModule.Radius,fThickness,
        false,false,true,true);
    m_spkCylinder = pkCylinder;

    // Set up a texture to add to the current color.  The face alone is just
    // vertex colored and diffuse.  With the texture added to it, the face
    // is now shiny.
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("metal.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    m_spkTSMetal = new TextureState;
    m_spkTSMetal->Set(0,pkTexture);
    m_spkPulley->SetRenderState(m_spkTSMetal);
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::CreateSpring ()
{
    float fThickness = 4.0f;
    float fXExtent = 2.0f, fYExtent = 18.0f, fZExtent = 1.0f;
    Vector3f kCenter;
    kCenter.X() = 0.0f;

    TriMesh* pkSide0 = NULL;
    kCenter.Y() = -0.5f*(float)m_kModule.Radius;
    kCenter.Z() = 0.5f*fThickness + fZExtent;
    CreateBoxMesh(pkSide0,kCenter,Vector3f::UNIT_X,Vector3f::UNIT_Y,
        Vector3f::UNIT_Z,fXExtent,fYExtent,fZExtent,false,true,false,true);
    m_spkSide0 = pkSide0;

    TriMesh* pkSide1 = NULL;
    kCenter.Y() = -0.5f*(float)m_kModule.Radius;
    kCenter.Z() = -0.5f*fThickness - fZExtent;
    CreateBoxMesh(pkSide1,kCenter,-Vector3f::UNIT_X,Vector3f::UNIT_Y,
        -Vector3f::UNIT_Z,fXExtent,fYExtent,fZExtent,false,true,false,true);
    m_spkSide1 = pkSide1;

    TriMesh* pkTop = NULL;
    kCenter.Y() = -0.5f*(float)m_kModule.Radius - fYExtent - 0.5f;
    kCenter.Z() = 0.0f;
    CreateBoxMesh(pkTop,kCenter,Vector3f::UNIT_Z,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,0.5f*fThickness+2.0f,fXExtent,1.0f,false,true,false,
        true);
    m_spkTop = pkTop;
}
//----------------------------------------------------------------------------
TubeSurface* MassPulleySpringSystem::CreateHelix ()
{
    // create quadratic spline for the medial axis
    const int iNumCtrlPoints = 1024;
    Vector3f* akCtrlPoint = new Vector3f[iNumCtrlPoints];
    const int iDegree = 2;
    m_pkHelixSpline = new BSplineCurve3f(iNumCtrlPoints,akCtrlPoint,iDegree,
        false,true);
    delete[] akCtrlPoint;

    // generate a tube surface whose medial axis is the spline
    bool bClosed = false;
    Vector3f kUpVector = Vector3f::UNIT_Z;
    int iMedialSamples = 128;
    int iSliceSamples = 16;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bSampleByArcLength = false;
    bool bInsideView = false;
    Vector2f kTextureMin(0.0f,0.0f), kTextureMax(1.0f,1.0f);

    TubeSurface* pkHelix = new TubeSurface(m_pkHelixSpline,CableRadial,
        bClosed,kUpVector,iMedialSamples,iSliceSamples,bWantNormals,
        bWantColors,bSampleByArcLength,bInsideView,&kTextureMin,&kTextureMax);

    // attach a texture for the helix
    pkHelix->SetRenderState(m_spkTSMetal);

    return pkHelix;
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::DoPhysical ()
{
    m_kModule.Update();

    UpdatePulley();
    UpdateCable();
    UpdateHelix();
    m_spkAssembly->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::UpdatePulley ()
{
    m_spkPulley->Rotate() = Matrix3f(Vector3f::UNIT_Z,
        (float)m_kModule.GetAngle());
    m_spkPulleyRoot->Translate() = Vector3f(0.0f,
        (float)m_kModule.GetCurrentY3(),0.0f);
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::UpdateCable ()
{
    // partition control points between two vertical wires and circle piece
    const int iCQuantity = m_pkCableSpline->GetNumCtrlPoints();
    float fFraction1 = (float)m_kModule.GetCableFraction1();
    float fFraction2 = (float)m_kModule.GetCableFraction2();
    float fFractionC = 1.0f - fFraction1 - fFraction2;

    int iMin, iMax, i;
    float fMult, fT;
    Vector3f kCtrl;
    kCtrl.Z() = 0.0f;

    // set control points for wire from mass 1 to pulley midline
    iMin = 0;
    iMax = (int)(fFraction1*iCQuantity);
    if ( iMin < iMax )
    {
        fMult = 1.0f/(iMax - iMin);
        kCtrl.X() = -(float)m_kModule.Radius;
        for (i = iMin; i <= iMax; i++)
        {
            fT = fMult*(i-iMin);
            kCtrl.Y() = (1.0f - fT)*(float)m_kModule.GetCurrentY1() +
                fT*(float)m_kModule.GetCurrentY3();
            m_pkCableSpline->SetControlPoint(i,kCtrl);
        }
    }
    else
    {
        m_pkCableSpline->SetControlPoint(iMin,kCtrl);
    }

    // set control points for wire along hemicircle of pulley
    iMin = iMax+1;
    iMax += (int)(fFractionC*iCQuantity);
    fMult = 1.0f/(iMax - iMin);
    for (i = iMin; i <= iMax; i++)
    {
        fT = -1.0f + fMult*(i-iMin);
        float fAngle = fT*Mathf::PI;
        kCtrl.X() = Mathf::Cos(fAngle)*(float)m_kModule.Radius;
        kCtrl.Y() = (float)m_kModule.GetCurrentY3() +
            Mathf::Sin(fAngle)*(float)m_kModule.Radius;
        m_pkCableSpline->SetControlPoint(i,kCtrl);
    }

    // set control points for wire from pulley midline to mass 2
    iMin = iMax+1;
    iMax = iCQuantity-1;
    if ( iMin < iMax )
    {
        fMult = 1.0f/(iMax - iMin);
        kCtrl.X() = (float)m_kModule.Radius;
        for (i = iMin; i <= iMax; i++)
        {
            fT = fMult*(i-iMin);
            kCtrl.Y() = (1.0f - fT)*(float)m_kModule.GetCurrentY3() +
                fT*(float)m_kModule.GetCurrentY2();
            m_pkCableSpline->SetControlPoint(i,kCtrl);
        }
    }
    else
    {
        m_pkCableSpline->SetControlPoint(iMin,kCtrl);
    }

    m_spkCable->UpdateSurface();

    // update the mass positions
    m_spkMass1->Translate() = Vector3f(-(float)m_kModule.Radius,
        (float)m_kModule.GetCurrentY1(),0.0f);
    m_spkMass2->Translate() = Vector3f((float)m_kModule.Radius,
        (float)m_kModule.GetCurrentY2(),0.0f);
}
//----------------------------------------------------------------------------
void MassPulleySpringSystem::UpdateHelix ()
{
    // current span of helix
    float fSpan = (float)(m_kModule.GetCurrentY3() - m_kModule.Radius - 4.0);

    const int iHQuantity = m_pkHelixSpline->GetNumCtrlPoints();
    const float fHRadius = 2.0f;
    const float fTMax = 14.0f;
    float fYMult = fSpan/fTMax;
    float fDelta = fTMax/(iHQuantity-1);
    for (int i = 0; i < iHQuantity; i++)
    {
        float fT = i*fDelta;
        float fAngle = Mathf::TWO_PI*fT;
        float fCos = Mathf::Cos(fAngle);
        float fSin = Mathf::Sin(fAngle);
        Vector3f kCtrl(fHRadius*fCos,-(float)m_kModule.Radius-4.0f-fYMult*fT,
            fHRadius*fSin);
        m_pkHelixSpline->SetControlPoint(i,kCtrl);
    }

    m_spkHelix->UpdateSurface();
}
//----------------------------------------------------------------------------
