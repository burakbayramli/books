// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WaterDropFormation.h"
#include "WmlStandardMesh.h"
//#include <fstream>
//using namespace std;

WaterDropFormation g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
WaterDropFormation::WaterDropFormation ()
    :
    Application("WaterDropFormation",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_pkSpline = NULL;
    m_pkCircle = NULL;
    m_akCtrlPoint = NULL;
    m_akTarget = NULL;
    m_fSimTime = 0.0f;
    m_fSimDelta = 0.05f;
}
//----------------------------------------------------------------------------
WaterDropFormation::~WaterDropFormation ()
{
}
//----------------------------------------------------------------------------
bool WaterDropFormation::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // create scene
    m_spkScene = new Node;
    m_spkTrnNode = new Node(3);
    m_spkScene->AttachChild(m_spkTrnNode);
    CreatePlane();
    CreateWall();
    CreateWaterRoot();

    // wireframe
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);

    // depth buffer
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    Configuration0();

    // center-and-fit for camera viewing
    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();
    ms_spkCamera->SetFrustum(0.1f,1000.0f,-0.055f,0.055f,0.04125f,-0.04125f);
    float fAngle = 0.01f*Mathf::PI;
    float fCos = Mathf::Cos(fAngle), fSin = Mathf::Sin(fAngle);
    Vector3f kCUp(fSin,0.0f,-fCos);
    Vector3f kCDir(-fCos,0.0f,-fSin);
    Vector3f kCLeft(0.0f,1.0f,0.0f);
    Vector3f kCLoc = -0.9f*kWBound.Radius()*kCDir;
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

    m_fLastSeconds = GetTimeInSeconds();

    return true;
}
//----------------------------------------------------------------------------
void WaterDropFormation::OnTerminate ()
{
    delete m_pkSpline;
    delete m_pkCircle;
    delete[] m_akCtrlPoint;
    delete[] m_akTarget;

    m_spkPlane = NULL;
    m_spkWall = NULL;
    m_spkWaterRoot = NULL;
    m_spkWaterSurface = NULL;
    m_spkWaterDrop = NULL;
    m_spkWireframe = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void WaterDropFormation::OnIdle ()
{
    MeasureTime();

#ifndef SINGLE_STEP
    float fCurrSeconds = GetTimeInSeconds();
    float fDiff = fCurrSeconds - m_fLastSeconds;
    if ( fDiff >= 0.033333f )
    {
        DoPhysical();
        m_fLastSeconds = fCurrSeconds;
    }
#endif

    DoVisual();
    UpdateClicks();
}
//----------------------------------------------------------------------------
void WaterDropFormation::OnKeyDown (unsigned char ucKey, int, int)
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
        return;

#ifdef SINGLE_STEP
    case 'g':
        DoPhysical();
        return;
#endif
    }
}
//----------------------------------------------------------------------------
void WaterDropFormation::CreatePlane ()
{
    // generate the plane
    TriMesh* pkMesh = NULL;
    CreateRectangleMesh(pkMesh,Vector3f::ZERO,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,8.0f,16.0f,false,false,true);
    m_spkPlane = pkMesh;

    // add a texture to the plane
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("gravel.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkPlane->SetRenderState(pkTS);

    m_spkTrnNode->AttachChild(m_spkPlane);
}
//----------------------------------------------------------------------------
void WaterDropFormation::CreateWall ()
{
    // generate the wall
    TriMesh* pkMesh = NULL;
    CreateRectangleMesh(pkMesh,Vector3f(-8.0f,0.0f,8.0f),Vector3f::UNIT_Y,
        Vector3f::UNIT_Z,Vector3f::UNIT_X,16.0f,8.0f,false,false,true);
    m_spkWall = pkMesh;

    // add a texture to the wall
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("stone.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkWall->SetRenderState(pkTS);

    m_spkTrnNode->AttachChild(m_spkWall);
}
//----------------------------------------------------------------------------
void WaterDropFormation::CreateWaterRoot ()
{
    m_spkWaterRoot = new Node(2);
    m_spkTrnNode->AttachChild(m_spkWaterRoot);
    m_spkWaterRoot->Translate() = 0.1f*Vector3f::UNIT_Z;
    m_spkWaterRoot->Scale() = 8.0f;

    // texture for the water objects
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("WaterA.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkWaterRoot->SetRenderState(pkTS);

    // the texture has an alpha channel of 1/2
    AlphaState* pkAS = new AlphaState;
    pkAS->BlendEnabled() = true;
    m_spkWaterRoot->SetRenderState(pkAS);
}
//----------------------------------------------------------------------------
void WaterDropFormation::Configuration0 ()
{
    // Application loops between Configuration0() and Configuration1().
    // Delete all the objects from "1" when restarting with "0".
    delete[] m_akCtrlPoint;
    delete[] m_akTarget;
    delete m_pkSpline;
    delete m_pkCircle;
    m_pkCircle = NULL;
    m_fSimTime = 0.0f;
    m_fSimDelta = 0.05f;

    m_spkWaterRoot->SetChild(0,NULL);
    m_spkWaterRoot->SetChild(1,NULL);
    m_spkWaterSurface = NULL;
    m_spkWaterDrop = NULL;

    // create water surface curve of revolution
    int iNumCtrlPoints = 13;
    int iDegree = 2;
    m_akCtrlPoint = new Vector2f[iNumCtrlPoints];
    m_akTarget = new Vector2f[iNumCtrlPoints];
    int i;
    for (i = 0; i < iNumCtrlPoints; i++)
        m_akCtrlPoint[i] = Vector2f(0.125f+0.0625f*i,0.0625f);

    float fH = 0.5f;
    float fD = 0.0625f;
    float fExtra = 0.1f;

    m_akTarget[ 0] = m_akCtrlPoint[ 0];
    m_akTarget[ 1] = m_akCtrlPoint[ 6];
    m_akTarget[ 2] = Vector2f(m_akCtrlPoint[6].X(),fH-fD-fExtra);
    m_akTarget[ 3] = Vector2f(m_akCtrlPoint[5].X(),fH-fD-fExtra);
    m_akTarget[ 4] = Vector2f(m_akCtrlPoint[5].X(),fH);
    m_akTarget[ 5] = Vector2f(m_akCtrlPoint[5].X(),fH+fD);
    m_akTarget[ 6] = Vector2f(m_akCtrlPoint[6].X(),fH+fD);
    m_akTarget[ 7] = Vector2f(m_akCtrlPoint[7].X(),fH+fD);
    m_akTarget[ 8] = Vector2f(m_akCtrlPoint[7].X(),fH);
    m_akTarget[ 9] = Vector2f(m_akCtrlPoint[7].X(),fH-fD-fExtra);
    m_akTarget[10] = Vector2f(m_akCtrlPoint[6].X(),fH-fD-fExtra);
    m_akTarget[11] = m_akCtrlPoint[ 6];
    m_akTarget[12] = m_akCtrlPoint[12];

    float* afWeight = new float[iNumCtrlPoints];
    for (i = 0; i < iNumCtrlPoints; i++)
        afWeight[i] = 1.0f;

    float fModWeight = 0.3f;
    afWeight[3] = fModWeight;
    afWeight[5] = fModWeight;
    afWeight[7] = fModWeight;
    afWeight[9] = fModWeight;

    m_pkSpline = new NURBSCurve2f(iNumCtrlPoints,m_akCtrlPoint,afWeight,
        iDegree,false,true);

    // restrict evaluation to a subinterval of the domain
    m_pkSpline->SetTimeInterval(0.5f,1.0f);

    delete[] afWeight;

    // create water surface
    float fXCenter = m_akCtrlPoint[6].X();  // middle control point
    int iCurveSamples = 32;
    int iRadialSamples = 16;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bWantUVs = true;
    bool bSampleByArcLength = false;
    bool bOutsideView = true;  // irrelevant for disk topology
    m_spkWaterSurface = new RevolutionSurface(m_pkSpline,fXCenter,
        RevolutionSurface::REV_DISK_TOPOLOGY,iCurveSamples,iRadialSamples,
        bWantNormals,bWantColors,bWantUVs,bSampleByArcLength,bOutsideView);

    m_spkWaterRoot->AttachChild(m_spkWaterSurface);
    m_spkWaterRoot->UpdateGS(0.0f);
    m_spkWaterRoot->UpdateRS();
}
//----------------------------------------------------------------------------
void WaterDropFormation::Configuration1 ()
{
    delete[] m_akTarget;
    m_akTarget = NULL;

    int iNumCtrlPoints = 5+9;
    int iDegree = 2;
    delete[] m_akCtrlPoint;
    m_akCtrlPoint = new Vector2f[iNumCtrlPoints];
    float* afWeight = new float[iNumCtrlPoints];

    // spline
    m_akCtrlPoint[0] = m_pkSpline->GetControlPoint(0);
    m_akCtrlPoint[1] = m_pkSpline->GetControlPoint(1);
    m_akCtrlPoint[2] = 0.5f*(m_pkSpline->GetControlPoint(1) +
        m_pkSpline->GetControlPoint(2));
    m_akCtrlPoint[3] = m_pkSpline->GetControlPoint(11);
    m_akCtrlPoint[4] = m_pkSpline->GetControlPoint(12);

    // circle
    int i, j;
    for (i = 2, j = 5; i <= 10; i++, j++)
        m_akCtrlPoint[j] = m_pkSpline->GetControlPoint(i);
    m_akCtrlPoint[5] = 0.5f*(m_akCtrlPoint[2]+m_akCtrlPoint[5]);
    m_akCtrlPoint[13] = m_akCtrlPoint[5];

    for (i = 0; i < iNumCtrlPoints; i++)
        afWeight[i] = 1.0f;

    afWeight[ 6] = m_pkSpline->GetControlWeight(3);
    afWeight[ 8] = m_pkSpline->GetControlWeight(5);
    afWeight[10] = m_pkSpline->GetControlWeight(7);
    afWeight[12] = m_pkSpline->GetControlWeight(9);

    delete m_pkSpline;
    m_pkSpline = new NURBSCurve2f(5,m_akCtrlPoint,afWeight,iDegree,false,
        true);

    // restrict evaluation to a subinterval of the domain
    m_pkSpline->SetTimeInterval(0.5f,1.0f);

    m_spkWaterSurface->SetCurve(m_pkSpline);

    m_pkCircle = new NURBSCurve2f(9,&m_akCtrlPoint[5],&afWeight[5],iDegree,
        true,false);
    delete[] afWeight;

    // Restrict evaluation to a subinterval of the domain.  Why 0.375?  The
    // circle NURBS is a loop and not open.  The curve is constructed with
    // iDegree (2) replicated control points.  Although the curve is
    // geometrically symmetric about the vertical axis, it is not symmetric
    // in t about the half way point (0.5) of the domain [0,1].
    m_pkCircle->SetTimeInterval(0.375f,1.0f);

    // Create water drop.  The outside view value is set to 'false' because
    // the curve (x(t),z(t)) has the property dz/dt < 0.  If the curve
    // instead had the property dz/dt > 0, then 'true' is the correct value
    // for the outside view.
    float fXCenter = m_akCtrlPoint[9].X();  // middle control point
    int iCurveSamples = 32;
    int iRadialSamples = 16;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bWantUVs = true;
    bool bSampleByArcLength = false;
    bool bOutsideView = false;
    m_spkWaterDrop = new RevolutionSurface(m_pkCircle,fXCenter,
        RevolutionSurface::REV_SPHERE_TOPOLOGY,iCurveSamples,iRadialSamples,
        bWantNormals,bWantColors,bWantUVs,bSampleByArcLength,bOutsideView);

    m_spkWaterRoot->AttachChild(m_spkWaterDrop);
    m_spkWaterRoot->UpdateRS();
}
//----------------------------------------------------------------------------
void WaterDropFormation::DoPhysical1 ()
{
    // modify control points
    float fT = m_fSimTime, fOmT = 1.0f-fT;
    float fT4 = fT*fT, fOmT4 = 1.0f - fT4;
    int iMax = m_pkSpline->GetNumCtrlPoints();
    for (int i = 0; i < iMax; i++)
    {
        if ( i != 4 )
        {
            m_pkSpline->SetControlPoint(i,fOmT*m_akCtrlPoint[i] +
                fT*m_akTarget[i]);
        }
        else
        {
            m_pkSpline->SetControlPoint(i,fOmT4*m_akCtrlPoint[i] +
                fT4*m_akTarget[i]);
        }
    }

    // modify mesh vertices
    m_spkWaterSurface->UpdateSurface();

    // update the scene graph
    m_spkScene->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void WaterDropFormation::DoPhysical2 ()
{
    if ( !m_pkCircle )
        Configuration1();

    m_fSimTime += m_fSimDelta;

    // surface evolves to a disk
    float fT = m_fSimTime - 1.0f, fOmT = 1.0f - fT;
    Vector2f kNewCtrl = fOmT*m_pkSpline->GetControlPoint(2) +
        fT*m_pkSpline->GetControlPoint(1);
    m_pkSpline->SetControlPoint(2,kNewCtrl);

    // sphere floats down a little bit
    int iMax = m_pkCircle->GetNumCtrlPoints();
    for (int i = 0; i < iMax; i++)
    {
        kNewCtrl = m_pkCircle->GetControlPoint(i) +
            Vector2f::UNIT_Y/32.0f;
        m_pkCircle->SetControlPoint(i,kNewCtrl);
    }

    m_spkWaterSurface->UpdateSurface();
    m_spkWaterDrop->UpdateSurface();
    m_spkScene->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void WaterDropFormation::DoPhysical3 ()
{
    m_fSimTime += m_fSimDelta;

    // sphere floats down a little bit
    int iMax = m_pkCircle->GetNumCtrlPoints();
    int i;
    for (i = 0; i < iMax; i++)
    {
        Vector2f kNewCtrl = m_pkCircle->GetControlPoint(i);
        if ( i == 0 || i == iMax-1 )
            kNewCtrl += 1.3f*Vector2f::UNIT_Y/32;
        else
            kNewCtrl += Vector2f::UNIT_Y/32;
        m_pkCircle->SetControlPoint(i,kNewCtrl);
    }

    m_spkWaterDrop->UpdateSurface();
    m_spkScene->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void WaterDropFormation::DoPhysical ()
{
    m_fSimTime += m_fSimDelta;
    if ( m_fSimTime <= 1.0f )
    {
        // water surface extruded to form a water drop
        DoPhysical1();
    }
    else if ( m_fSimTime <= 2.0f )
    {
        // water drop splits from water surface
        DoPhysical2();
    }
    else if ( m_fSimTime <= 4.0f )
    {
        // water drop continues downward motion, surface no longer changes
        DoPhysical3();
    }
    else
    {
        // restart
        Configuration0();
    }
}
//----------------------------------------------------------------------------
void WaterDropFormation::DoVisual ()
{
    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        //DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);

        //char acMsg[256];
        //sprintf(acMsg,"time = %6.4f",m_fSimTime);
        //ms_spkRenderer->Draw(96,GetHeight()-8,ColorRGB::WHITE,acMsg);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
