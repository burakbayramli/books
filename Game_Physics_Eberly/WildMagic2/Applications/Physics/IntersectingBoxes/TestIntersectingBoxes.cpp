// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "TestIntersectingBoxes.h"
using namespace std;

TestIntersectingBoxes g_kTheApp;
float TestIntersectingBoxes::ms_fSize = 256.0f;

// #define SINGLE_STEP

//----------------------------------------------------------------------------
TestIntersectingBoxes::TestIntersectingBoxes ()
    :
    Application("IntersectingBoxes",0,0,640,480,
        ColorRGB(0.75f,0.75f,0.75f))
{
    m_pkIB = NULL;
    m_bDoSimulation = true;
    m_fLastIdle = 0.0f;
}
//----------------------------------------------------------------------------
bool TestIntersectingBoxes::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // create some axis-aligned boxes for intersection testing
    const int iMax = 16;
    int i;
    for (i = 0; i < iMax; i++)
    {
        float fXMin = 0.5f*ms_fSize*Mathf::SymmetricRandom();
        float fXMax = fXMin + Mathf::IntervalRandom(8.0f,32.0f);
        float fYMin = 0.5f*ms_fSize*Mathf::SymmetricRandom();
        float fYMax = fYMin + Mathf::IntervalRandom(8.0f,32.0f);
        float fZMin = 0.5f*ms_fSize*Mathf::SymmetricRandom();
        float fZMax = fZMin + Mathf::IntervalRandom(8.0f,32.0f);
        m_kBoxes.push_back(
            AxisAlignedBox3f(fXMin,fXMax,fYMin,fYMax,fZMin,fZMax));
    }
    m_pkIB = new IntersectingBoxesf(m_kBoxes);

    // scene graph for the visual representation of the boxes
    m_spkScene = new Node(iMax);

    m_spkWireframe = new WireframeState;
    m_spkWireframe->Enabled() = false;
    m_spkScene->SetRenderState(m_spkWireframe);

    ZBufferState* pkZS = new ZBufferState;
    pkZS->Enabled() = true;
    pkZS->Writeable() = true;
    pkZS->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZS);

    // materials for boxes, blue for nonintersecting and red for intersecting
    m_spkBlue = new MaterialState;
    m_spkBlue->Emissive() = ColorRGB::BLACK;
    m_spkBlue->Ambient() = ColorRGB(0.25f,0.25f,0.25f);
    m_spkBlue->Diffuse() = ColorRGB(0.0f,0.0f,0.5f);
    m_spkBlue->Specular() = ColorRGB::BLACK;
    m_spkBlue->Shininess() = 1.0f;
    m_spkBlue->Alpha() = 1.0f;

    m_spkRed = new MaterialState;
    m_spkRed->Emissive() = ColorRGB::BLACK;
    m_spkRed->Ambient() = ColorRGB(0.25f,0.25f,0.25f);
    m_spkRed->Diffuse() = ColorRGB(0.5f,0.0f,0.0f);
    m_spkRed->Specular() = ColorRGB::BLACK;
    m_spkRed->Shininess() = 1.0f;
    m_spkRed->Alpha() = 1.0f;

    // a light for the scene
    DirectionalLight* pkLight = new DirectionalLight;
    pkLight->Ambient() = ColorRGB::WHITE;
    pkLight->Diffuse() = ColorRGB::WHITE;
    pkLight->Specular() = ColorRGB::BLACK;
    pkLight->On() = true;
    pkLight->Direction() = Vector3f::UNIT_Z;
    LightState* pkLS = new LightState;
    pkLS->Attach(pkLight);
    m_spkScene->SetRenderState(pkLS);

    // create visual representations of the boxes
    for (i = 0; i < iMax; i++)
    {
        Vector3f kCenter(
            0.5f*(m_kBoxes[i].XMin()+m_kBoxes[i].XMax()),
            0.5f*(m_kBoxes[i].YMin()+m_kBoxes[i].YMax()),
            0.5f*(m_kBoxes[i].ZMin()+m_kBoxes[i].ZMax()));

        float fXExtent = 0.5f*(m_kBoxes[i].XMax()-m_kBoxes[i].XMin());
        float fYExtent = 0.5f*(m_kBoxes[i].YMax()-m_kBoxes[i].YMin());
        float fZExtent = 0.5f*(m_kBoxes[i].ZMax()-m_kBoxes[i].ZMin());

        TriMesh* pkMesh = NULL;
        CreateBoxMesh(pkMesh,kCenter,Vector3f::UNIT_X,Vector3f::UNIT_Y,
            Vector3f::UNIT_Z,fXExtent,fYExtent,fZExtent,true,false,false,
            true);

        pkMesh->SetRenderState(m_spkBlue);
        m_spkScene->AttachChild(pkMesh);
    }

    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,-ms_fSize);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.5f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();
    return true;
}
//----------------------------------------------------------------------------
void TestIntersectingBoxes::OnTerminate ()
{
    delete m_pkIB;
    m_spkScene = NULL;
    m_spkBlue = NULL;
    m_spkRed = NULL;
    m_spkWireframe = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void TestIntersectingBoxes::OnIdle ()
{
    MeasureTime();

    MoveCamera();
    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

#ifndef SINGLE_STEP
    if ( m_bDoSimulation )
    {
        float fCurrIdle = GetTimeInSeconds();
        float fDiff = fCurrIdle - m_fLastIdle;
        if ( fDiff >= 1.0f/30.0f )
        {
            ModifyBoxes();
            m_fLastIdle = fCurrIdle;
        }
    }
#endif

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void TestIntersectingBoxes::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
    switch ( ucKey )
    {
    case 'w':
    case 'W':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;
    case 's':
    case 'S':
        m_bDoSimulation = !m_bDoSimulation;
        break;
#ifdef SINGLE_STEP
    case 'g':
    case 'G':
        ModifyBoxes();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
void TestIntersectingBoxes::ModifyBoxes ()
{
    int i;
    for (i = 0; i < (int)m_kBoxes.size(); i++)
    {
        AxisAlignedBox3f kBox = m_kBoxes[i];

        float fDX = Mathf::IntervalRandom(-4.0f,4.0f);
        if ( kBox.XMin()+fDX >= -ms_fSize && kBox.XMax()+fDX <= ms_fSize )
        {
            kBox.XMin() += fDX;
            kBox.XMax() += fDX;
        }

        float fDY = Mathf::IntervalRandom(-4.0f,4.0f);
        if ( kBox.YMin()+fDY >= -ms_fSize && kBox.YMax()+fDY <= ms_fSize )
        {
            kBox.YMin() += fDY;
            kBox.YMax() += fDY;
        }

        float fDZ = Mathf::IntervalRandom(-4.0f,4.0f);
        if ( kBox.ZMin()+fDZ >= -ms_fSize && kBox.ZMax()+fDZ <= ms_fSize )
        {
            kBox.ZMin() += fDZ;
            kBox.ZMax() += fDZ;
        }

        m_pkIB->SetBox(i,kBox);
        ModifyMesh(i);
    }

    m_pkIB->Update();
    m_spkScene->UpdateGS(0.0f);

    // switch material to red for any box that overlaps another
    TriMesh* pkMesh;
    for (i = 0; i < (int)m_kBoxes.size(); i++)
    {
        // reset all boxes to blue
        pkMesh = WmlStaticCast(TriMesh,m_spkScene->GetChild(i));
        pkMesh->SetRenderState(m_spkBlue);
    }

    const set<pair<int,int> >& rkOverlap = m_pkIB->GetOverlap();
    set<pair<int,int> >::const_iterator pkIter = rkOverlap.begin();
    for (/**/; pkIter != rkOverlap.end(); pkIter++)
    {
        // set intersecting boxes to red
        i = pkIter->first;
        pkMesh = WmlStaticCast(TriMesh,m_spkScene->GetChild(i));
        pkMesh->SetRenderState(m_spkRed);
        i = pkIter->second;
        pkMesh = WmlStaticCast(TriMesh,m_spkScene->GetChild(i));
        pkMesh->SetRenderState(m_spkRed);
    }

    m_spkScene->UpdateRS();
}
//----------------------------------------------------------------------------
void TestIntersectingBoxes::ModifyMesh (int i)
{
    Vector3f kCenter(
        0.5f*(m_kBoxes[i].XMin()+m_kBoxes[i].XMax()),
        0.5f*(m_kBoxes[i].YMin()+m_kBoxes[i].YMax()),
        0.5f*(m_kBoxes[i].ZMin()+m_kBoxes[i].ZMax()));

    float fXExtent = 0.5f*(m_kBoxes[i].XMax()-m_kBoxes[i].XMin());
    float fYExtent = 0.5f*(m_kBoxes[i].YMax()-m_kBoxes[i].YMin());
    float fZExtent = 0.5f*(m_kBoxes[i].ZMax()-m_kBoxes[i].ZMin());

    Vector3f kXTerm = fXExtent*Vector3f::UNIT_X;
    Vector3f kYTerm = fYExtent*Vector3f::UNIT_Y;
    Vector3f kZTerm = fZExtent*Vector3f::UNIT_Z;

    TriMesh* pkMesh = WmlStaticCast(TriMesh,m_spkScene->GetChild(i));
    Vector3f* akVertex = pkMesh->Vertices();

    akVertex[0] = kCenter - kXTerm - kYTerm - kZTerm;
    akVertex[1] = kCenter + kXTerm - kYTerm - kZTerm;
    akVertex[2] = kCenter + kXTerm + kYTerm - kZTerm;
    akVertex[3] = kCenter - kXTerm + kYTerm - kZTerm;
    akVertex[4] = kCenter - kXTerm - kYTerm + kZTerm;
    akVertex[5] = kCenter + kXTerm - kYTerm + kZTerm;
    akVertex[6] = kCenter + kXTerm + kYTerm + kZTerm;
    akVertex[7] = kCenter - kXTerm + kYTerm + kZTerm;

    pkMesh->UpdateModelBound();
}
//----------------------------------------------------------------------------
