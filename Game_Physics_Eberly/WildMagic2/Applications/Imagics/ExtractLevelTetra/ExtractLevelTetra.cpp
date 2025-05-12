// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "ExtractLevelTetra.h"
#include "WmlExtractSurfaceTetra.h"
#include "WmlImages.h"
using namespace std;

ExtractLevelTetra g_kTheApp;

//----------------------------------------------------------------------------
ExtractLevelTetra::ExtractLevelTetra ()
    :
    Application("ExtractLevelTetra",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
ExtractLevelTetra::~ExtractLevelTetra ()
{
}
//----------------------------------------------------------------------------
void ExtractLevelTetra::CreateModel ()
{
    // extract a level surface
    ImageInt3D kImage("molecule.im");  // 97x97x116

    ExtractSurfaceTetra kESC(kImage.GetBound(0),kImage.GetBound(1),
        kImage.GetBound(2),(int*)kImage.GetData());

    vector<Vector3f> kVA, kNA;
    vector<TriangleKey> kTA;

    kESC.ExtractContour(64,kVA,kTA);
    kESC.MakeUnique(kVA,kTA);
    kESC.OrientTriangles(kVA,kTA,false);
    kESC.ComputeNormals(kVA,kTA,kNA);

    // create a triangle mesh for the surface
    int iVQuantity = (int)kVA.size();
    Vector3f* akVertex = new Vector3f[iVQuantity];
    memcpy(akVertex,&kVA.front(),iVQuantity*sizeof(Vector3f));

    Vector3f* akNormal = new Vector3f[iVQuantity];
    memcpy(akNormal,&kNA.front(),iVQuantity*sizeof(Vector3f));

    int iTQuantity = (int)kTA.size();
    int* aiConnect = new int[3*iTQuantity];
    int* piConnect = aiConnect;
    for (int i = 0; i < iTQuantity; i++)
    {
        *piConnect++ = kTA[i].V[0];
        *piConnect++ = kTA[i].V[1];
        *piConnect++ = kTA[i].V[2];
    }

    m_spkModel = new TriMesh(iVQuantity,akVertex,akNormal,NULL,NULL,
        iTQuantity,aiConnect);

    // depth buffering
    ZBufferState* pkZS = new ZBufferState;
    pkZS->Enabled() = true;
    pkZS->Writeable() = true;
    pkZS->Compare() = ZBufferState::CF_LEQUAL;
    m_spkModel->SetRenderState(pkZS);

    // wireframe
    m_spkWireframe = new WireframeState;
    m_spkWireframe->Enabled() = false;
    m_spkModel->SetRenderState(m_spkWireframe);

    // material
    MaterialState* pkMS = new MaterialState;
    pkMS->Emissive() = ColorRGB(0.0f,0.0f,0.0f);
    pkMS->Ambient() = ColorRGB(0.5f,0.5f,0.5f);
    pkMS->Diffuse() = ColorRGB(0.99607f,0.83920f,0.67059f);
    pkMS->Specular() = ColorRGB(0.8f,0.8f,0.8f);
    pkMS->Shininess() = 4.0f;
    pkMS->Alpha() = 1.0f;
    m_spkModel->SetRenderState(pkMS);

    // lights
    ColorRGB kAmbient(0.25f,0.25f,0.25f);
    ColorRGB kDiffuse(0.5f,0.5f,0.5f);
    ColorRGB kSpecular(0.1f,0.1f,0.1f);

    AmbientLight* pkALight = new AmbientLight;
    pkALight->Ambient() = kAmbient;
    pkALight->Diffuse() = kDiffuse;
    pkALight->Specular() = kSpecular;
    pkALight->On() = true;

    DirectionalLight* pkDLight = new DirectionalLight;
    pkDLight->Ambient() = kAmbient;
    pkDLight->Diffuse() = kDiffuse;
    pkDLight->Specular() = kSpecular;
    pkDLight->On() = true;
    pkDLight->Direction() = Vector3f::UNIT_Z;

    LightState* pkLS = new LightState;
    pkLS->Attach(pkALight);
    pkLS->Attach(pkDLight);
    m_spkModel->SetRenderState(pkLS);
}
//----------------------------------------------------------------------------
bool ExtractLevelTetra::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    CreateModel();

    // create turret
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);
    m_spkTrnNode->AttachChild(m_spkModel);
    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    // center-and-fit camera parameters
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 5.0f;
    m_fRotSpeed = 0.1f;
    m_bTurretActive = true;
    SetTurretAxes();
    return true;
}
//----------------------------------------------------------------------------
void ExtractLevelTetra::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void ExtractLevelTetra::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

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
void ExtractLevelTetra::OnKeyDown (unsigned char ucKey, int, int)
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
    }
}
//----------------------------------------------------------------------------
