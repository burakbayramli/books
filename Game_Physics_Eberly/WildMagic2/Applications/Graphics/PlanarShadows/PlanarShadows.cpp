// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "PlanarShadows.h"

PlanarShadows g_kTheApp;

//----------------------------------------------------------------------------
PlanarShadows::PlanarShadows ()
    :
    Application("PlanarShadows",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool PlanarShadows::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(100.0f,0.0f,23.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // scene graph:
    // Root
    //     BipedNode
    //     PlaneMesh
    //     PlanarShadowNode

    m_spkScene = new Node(3);

    if ( !LoadBiped() )
        return true;

    CreatePlane();
    CreatePlanarShadowNode();

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.1f;
    m_fRotSpeed = 0.01f;

    m_bInitialized = true;

    return true;
}
//----------------------------------------------------------------------------
bool PlanarShadows::LoadBiped ()
{
    Stream kStream;
    if ( !kStream.Load("SkinnedBiped.mgc") )
        return false;

    m_spkBiped = (Node*) kStream.GetObjectAt(0);
    m_spkScene->AttachChild(m_spkBiped);

    m_spkZBuffer = WmlSmartPointerCast(ZBufferState,
        m_spkBiped->RemoveRenderState(RenderState::RS_ZBUFFER));
    m_spkScene->SetRenderState(m_spkZBuffer);

    m_spkWireframe = WmlSmartPointerCast(WireframeState,
        m_spkBiped->RemoveRenderState(RenderState::RS_WIREFRAME));
    m_spkScene->SetRenderState(m_spkWireframe);
    return true;
}
//----------------------------------------------------------------------------
void PlanarShadows::CreatePlane ()
{
    const float fXExtent = 128.0f;
    const float fYExtent = 256.0f;
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
    aiConnect[ 0] = 0;  aiConnect[ 1] = 1;  aiConnect[ 2] = 2;
    aiConnect[ 3] = 0;  aiConnect[ 4] = 2;  aiConnect[ 5] = 3;

    m_spkPlane = new TriMesh(4,akVertex,NULL,NULL,akUV,2,aiConnect);

    unsigned char* aucData = new unsigned char[4*sizeof(int)];
#ifdef WML_BIG_ENDIAN
    unsigned int uiColor = 0x8F7F00FF;
#else
    unsigned int uiColor = 0xFF007F8F;
#endif
    int* aiData = (int*)aucData;
    aiData[0] = uiColor;
    aiData[1] = uiColor;
    aiData[2] = uiColor;
    aiData[3] = uiColor;
    Image* pkImage = new Image(Image::IT_RGBA8888,2,2,aucData);
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(pkImage);
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkPlane->SetRenderState(pkTS);

    m_spkScene->AttachChild(m_spkPlane);
}
//----------------------------------------------------------------------------
void PlanarShadows::CreatePlanarShadowNode ()
{
    m_spkLight = new PointLight;
    m_spkLight->Location() = Vector3f(0.0f,-64.0f,128.0f);
    m_spkLight->Ambient() = ColorRGB(0.125f,0.125f,0.125f);
    m_spkLight->Diffuse() = ColorRGB(0.5f,0.5f,0.5f);
    m_spkLight->Specular() = ColorRGB(0.1f,0.1f,0.1f);
    m_spkLight->On() = true;

    m_spkShadowNode = new PlanarShadow(m_spkBiped,m_spkPlane,m_spkLight);
    m_spkScene->AttachChild(m_spkShadowNode);
}
//----------------------------------------------------------------------------
void PlanarShadows::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkBiped = NULL;
    m_spkPlane = NULL;
    m_spkShadowNode = NULL;
    m_spkLight = NULL;
    m_spkZBuffer = NULL;
    m_spkWireframe = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void PlanarShadows::OnIdle ()
{
    MeasureTime();

    MoveCamera();

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
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of SkinnedBiped.mgc failed. "
                "Make sure this file is in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void PlanarShadows::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    // animation
    static float fTime = 0.0f;

    switch ( ucKey )
    {
    case '0':
        ResetTime();
        return;
    case 'g':
        fTime += 0.01f;
        break;
    case 'G':
        fTime = 0.0f;
        break;
    case 'w':
    case 'W':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;
    }

    m_spkScene->UpdateGS(fTime);
}
//----------------------------------------------------------------------------
