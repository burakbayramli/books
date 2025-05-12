// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "PlanarReflections.h"

PlanarReflections g_kTheApp;

//----------------------------------------------------------------------------
PlanarReflections::PlanarReflections ()
    :
    Application("PlanarReflections",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
    m_fTime = 0.0f;
    m_fDTime = 0.01f;
    m_fSkyDU = 0.00015f;
    m_fPlaneDV = 0.0015f;
}
//----------------------------------------------------------------------------
bool PlanarReflections::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(182.0f,0.0f,23.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // scene graph:
    // Root
    //     Biped
    //     PlaneMesh
    //     PlanarReflectionNode

    m_spkScene = new Node(3);

    if ( !LoadBiped() )
        return true;

    m_pkBluewater = Image::Load("bluewater.mif");
    if ( !m_pkBluewater )
        return true;

    m_pkRedsky = Image::Load("redsky.mif");
    if ( !m_pkRedsky )
        return true;

    CreatePlane();
    CreatePlanarReflectionNode();
    CreateScreenPolygon();

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
void PlanarReflections::CreateScreenPolygon ()
{
    // make a sky background (z = 1)
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(0.0f,0.0f,0.0f);
    akVertex[1] = Vector3f(1.0f,0.0f,0.0f);
    akVertex[2] = Vector3f(1.0f,1.0f,0.0f);
    akVertex[3] = Vector3f(0.0f,1.0f,0.0f);

    m_akSkyUV = new Vector2f[4];
    m_akSkyUV[0] = Vector2f(0.0f,0.0f);
    m_akSkyUV[1] = Vector2f(0.1f,0.0f);
    m_akSkyUV[2] = Vector2f(0.1f,1.0f);
    m_akSkyUV[3] = Vector2f(0.0f,1.0f);

    // add a texture to the plane
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(m_pkRedsky);
    pkTexture->Filter() = Texture::FM_LINEAR;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);

    m_spkSky = new ScreenPolygon(4,akVertex,NULL,NULL,m_akSkyUV);
    m_spkSky->Mesh()->SetRenderState(pkTS);
    m_spkSky->Mesh()->UpdateRS();
}
//----------------------------------------------------------------------------
bool PlanarReflections::LoadBiped ()
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
void PlanarReflections::CreatePlane ()
{
    const float fXExtent = 128.0f;
    const float fYExtent = 256.0f;
    const float fZValue = 0.0f;
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-fXExtent,-fYExtent,fZValue);
    akVertex[1] = Vector3f(+fXExtent,-fYExtent,fZValue);
    akVertex[2] = Vector3f(+fXExtent,+fYExtent,fZValue);
    akVertex[3] = Vector3f(-fXExtent,+fYExtent,fZValue);

    m_akPlaneUV = new Vector2f[4];
    m_akPlaneUV[0] = Vector2f(0.0f,0.0f);
    m_akPlaneUV[1] = Vector2f(1.0f,0.0f);
    m_akPlaneUV[2] = Vector2f(1.0f,1.0f);
    m_akPlaneUV[3] = Vector2f(0.0f,1.0f);

    int* aiConnect = new int[6];
    aiConnect[ 0] = 0;  aiConnect[ 1] = 1;  aiConnect[ 2] = 2;
    aiConnect[ 3] = 0;  aiConnect[ 4] = 2;  aiConnect[ 5] = 3;

    m_spkPlane = new TriMesh(4,akVertex,NULL,NULL,m_akPlaneUV,2,aiConnect);

    // add a material since we are modulating the texture
    MaterialState* pkMS = new MaterialState;
    pkMS->Ambient() = ColorRGB(1.0f,0.0f,0.0f);
    pkMS->Diffuse() = ColorRGB(1.0f,0.0f,0.0f);
    pkMS->Ambient() = ColorRGB(0.2f,0.7f,0.3f);
    pkMS->Diffuse() = ColorRGB(0.2f,0.7f,0.3f);
    m_spkPlane->SetRenderState(pkMS);

    // add a light
    DirectionalLight* pkDLight = new DirectionalLight;
    pkDLight->Direction() = Vector3f(0.0f,-0.5f,0.5f);
    pkDLight->Ambient() = ColorRGB(0.1f,0.1f,0.1f);
    pkDLight->Diffuse() = ColorRGB(0.8f,0.8f,0.8f);
    pkDLight->On() = true;
    LightState* pkLS = new LightState;
    pkLS->Attach(pkDLight);
    m_spkPlane->SetRenderState(pkLS);

    // add a texture to the plane
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(m_pkBluewater);
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_DECAL;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkPlane->SetRenderState(pkTS);

    m_spkScene->AttachChild(m_spkPlane);
}
//----------------------------------------------------------------------------
void PlanarReflections::CreatePlanarReflectionNode ()
{
    float fReflectance = 0.6f;
    m_spkReflectionNode = new PlanarReflection(m_spkBiped,m_spkPlane,
        fReflectance);
    m_spkScene->AttachChild(m_spkReflectionNode);
}
//----------------------------------------------------------------------------
void PlanarReflections::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkBiped = NULL;
    m_spkPlane = NULL;
    m_spkReflectionNode = NULL;
    m_spkZBuffer = NULL;
    m_spkWireframe = NULL;
    m_spkSky = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void PlanarReflections::OnIdle ()
{
    MeasureTime();

    MoveCamera();

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkSky);
            ms_spkRenderer->Draw(m_spkScene);
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of SkinnedBiped.mgc, bluewater.mif, or redsky.mif"
                " failed. ");
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void PlanarReflections::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    // animation
    switch ( ucKey )
    {
    case '0':
        ResetTime();
        return;
    case 'g':
        m_fTime += m_fDTime;
        if ( m_fTime < 3.5f )
        {
            // The time 3.5 is known to be the last time for the keyframe
            // controllers in SkinnedBiped.mgc.
            m_akSkyUV[0].X() += m_fSkyDU;
            m_akSkyUV[1].X() += m_fSkyDU;
            m_akSkyUV[2].X() += m_fSkyDU;
            m_akSkyUV[3].X() += m_fSkyDU;
            m_akPlaneUV[0].Y() += m_fPlaneDV;
            m_akPlaneUV[1].Y() += m_fPlaneDV;
            m_akPlaneUV[2].Y() += m_fPlaneDV;
            m_akPlaneUV[3].Y() += m_fPlaneDV;
        }
        break;
    case 'G':
        m_fTime = 0.0f;
        m_akSkyUV[0] = Vector2f(0.0f,0.0f);
        m_akSkyUV[1] = Vector2f(0.1f,0.0f);
        m_akSkyUV[2] = Vector2f(0.1f,1.0f);
        m_akSkyUV[3] = Vector2f(0.0f,1.0f);
        m_akPlaneUV[0] = Vector2f(0.0f,0.0f);
        m_akPlaneUV[1] = Vector2f(1.0f,0.0f);
        m_akPlaneUV[2] = Vector2f(1.0f,1.0f);
        m_akPlaneUV[3] = Vector2f(0.0f,1.0f);
        break;
    case 'w':
    case 'W':
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;
    }

    m_spkScene->UpdateGS(m_fTime);
}
//----------------------------------------------------------------------------
