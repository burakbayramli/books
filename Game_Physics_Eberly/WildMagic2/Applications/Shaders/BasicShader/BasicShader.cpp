// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "BasicShader.h"
#include "WmlPointLight.h"

BasicShader g_kTheApp;

//----------------------------------------------------------------------------
BasicShader::BasicShader ()
    :
    Application("BasicShader",0,0,640,480,ColorRGB(0.75f,0.75f,0.75f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
TriMesh* BasicShader::CreateSquare ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-0.5f,0.0f,-0.5f);
    akVertex[1] = Vector3f(-0.5f,0.0f, 0.5f);
    akVertex[2] = Vector3f( 0.5f,0.0f, 0.5f);
    akVertex[3] = Vector3f( 0.5f,0.0f,-0.5f);

    Vector3f* akNormal = new Vector3f[4];
    akNormal[0] = Vector3f::UNIT_Y;
    akNormal[1] = Vector3f::UNIT_Y;
    akNormal[2] = Vector3f::UNIT_Y;
    akNormal[3] = Vector3f::UNIT_Y;

    Vector2f* akUV0 = new Vector2f[4];
    akUV0[0] = Vector2f(1.0f,1.0f);
    akUV0[1] = Vector2f(1.0f,0.0f);
    akUV0[2] = Vector2f(0.0f,0.0f);
    akUV0[3] = Vector2f(0.0f,1.0f);

    Vector2f* akUV1 = new Vector2f[4];
    akUV1[0] = Vector2f(1.0f,1.0f);
    akUV1[1] = Vector2f(1.0f,0.0f);
    akUV1[2] = Vector2f(0.0f,0.0f);
    akUV1[3] = Vector2f(0.0f,1.0f);

    ColorRGB* akCol = new ColorRGB[4];
    akCol[0] = ColorRGB(1.0f,1.0f,1.0f);
    akCol[1] = ColorRGB(1.0f,0.0f,0.0f);
    akCol[2] = ColorRGB(0.0f,0.0f,0.0f);
    akCol[3] = ColorRGB(0.0f,0.0f,1.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1; aiConnect[2] = 3;
    aiConnect[3] = 3;  aiConnect[4] = 1; aiConnect[5] = 2;

    return new TriMesh(4,akVertex,akNormal,akCol,akUV0,2,aiConnect,akUV1,NULL,
        NULL,NULL);
}
//----------------------------------------------------------------------------
bool BasicShader::Setup ()
{
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    m_spkModel = new Node(1);

    // create the triangle mesh surface
    m_spkTriMesh = CreateSquare();
    m_spkTriMesh->SetVertexShader(m_spkVertShader);
    m_spkTriMesh->SetPixelShader(m_spkPixShader);

    Image* pkImage = Image::Load("texture.mif");
    if ( !pkImage )
        return false;
    Texture* pkTex = new Texture;
    pkTex->SetImage(pkImage);
    pkTex->Apply() = Texture::AM_DECAL;
    pkTex->Filter() = Texture::FM_LINEAR;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTex);
    m_spkTriMesh->SetRenderState(pkTS);

    // If your shader uses lights (or fog, or textures, etc...)
    // You need to attach them as state in WildMagic if you want to use
    // Wml<foo> to refer to them within the shader.
    PointLight* pkLight = new PointLight();
    pkLight->Ambient() = ColorRGB(0.2f, 0.2f, 0.2f);
    pkLight->Diffuse() = ColorRGB(0.8f, 0.8f, 0.8f);
    pkLight->Location() = Vector3f(0.0f, 1.0f, -5.0f);

    LightState* pkLS = new LightState;
    pkLS->Attach(pkLight);
    m_spkTriMesh->SetRenderState(pkLS);

    m_spkTriMesh->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    m_spkTriMesh->Translate() = Vector3f(0.0f,-1.0f,0.0f);
    m_spkModel->AttachChild(m_spkTriMesh);
    m_spkTrnNode->AttachChild(m_spkModel);

    return true;
}
//----------------------------------------------------------------------------
bool BasicShader::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_bVertexShader = true;
    m_spkVertShader = VertexShader::Load("BasicShader.wvs");
    m_spkPixShader = PixelShader::Load("BasicShader.wps");

    if ( !m_spkVertShader || !m_spkPixShader )
        return true;

    if ( !Setup() )
        return true;

    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    m_fN = 1.0f;
    m_fF = 100.0f;
    m_fL = -0.55f;
    m_fR = 0.55f;
    m_fT = 0.4125f;
    m_fB = -0.4125f;
    ms_spkCamera->SetFrustum(m_fN,m_fF,m_fL,m_fR,m_fT,m_fB);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    Vector3f kCLoc = -2.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void BasicShader::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BasicShader::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            //DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Load of BasicShader.{wvs,wps} or texture.mif failed.  ");
            ms_spkRenderer->Draw(8,48,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void BasicShader::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case ' ':
        m_bVertexShader = !m_bVertexShader;
        if ( m_bVertexShader )
            m_spkTriMesh->SetVertexShader(m_spkVertShader);
        else
            m_spkTriMesh->SetVertexShader(NULL);
        break;

    // tiled rendering
    case '0':  // full window
        ms_spkCamera->SetFrustum(m_fN,m_fF,m_fL,m_fR,m_fT,m_fB);
        break;
    case '1':  // upper left
        ms_spkCamera->SetFrustum(m_fN,m_fF,m_fL,0.0f,m_fT,0.0f);
        break;
    case '2':  // lower left
        ms_spkCamera->SetFrustum(m_fN,m_fF,m_fL,0.0f,0.0f,m_fB);
        break;
    case '3':  // upper right
        ms_spkCamera->SetFrustum(m_fN,m_fF,0.0f,m_fR,m_fT,0.0f);
        break;
    case '4':  // lower right
        ms_spkCamera->SetFrustum(m_fN,m_fF,0.0f,m_fR,0.0f,m_fB);
        break;
    }
}
//----------------------------------------------------------------------------
