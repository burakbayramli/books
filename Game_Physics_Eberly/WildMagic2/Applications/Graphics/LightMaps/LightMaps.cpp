// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "LightMaps.h"

LightMaps g_kTheApp;

//----------------------------------------------------------------------------
LightMaps::LightMaps ()
    :
    Application("LightMaps",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
TriMesh* LightMaps::CreateSquare ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-0.5f,0.0f,-0.5f);
    akVertex[1] = Vector3f(-0.5f,0.0f, 0.5f);
    akVertex[2] = Vector3f( 0.5f,0.0f, 0.5f);
    akVertex[3] = Vector3f( 0.5f,0.0f,-0.5f);

    Vector2f* akUV = new Vector2f[4];
    akUV[0] = Vector2f(1.0f,0.0f);
    akUV[1] = Vector2f(1.0f,1.0f);
    akUV[2] = Vector2f(0.0f,1.0f);
    akUV[3] = Vector2f(0.0f,0.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1; aiConnect[2] = 3;
    aiConnect[3] = 3;  aiConnect[4] = 1; aiConnect[5] = 2;

    return new TriMesh(4,akVertex,NULL,NULL,akUV,2,aiConnect);
}
//----------------------------------------------------------------------------
TriMesh* LightMaps::CreateLightMappedSquare ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-0.5f,0.0f,-0.5f);
    akVertex[1] = Vector3f(-0.5f,0.0f, 0.5f);
    akVertex[2] = Vector3f( 0.5f,0.0f, 0.5f);
    akVertex[3] = Vector3f( 0.5f,0.0f,-0.5f);

    // coordinates for the base texture
    Vector2f* akUV0 = new Vector2f[4];
    akUV0[0] = Vector2f(1.0f,0.0f);
    akUV0[1] = Vector2f(1.0f,1.0f);
    akUV0[2] = Vector2f(0.0f,1.0f);
    akUV0[3] = Vector2f(0.0f,0.0f);

    // coordinates for the light map
    Vector2f* akUV1 = new Vector2f[4];
    akUV1[0] = Vector2f(1.0f,0.0f);
    akUV1[1] = Vector2f(1.0f,1.0f);
    akUV1[2] = Vector2f(0.0f,1.0f);
    akUV1[3] = Vector2f(0.0f,0.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1; aiConnect[2] = 3;
    aiConnect[3] = 3;  aiConnect[4] = 1; aiConnect[5] = 2;

    return new TriMesh(4,akVertex,NULL,NULL,akUV0,2,aiConnect,akUV1);
}
//----------------------------------------------------------------------------
bool LightMaps::Setup ()
{
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    ImagePtr spkDoorImage = Image::Load("Door.mif");
    if ( !spkDoorImage )
        return false;

    ImagePtr spkLightMapImage = Image::Load("YellowRadial.mif");
    if ( !spkLightMapImage )
        return false;

    // base texture
    Texture* pkBaseTexture = new Texture;
    pkBaseTexture->SetImage(spkDoorImage);
    pkBaseTexture->Filter() = Texture::FM_LINEAR;
    pkBaseTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkBaseTexture->Apply() = Texture::AM_REPLACE;

    // set up to multiply with base texture
    Texture* pkLightTextureMul = new Texture;
    pkLightTextureMul->SetImage(spkLightMapImage);
    pkLightTextureMul->Filter() = Texture::FM_LINEAR;
    pkLightTextureMul->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkLightTextureMul->Apply() = Texture::AM_MODULATE;

    // set up to subtract from base texture
    Texture* pkLightTextureSub = new Texture;
    pkLightTextureSub->SetImage(spkLightMapImage);
    pkLightTextureSub->Filter() = Texture::FM_LINEAR;
    pkLightTextureSub->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkLightTextureSub->Apply() = Texture::AM_COMBINE;
    pkLightTextureSub->CombineFuncRGB() = Texture::ACF_SUBTRACT;
    pkLightTextureSub->CombineSrc0RGB() = Texture::ACS_PREVIOUS;
    pkLightTextureSub->CombineSrc1RGB() = Texture::ACS_TEXTURE;

    // use base texture
    TriMesh* pkSquare0 = CreateSquare();
    pkSquare0->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkSquare0->Translate() = Vector3f(1.0f,-1.0f,0.0f);
    m_spkTrnNode->AttachChild(pkSquare0);

    TextureState* pkTS0 = new TextureState;
    pkTS0->Set(0,pkBaseTexture);
    pkSquare0->SetRenderState(pkTS0);

    // use light map texture
    TriMesh* pkSquare1 = CreateSquare();
    pkSquare1->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkSquare1->Translate() = Vector3f(0.0f,-1.0f,0.0f);
    m_spkTrnNode->AttachChild(pkSquare1);

    TextureState* pkTS1 = new TextureState;
    pkTS1->Set(0,pkLightTextureMul);
    pkSquare1->SetRenderState(pkTS1);

    // use base texture with light map multiplied
    TriMesh* pkSquare2 = CreateLightMappedSquare();
    pkSquare2->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkSquare2->Translate() = Vector3f(-1.0f,-1.0f,0.0f);
    m_spkTrnNode->AttachChild(pkSquare2);

    TextureState* pkTS2 = new TextureState;
    pkTS2->Set(0,pkBaseTexture);
    pkTS2->Set(1,pkLightTextureMul);
    pkSquare2->SetRenderState(pkTS2);

    // use base texture with light map subtracted
    TriMesh* pkSquare3 = CreateLightMappedSquare();
    pkSquare3->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkSquare3->Translate() = Vector3f(-2.0f,-1.0f,0.0f);
    m_spkTrnNode->AttachChild(pkSquare3);

    TextureState* pkTS3 = new TextureState;
    pkTS3->Set(0,pkBaseTexture);
    pkTS3->Set(1,pkLightTextureSub);
    pkSquare3->SetRenderState(pkTS3);

    return true;
}
//----------------------------------------------------------------------------
bool LightMaps::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    if ( !Setup() )
        return true;

    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
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
void LightMaps::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void LightMaps::OnIdle ()
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
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of Door.mif or YellowRadial.mif failed.  ");
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
void LightMaps::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
