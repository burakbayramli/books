// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "BumpMaps.h"

BumpMaps g_kTheApp;

//----------------------------------------------------------------------------
BumpMaps::BumpMaps ()
    :
    Application("BumpMaps",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
TriMesh* BumpMaps::CreateSquare ()
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
    akUV0[0] = Vector2f(1.0f,0.0f);
    akUV0[1] = Vector2f(1.0f,1.0f);
    akUV0[2] = Vector2f(0.0f,1.0f);
    akUV0[3] = Vector2f(0.0f,0.0f);

    Vector2f* akUV1 = new Vector2f[4];
    akUV1[0] = Vector2f(1.0f,0.0f);
    akUV1[1] = Vector2f(1.0f,1.0f);
    akUV1[2] = Vector2f(0.0f,1.0f);
    akUV1[3] = Vector2f(0.0f,0.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1; aiConnect[2] = 3;
    aiConnect[3] = 3;  aiConnect[4] = 1; aiConnect[5] = 2;

    return new TriMesh(4,akVertex,akNormal,NULL,akUV0,2,aiConnect,NULL,NULL,
        NULL,akUV1);
}
//----------------------------------------------------------------------------
bool BumpMaps::Setup ()
{
    Image* pkSunfire = Image::Load("sunfire.mif");
    if ( !pkSunfire )
        return false;

    Image* pkNormalmap = Image::Load("normalmap.mif");
    if ( !pkNormalmap )
        return false;

    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);

    // Root node for a scene graph that contains a bump-mapped triangle mesh
    // square.
    m_spkModel = new Node(1);

    // create and attach a material with no specular component
    MaterialState* pkMS = new MaterialState;
    pkMS->Ambient() = ColorRGB(0.8f,0.8f,0.8f);
    pkMS->Diffuse() = ColorRGB(0.9f,0.9f,0.9f);
    m_spkModel->SetRenderState(pkMS);

    // Set up the base texture for the surface that gets bump mapped.  The
    // apply mode must be decal or replace, otherwise you will lose the bump
    // map effect since it is being blended in.  The bump map takes care of
    // the ambient and diffuse lighting.
    Texture* pkBumpTexture = new Texture;
    pkBumpTexture->SetImage(pkSunfire);
    pkBumpTexture->Filter() = Texture::FM_LINEAR;
    pkBumpTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkBumpTexture->Apply() = Texture::AM_REPLACE;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkBumpTexture);

    // The normal map texture.  The BumpMap object will set the apply mode.
    Texture* pkNormalMap = new Texture;
    pkNormalMap->SetImage(pkNormalmap);
    pkNormalMap->Filter() = Texture::FM_LINEAR;
    pkNormalMap->Mipmap() = Texture::MM_LINEAR;

    // create a light for the bump map object
    PointLight* pkLight = new PointLight;
    pkLight->Location() = Vector3f(0.0f,1.5f,0.0f);
    pkLight->Ambient() = ColorRGB(0.2f,0.2f,0.2f);
    pkLight->Diffuse() = ColorRGB(0.8f,0.8f,0.8f);
    pkLight->On() = true;

    // create the triangle mesh surface that will be bump mapped
    TriMesh* pkBSquare = CreateSquare();
    pkBSquare->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkBSquare->Translate() = Vector3f(0.0f,-1.0f,0.0f);
    pkBSquare->SetRenderState(pkTS);
    m_spkModel->AttachChild(pkBSquare);

    // create the bump map object
    BumpMap* pkBumpMap = new BumpMap(m_spkModel,pkNormalMap,pkLight,true);

    m_spkTrnNode->AttachChild(pkBumpMap);
    m_spkTrnNode->AttachChild(m_spkModel);

    return true;
}
//----------------------------------------------------------------------------
bool BumpMaps::OnInitialize ()
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
void BumpMaps::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BumpMaps::OnIdle ()
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
                "Load of sunfire.mif or normalmap.mif failed.  ");
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
void BumpMaps::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
