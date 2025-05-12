// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "GlossMaps.h"

GlossMaps g_kTheApp;

//----------------------------------------------------------------------------
GlossMaps::GlossMaps ()
    :
    Application("GlossMaps",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
TriMesh* GlossMaps::CreateSquare ()
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

    Vector2f* akUV = new Vector2f[4];
    akUV[0] = Vector2f(1.0f,0.0f);
    akUV[1] = Vector2f(1.0f,1.0f);
    akUV[2] = Vector2f(0.0f,1.0f);
    akUV[3] = Vector2f(0.0f,0.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1; aiConnect[2] = 3;
    aiConnect[3] = 3;  aiConnect[4] = 1; aiConnect[5] = 2;

    return new TriMesh(4,akVertex,akNormal,NULL,akUV,2,aiConnect);
}
//----------------------------------------------------------------------------
bool GlossMaps::Setup ()
{
    // Create two textured polygons, one with a gloss map.  The gloss map
    // uses the alpha channel of the texture map to indicate which parts of a
    // surface have the underlying specular material.  A pure gloss map
    // typically only has content in the alpha channel, but since you are
    // applying a texture anyway, you can fill the rgb with the texture colors
    // if desired.

    Image* pkImage = Image::Load("magic.mif");
    if ( !pkImage )
        return false;

    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(2);
    m_spkScene->AttachChild(m_spkTrnNode);

    // Root node for a scene graph that contains two triangle mesh squares,
    // one with regular texture and one gloss-mapped.  The model has a light
    // state that manages a directional light and has a material with a
    // specular component.
    m_spkModel = new Node(2);

    MaterialState* pkMS = new MaterialState;
    pkMS->Ambient() = ColorRGB(0.2f,0.2f,0.2f);
    pkMS->Diffuse() = ColorRGB(0.7f,0.7f,0.7f);
    pkMS->Specular() = ColorRGB::WHITE;
    pkMS->Shininess() = 25.0f;
    m_spkModel->SetRenderState(pkMS);

    DirectionalLight* pkLight = new DirectionalLight;
    pkLight->Direction() = Vector3f(0.0f,-1.5f,0.0f);
    pkLight->Ambient() = ColorRGB(0.1f,0.1f,0.1f);
    pkLight->Diffuse() = ColorRGB(0.6f,0.6f,0.6f);
    pkLight->Specular() = ColorRGB::WHITE;
    pkLight->On() = true;
    LightState* pkLS = new LightState;
    pkLS->Attach(pkLight);
    m_spkModel->SetRenderState(pkLS);

    // regular square
    TriMesh* pkRSquare = CreateSquare();
    pkRSquare->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkRSquare->Translate() = Vector3f(1.0f,-1.0f,0.0f);
    m_spkModel->AttachChild(pkRSquare);

    // gloss-mapped square
    TriMesh* pkGSquare = CreateSquare();
    pkGSquare->Rotate().FromAxisAngle(Vector3f::UNIT_X,-0.25f*Mathf::PI);
    pkGSquare->Translate() = Vector3f(-1.0f,-1.0f,0.0f);
    Node* pkGNode = new Node;
    pkGNode->AttachChild(pkGSquare);
    m_spkModel->AttachChild(pkGNode);

    // Set up the gloss map texture for use as a gloss map.  The GlossMap
    // object will provide a texture state to manage this.
    Texture* pkGTexture = new Texture;
    pkGTexture->SetImage(pkImage);
    pkGTexture->Filter() = Texture::FM_LINEAR;
    pkGTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkGTexture->Apply() = Texture::AM_MODULATE;

    // Create a gloss map.  No other render effects are active in the demo,
    // so use texture unit zero.
    GlossMap* pkGlossMap = new GlossMap(pkGNode,pkGTexture,0);

    m_spkTrnNode->AttachChild(m_spkModel);
    m_spkTrnNode->AttachChild(pkGlossMap);

    return true;
}
//----------------------------------------------------------------------------
bool GlossMaps::OnInitialize ()
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
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir;
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
void GlossMaps::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkModel = NULL;
    m_spkLightMesh = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void GlossMaps::OnIdle ()
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
                "Load of magic.mif failed. "
                "Make sure this file is in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void GlossMaps::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
