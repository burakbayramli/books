// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "CameraAndLightNodes.h"

CameraAndLightNodes g_kTheApp;

//----------------------------------------------------------------------------
CameraAndLightNodes::CameraAndLightNodes ()
    :
    Application("CameraAndLightNodes",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_fMax = 100.0f;
    m_fHeight = -5.0f;
    m_pkRedsky = NULL;
    m_pkGround = NULL;
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
CameraNode* CameraAndLightNodes::CreateCameraNode ()
{
    // Set up camera node.  The default translation for the node is (0,0,0),
    // so the camera is initially located at the origin.  The default rotation
    // for the node is the identity matrix.  Column 0 represents the camera
    // left, column 1 represents the camera up, and column 2 represents the
    // camera direction.
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    ms_spkCamera->Update();
    return new CameraNode(ms_spkCamera,2);
}
//----------------------------------------------------------------------------
BezierMesh* CameraAndLightNodes::CreateLightTarget (Light* pkLight)
{
    // Create a quadratic rectangle patch that is illuminated by the light.
    // To hide the artifacts of vertex normal lighting on a grid, the patch
    // is slightly bent so that the intersection with a plane is nearly
    // circular.  The patch is translated slightly below the plane of the
    // ground to hide the corners and the jaggies.

    int iDegree = 2;
    int iCtrlQuantity = (iDegree+1)*(iDegree+1);
    int* aiIndex = new int[iCtrlQuantity];
    int i;
    for (i = 0; i < iCtrlQuantity; i++)
        aiIndex[i] = i;

    float fHeight = m_fHeight + 0.34f;
    float fDC0 = 0.5f;
    float fDE0 = 0.25f;
    float fDC1 = 0.5f;
    float fDE1 = 0.25f;
    Vector3f* akVertex = new Vector3f[iCtrlQuantity];
    akVertex[0] = Vector3f(-4.0f,fHeight-fDC0,-8.0f);
    akVertex[1] = Vector3f(-4.0f,fHeight-fDE0, 0.0f);
    akVertex[2] = Vector3f(-4.0f,fHeight-fDC0,+8.0f);
    akVertex[3] = Vector3f( 0.0f,fHeight-fDE0,-8.0f);
    akVertex[4] = Vector3f( 0.0f,fHeight     , 0.0f);
    akVertex[5] = Vector3f( 0.0f,fHeight-fDE0,+8.0f);
    akVertex[6] = Vector3f(+4.0f,fHeight-fDC1,-8.0f);
    akVertex[7] = Vector3f(+4.0f,fHeight-fDE1, 0.0f);
    akVertex[8] = Vector3f(+4.0f,fHeight-fDC1,+8.0f);


    BezierPatchPtr* aspkPatch = new BezierPatchPtr[1];
    aspkPatch[0] = new BezierRectangle2(aiIndex);
    BezierMesh* pkMesh = new BezierMesh(iCtrlQuantity,akVertex,
        true,NULL,NULL,1,aspkPatch);

    pkMesh->Tessellate(4);

    AlphaState* pkAS = new AlphaState;
    pkAS->BlendEnabled() = true;
    pkMesh->SetRenderState(pkAS);

    MaterialState* pkMS = new MaterialState;
    pkMS->Emissive() = ColorRGB(0.0f,0.0f,0.0f);
    pkMS->Ambient() = ColorRGB(0.5f,0.5f,0.5f);
    pkMS->Diffuse() = ColorRGB(1.0f,0.85f,0.75f);
    pkMS->Specular() = ColorRGB(0.8f,0.8f,0.8f);
    pkMS->Shininess() = 1.0f;
    pkMS->Alpha() = 0.5f;
    pkMesh->SetRenderState(pkMS);

    LightState* pkLS = new LightState;
    pkLS->Attach(pkLight);
    pkMesh->SetRenderState(pkLS);

    return pkMesh;
}
//----------------------------------------------------------------------------
Node* CameraAndLightNodes::CreateLightFixture (LightPtr& rspkAdjustableLight)
{
    Node* pkLFixture = new Node(2);

    // point light illuminates the target
    PointLight* pkPLight = new PointLight;
    pkPLight->Ambient() = ColorRGB(0.25f,0.25f,0.25f);
    pkPLight->Diffuse() = ColorRGB(0.5f,0.5f,0.5f);
    pkPLight->Specular() = ColorRGB(0.1f,0.1f,0.1f);
    rspkAdjustableLight = pkPLight;

    // the target itself
    BezierMesh* pkLTarget = CreateLightTarget(pkPLight);

    // Encapsulate the light in a light node.  Rotate the light node so the
    // light points down.
    LightNode* pkLNode = new LightNode(pkPLight);
    pkLNode->Rotate().FromAxisAngle(Vector3f::UNIT_X,Mathf::HALF_PI);

    pkLFixture->AttachChild(pkLNode);
    pkLFixture->AttachChild(pkLTarget);

    return pkLFixture;
}
//----------------------------------------------------------------------------
TriMesh* CameraAndLightNodes::CreateGround ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(-m_fMax,m_fHeight,-m_fMax);
    akVertex[1] = Vector3f(+m_fMax,m_fHeight,-m_fMax);
    akVertex[2] = Vector3f(+m_fMax,m_fHeight,+m_fMax);
    akVertex[3] = Vector3f(-m_fMax,m_fHeight,+m_fMax);

    Vector3f* akNormal = new Vector3f[4];
    akNormal[0] = Vector3f::UNIT_Y;
    akNormal[1] = Vector3f::UNIT_Y;
    akNormal[2] = Vector3f::UNIT_Y;
    akNormal[3] = Vector3f::UNIT_Y;

    Vector2f* akUV = new Vector2f[4];
    akUV[0] = Vector2f(0.0f,0.0f);
    akUV[1] = Vector2f(8.0f,0.0f);
    akUV[2] = Vector2f(8.0f,8.0f);
    akUV[3] = Vector2f(0.0f,8.0f);

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 2;  aiConnect[2] = 1;
    aiConnect[3] = 0;  aiConnect[4] = 3;  aiConnect[5] = 2;

    TriMesh* pkMesh = new TriMesh(4,akVertex,akNormal,NULL,akUV,2,aiConnect);

    AmbientLight* pkALight = new AmbientLight;
    pkALight->Ambient() = ColorRGB(0.25f,0.25f,0.25f);
    pkALight->Diffuse() = ColorRGB(0.5f,0.5f,0.5f);
    pkALight->Specular() = ColorRGB(0.1f,0.1f,0.1f);
    LightState* pkLS = new LightState;
    pkLS->Attach(pkALight);
    pkMesh->SetRenderState(pkLS);

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(m_pkGround);
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    pkMesh->SetRenderState(pkTS);

    return pkMesh;
}
//----------------------------------------------------------------------------
Node* CameraAndLightNodes::CreateSceneNode ()
{
    Node* pkRoot = new Node(2);

    m_spkWireframeState = new WireframeState;
    pkRoot->SetRenderState(m_spkWireframeState);

    ZBufferState* pkZBufferState = new ZBufferState;
    pkZBufferState->Enabled() = true;
    pkZBufferState->Writeable() = true;
    pkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    pkRoot->SetRenderState(pkZBufferState);

    return pkRoot;
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::CreateScreenPolygon ()
{
    // make a sky background (z = 1)
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(0.0f,0.0f,0.0f);
    akVertex[1] = Vector3f(1.0f,0.0f,0.0f);
    akVertex[2] = Vector3f(1.0f,1.0f,0.0f);
    akVertex[3] = Vector3f(0.0f,1.0f,0.0f);

    Vector2f* akUV = new Vector2f[4];
    akUV[0] = Vector2f(0.0f,0.0f);
    akUV[1] = Vector2f(0.1f,0.0f);
    akUV[2] = Vector2f(0.1f,1.0f);
    akUV[3] = Vector2f(0.0f,1.0f);

    // add a texture to the plane
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(m_pkRedsky);
    pkTexture->Filter() = Texture::FM_LINEAR;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);

    m_spkSky = new ScreenPolygon(4,akVertex,NULL,NULL,akUV);
    m_spkSky->Mesh()->SetRenderState(pkTS);
    m_spkSky->Mesh()->UpdateRS();
}
//----------------------------------------------------------------------------
bool CameraAndLightNodes::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // scene -+--> groundPoly
    //        |
    //        +--> cameraNode --+--> lightFixture0 +--> lightNode0
    //                          |                  |
    //                          |                  +--> lightTarget0
    //                          |
    //                          +--> lightFixture1 +--> lightNode1
    //                                             |
    //                                             +--> lightTarget0

    m_pkRedsky = Image::Load("redsky.mif");
    if ( !m_pkRedsky )
        return true;

    m_pkGround = Image::Load("ground.mif");
    if ( !m_pkGround )
        return true;

    TriMesh* pkGround = CreateGround();
    Node* pkLFixture0 = CreateLightFixture(m_spkAdjustableLight0);
    Node* pkLFixture1 = CreateLightFixture(m_spkAdjustableLight1);
    m_spkCNode = CreateCameraNode();
    m_spkScene = CreateSceneNode();

    m_spkScene->AttachChild(pkGround);
    m_spkScene->AttachChild(m_spkCNode);
    m_spkCNode->AttachChild(pkLFixture0);
    m_spkCNode->AttachChild(pkLFixture1);

    CreateScreenPolygon();

    // translate the lights to the correct locations
    Camera* pkCamera = m_spkCNode->GetCamera();

    pkLFixture0->Translate() =
        pkCamera->GetLocation() +
        24.0f*pkCamera->GetDirection() + 4.0f*pkCamera->GetLeft();

    pkLFixture1->Translate() =
        pkCamera->GetLocation() +
        24.0f*pkCamera->GetDirection() - 4.0f*pkCamera->GetLeft();

    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // activate the keyboard motion system in Mgc::Application
    m_bTurretActive = true;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkWireframeState = NULL;
    m_spkCNode = NULL;
    m_spkSky = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::OnIdle ()
{
    CameraMoved();

    // draw the scene
    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkSky);
            ms_spkRenderer->Draw(m_spkScene);
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of redsky.mif or ground.mif failed.  ");
            ms_spkRenderer->Draw(8,32,ColorRGB::WHITE,
                "Make sure these files are in the same directory as the "
                "executable.");
        }
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case '+':  // increase light intensity
    case '=':
        m_spkAdjustableLight0->Intensity() += 0.1f;
        m_spkAdjustableLight1->Intensity() += 0.1f;
        break;
    case '-':  // decrease light intensity
    case '_':
        if ( m_spkAdjustableLight0->Intensity() >= 0.1f )
            m_spkAdjustableLight0->Intensity() -= 0.1f;
        if ( m_spkAdjustableLight1->Intensity() >= 0.1f )
            m_spkAdjustableLight1->Intensity() -= 0.1f;
        break;
    case 'w':
    case 'W':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        break;
    }
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::GoForward ()
{
    Vector3f kDirection = m_spkCNode->Rotate().GetColumn(2);
    m_spkCNode->Translate() += m_fTrnSpeed*kDirection;
    m_spkCNode->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::GoBackward ()
{
    Vector3f kDirection = m_spkCNode->Rotate().GetColumn(2);
    m_spkCNode->Translate() -= m_fTrnSpeed*kDirection;
    m_spkCNode->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::GoLeft ()
{
    Vector3f kUp = m_spkCNode->Rotate().GetColumn(1);
    Matrix3f kIncr;
    kIncr.FromAxisAngle(kUp,m_fRotSpeed);
    m_spkCNode->Rotate() = kIncr*m_spkCNode->Rotate();
    m_spkCNode->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void CameraAndLightNodes::GoRight ()
{
    Vector3f kUp = m_spkCNode->Rotate().GetColumn(1);
    Matrix3f kIncr;
    kIncr.FromAxisAngle(kUp,-m_fRotSpeed);
    m_spkCNode->Rotate() = kIncr*m_spkCNode->Rotate();
    m_spkCNode->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
bool CameraAndLightNodes::CameraMoved ()
{
    if ( m_bUArrowPressed )
    {
        GoForward();
        return true;
    }

    if ( m_bDArrowPressed )
    {
        GoBackward();
        return true;
    }

    if ( m_bLArrowPressed )
    {
        GoLeft();
        return true;
    }

    if ( m_bRArrowPressed )
    {
        GoRight();
        return true;
    }

    return false;
}
//----------------------------------------------------------------------------
