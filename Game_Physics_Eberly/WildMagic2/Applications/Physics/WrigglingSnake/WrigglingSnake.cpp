// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WrigglingSnake.h"
WrigglingSnake g_kTheApp;
float WrigglingSnake::ms_fRadius = 0.0625f;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
WrigglingSnake::WrigglingSnake ()
    :
    Application("WrigglingSnake",0,0,640,480,
        ColorRGB(1.0f,0.823529f,0.607843f))
{
    m_iNumCtrl = 32;
    m_iDegree = 3;
    m_pkCenter = NULL;
    m_afAmplitude = new float[m_iNumCtrl];
    m_afPhase = new float[m_iNumCtrl];
    m_iShellQuantity = 4;
}
//----------------------------------------------------------------------------
WrigglingSnake::~WrigglingSnake ()
{
    delete[] m_afAmplitude;
    delete[] m_afPhase;
}
//----------------------------------------------------------------------------
bool WrigglingSnake::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    CreateSnake();

    // center-and-fit for camera viewing
    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(0.0f,-1.0f,0.0f);
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();

    return true;
}
//----------------------------------------------------------------------------
void WrigglingSnake::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkSnakeRoot = NULL;
    m_spkSnakeBody = NULL;
    m_spkSnakeHead = NULL;
    m_spkWireframe = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void WrigglingSnake::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

#ifndef SINGLE_STEP
    ModifyCurve();
#endif

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-8,ColorRGB::BLACK);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void WrigglingSnake::OnKeyDown (unsigned char ucKey, int, int)
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
        break;
#ifdef SINGLE_STEP
    case 'g':
    case 'G':
        ModifyCurve();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
float WrigglingSnake::Radial (float fT)
{
    return ms_fRadius*(2.0f*fT)/(1.0f+fT);
}
//----------------------------------------------------------------------------
void WrigglingSnake::CreateSnake ()
{
    m_spkSnakeRoot = new Node(2);
    m_spkTrnNode->AttachChild(m_spkSnakeRoot);
    CreateSnakeBody();
    CreateSnakeHead();
}
//----------------------------------------------------------------------------
void WrigglingSnake::CreateSnakeBody ()
{
    // create the B-spline curve for the snake body
    Vector3f* akCtrl = new Vector3f[m_iNumCtrl];
    int i;
    for (i = 0; i < m_iNumCtrl; i++)
    {
        // control points for a snake
        float fRatio = ((float)i)/(float)(m_iNumCtrl-1);
        float fX = -1.0f + 2.0f*fRatio;
        float fXMod = 10.0f*fX - 4.0f;
        akCtrl[i].X() = fX;
        akCtrl[i].Y() = ms_fRadius*(1.5f + Mathf::ATan(fXMod)/Mathf::PI);
        akCtrl[i].Z() = 0.0f;

        // sinusoidal motion for snake
        m_afAmplitude[i] = 0.1f+fRatio*Mathf::Exp(-fRatio);
        m_afPhase[i] = 1.5f*fRatio*Mathf::TWO_PI;
    }

    // the control points are copied by the curve objects
    m_pkCenter = new BSplineCurve3f(m_iNumCtrl,akCtrl,m_iDegree,false,true);
    delete[] akCtrl;

    // generate a tube surface
    bool bClosed = false;
    Vector3f kUpVector = Vector3f::UNIT_Y;
    int iMedialSamples = 128;
    int iSliceSamples = 32;
    bool bWantNormals = true;
    bool bWantColors = false;
    bool bSampleByArcLength = false;
    bool bInsideView = false;
    Vector2f kTextureMin(0.0f,0.0f), kTextureMax(1.0f,16.0f);

    m_spkSnakeBody = new TubeSurface(m_pkCenter,Radial,bClosed,kUpVector,
        iMedialSamples,iSliceSamples,bWantNormals,bWantColors,
        bSampleByArcLength,bInsideView,&kTextureMin,&kTextureMax);

    // attach a texture for the snake body
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("snake.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    //m_spkSnakeBody->SetRenderState(pkTS);

    // Set up a light map to add to the current color.
    pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("LightMap.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_ADD;
    pkTexture->Envmap() = Texture::EM_SPHERE;
    pkTS->Set(1,pkTexture);
    m_spkSnakeBody->SetRenderState(pkTS);


    m_spkSnakeRoot->AttachChild(m_spkSnakeBody);
}
//----------------------------------------------------------------------------
void WrigglingSnake::CreateSnakeHead ()
{
    // Create the snake head as a paraboloid that is attached to the last
    // ring of vertices on the snake body.  These vertices are generated
    // for t = 1.
    int iSliceSamples = m_spkSnakeBody->GetSliceSamples();

    // number of rays (determined by slice samples of tube surface)
    int iRQ = iSliceSamples - 1;

    // number of shells (your choice, specified in application constructor)
    int iSQ = m_iShellQuantity, iSQm1 = iSQ-1;

    // generate vertices (to be filled in by UpdateSnakeHead)
    int iVQuantity = 1 + iRQ*iSQm1;
    Vector3f* akVertex = new Vector3f[iVQuantity];

    // generate vertex colors coordinates
    ColorRGB* akColor = new ColorRGB[iVQuantity];
    for (int i = 0; i < iVQuantity; i++)
        akColor[i] = ColorRGB(0.0f,0.25f,0.0f);

    // generate triangles
    int iTQuantity = iRQ*(2*iSQm1-1);
    int* aiConnect = new int[3*iTQuantity];
    int* piConnect = aiConnect;
    int iT = 0;
    for (int iR0 = iRQ-1, iR1 = 0; iR1 < iRQ; iR0 = iR1++)
    {
        *piConnect++ = 0;
        *piConnect++ = 1+iSQm1*iR0;
        *piConnect++ = 1+iSQm1*iR1;
        iT++;
        for (int iS = 1; iS < iSQm1 ; iS++)
        {
            int i00 = iS+iSQm1*iR0;
            int i01 = iS+iSQm1*iR1;
            int i10 = i00+1;
            int i11 = i01+1;
            *piConnect++ = i00;
            *piConnect++ = i10;
            *piConnect++ = i11;
            *piConnect++ = i00;
            *piConnect++ = i11;
            *piConnect++ = i01;
            iT += 2;
        }
    }

    assert( iT == iTQuantity );

    m_spkSnakeHead = new TriMesh(iVQuantity,akVertex,NULL,akColor,NULL,
        iTQuantity,aiConnect);

    m_spkSnakeRoot->AttachChild(m_spkSnakeHead);
    UpdateSnakeHead();
}
//----------------------------------------------------------------------------
void WrigglingSnake::UpdateSnake ()
{
    // The order of calls is important since the snake head uses the last
    // ring of vertices in the tube surface of the snake body.
    UpdateSnakeBody();
    UpdateSnakeHead();
    m_spkSnakeRoot->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void WrigglingSnake::UpdateSnakeBody ()
{
    m_spkSnakeBody->UpdateSurface();
}
//----------------------------------------------------------------------------
void WrigglingSnake::UpdateSnakeHead ()
{
    // get the ring of vertices at the head-end of the tube
    int iSliceSamples = m_spkSnakeBody->GetSliceSamples();
    Vector3f* akSlice = new Vector3f[iSliceSamples+1];
    m_spkSnakeBody->GetTMaxSlice(akSlice);

    // compute the center of the slice vertices
    Vector3f kCenter = akSlice[0];
    int i;
    for (i = 1; i <= iSliceSamples; i++)
        kCenter += akSlice[i];
    kCenter /= (float)(iSliceSamples+1);

    // Compute a unit-length normal of the plane of the vertices.  The normal
    // points away from tube and is used to extrude the paraboloid surface
    // for the head.
    Vector3f kEdge1 = akSlice[1] - akSlice[0];
    Vector3f kEdge2 = akSlice[2] - akSlice[0];
    Vector3f kNormal = kEdge1.UnitCross(kEdge2);

    // Adjust the normal length to include the height of the ellipsoid vertex
    // above the plane of the slice.
    kNormal *= 3.0f*ms_fRadius;

    Vector3f* akVertex = m_spkSnakeHead->Vertices();

    // origin
    akVertex[0] = kCenter + kNormal;

    // remaining shells
    const int iSQm1 = m_iShellQuantity - 1;
    float fFactor = 1.0f/iSQm1;
    for (int iR = 0; iR < iSliceSamples-1; iR++)
    {
        for (int iS = 1; iS < m_iShellQuantity; iS++)
        {
            float fT = fFactor*iS, fOmT = 1.0f - fT;
            i = iS + iSQm1*iR;
            akVertex[i] = fOmT*kCenter + fT*akSlice[iR] +
                Mathf::Pow(fOmT,0.25f)*kNormal;
        }
    }

    delete[] akSlice;

    m_spkSnakeHead->UpdateModelBound();
}
//----------------------------------------------------------------------------
void WrigglingSnake::ModifyCurve ()
{
    // perturb the snake medial curve
    float fTime = GetTimeInSeconds();
    for (int i = 0; i < m_iNumCtrl; i++)
    {
        Vector3f kCtrl = m_pkCenter->GetControlPoint(i);
        kCtrl.Z() = m_afAmplitude[i]*Mathf::Sin(3.0f*fTime+m_afPhase[i]);
        m_pkCenter->SetControlPoint(i,kCtrl);
    }

    UpdateSnake();
}
//----------------------------------------------------------------------------
