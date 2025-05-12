// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "FlowingSkirt.h"
FlowingSkirt g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
FlowingSkirt::FlowingSkirt ()
    :
    Application("FlowingSkirt",0,0,640,480,ColorRGB(0.75f,0.75f,0.75f))
{
    m_iNumCtrl = 32;
    m_iDegree = 3;
    m_fATop = 1.0f;
    m_fBTop = 1.5f;
    m_fABot = 2.0f;
    m_fBBot = 3.0f;
    m_pkSkirtTop = NULL;
    m_pkSkirtBot = NULL;
    m_afFrequency = new float[m_iNumCtrl];
}
//----------------------------------------------------------------------------
FlowingSkirt::~FlowingSkirt ()
{
    delete[] m_afFrequency;
}
//----------------------------------------------------------------------------
bool FlowingSkirt::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(1);
    m_spkScene->AttachChild(m_spkTrnNode);
    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);
    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    CreateSkirt();

    // center-and-fit for camera viewing
    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    // sample camera code
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

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.005f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();

    return true;
}
//----------------------------------------------------------------------------
void FlowingSkirt::OnTerminate ()
{
    delete m_pkSkirtTop;
    delete m_pkSkirtBot;

    m_spkScene = NULL;
    m_spkTrnNode = NULL;
    m_spkSkirt = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void FlowingSkirt::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

#ifndef SINGLE_STEP
    ModifyCurves();
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
void FlowingSkirt::OnKeyDown (unsigned char ucKey, int, int)
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
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        break;
#ifdef SINGLE_STEP
    case 'g':
    case 'G':
        ModifyCurves();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
void FlowingSkirt::CreateSkirt ()
{
    // The skirt top and bottom boundary curves are chosen to be periodic,
    // looped B-spline curves.  The top control points are generated on an
    // ellipse (x/a0)^2 + (z/b0)^2 = 1 with y = 4.  The bottom control points
    // are generated on an ellipse (x/a1)^2 + (z/b1)^2 = 1 with y = 0.

    // The vertex storage is used for the B-spline control points.  The
    // curve objects make a copy of the input points.  The vertex storage is
    // then used for the skirt mesh vertices themselves.
    int iVQuantity = 2*m_iNumCtrl;
    Vector3f* akVertex = new Vector3f[iVQuantity];
    Vector2f* akUV = new Vector2f[iVQuantity];

    int i, j;
    for (i = 0, j = m_iNumCtrl; i < m_iNumCtrl; i++, j++)
    {
        float fRatio = ((float)i)/((float)m_iNumCtrl);
        float fAngle = Mathf::TWO_PI*fRatio;
        float fSin = Mathf::Sin(fAngle);
        float fCos = Mathf::Cos(fAngle);
        float fV = 1.0f - Mathf::FAbs(2.0f*fRatio - 1.0f);

        // skirt top
        akVertex[i].X() = m_fATop*fCos;
        akVertex[i].Y() = 4.0f;
        akVertex[i].Z() = m_fBTop*fSin;
        akUV[i].X() = 1.0f;
        akUV[i].Y() = fV;

        // skirt bottom
        akVertex[j].X() = m_fABot*fCos;
        akVertex[j].Y() = 0.0f;
        akVertex[j].Z() = m_fBBot*fSin;
        akUV[j].X() = 0.0f;
        akUV[j].Y() = fV;

        // frequency of sinusoidal motion for skirt bottom
        m_afFrequency[i] = 0.5f*(1.0f+Mathf::UnitRandom());
    }

    // the control points are copied by the curve objects
    m_pkSkirtTop = new BSplineCurve3f(m_iNumCtrl,akVertex,m_iDegree,true,
        false);

    m_pkSkirtBot = new BSplineCurve3f(m_iNumCtrl,&akVertex[m_iNumCtrl],
        m_iDegree,true,false);

    // generate the triangle connectivity (cylinder connectivity)
    int iTQuantity = iVQuantity;
    int* aiConnect = new int[3*iTQuantity];
    int i0 = 0, i1 = 1, i2 = m_iNumCtrl, i3 = m_iNumCtrl+1;
    for (i = 0; i1 < m_iNumCtrl; i0 = i1++, i2 = i3++)
    {
        aiConnect[i++] = i0;
        aiConnect[i++] = i1;
        aiConnect[i++] = i3;
        aiConnect[i++] = i0;
        aiConnect[i++] = i3;
        aiConnect[i++] = i2;
    }
    aiConnect[i++] = m_iNumCtrl-1;
    aiConnect[i++] = 0;
    aiConnect[i++] = m_iNumCtrl;
    aiConnect[i++] = m_iNumCtrl-1;
    aiConnect[i++] = m_iNumCtrl;
    aiConnect[i++] = 2*m_iNumCtrl-1;

    m_spkSkirt = new TriMesh(iVQuantity,akVertex,NULL,NULL,akUV,iTQuantity,
        aiConnect);

    // attach a texture for the skirt
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("flower.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkSkirt->SetRenderState(pkTS);

    // double-sided triangles
    CullState* pkCS = new CullState;
    pkCS->Enabled() = false;
    m_spkSkirt->SetRenderState(pkCS);

    // compute the vertex values for the current B-spline curves
    UpdateSkirt();

    m_spkTrnNode->AttachChild(m_spkSkirt);
}
//----------------------------------------------------------------------------
void FlowingSkirt::UpdateSkirt ()
{
    Vector3f* akVertex = m_spkSkirt->Vertices();

    for (int i = 0, j = m_iNumCtrl; i < m_iNumCtrl; i++, j++)
    {
        float fT = ((float)i)/((float)m_iNumCtrl);
        akVertex[i] = m_pkSkirtTop->GetPosition(fT);
        akVertex[j] = m_pkSkirtBot->GetPosition(fT);
    }

    m_spkSkirt->UpdateModelBound();
    m_spkSkirt->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
void FlowingSkirt::ModifyCurves ()
{
    // perturb the skirt bottom
    float fTime = GetTimeInSeconds();
    for (int i = 0; i < m_iNumCtrl; i++)
    {
        float fRatio = ((float)i)/((float)m_iNumCtrl);
        float fAngle = Mathf::TWO_PI*fRatio;
        float fSin = Mathf::Sin(fAngle);
        float fCos = Mathf::Cos(fAngle);
        float fAmplitude = 1.0f + 0.25f*Mathf::Cos(m_afFrequency[i]*fTime);
        m_pkSkirtBot->SetControlPoint(i,
            Vector3f(fAmplitude*m_fABot*fCos,0.0f,fAmplitude*m_fBBot*fSin));
    }

    UpdateSkirt();
}
//----------------------------------------------------------------------------
