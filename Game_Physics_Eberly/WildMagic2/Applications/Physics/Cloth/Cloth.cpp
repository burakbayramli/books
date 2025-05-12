// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "Cloth.h"

Cloth g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
Cloth::Cloth ()
    :
    Application("Cloth",0,0,640,480,ColorRGB(0.85f,0.85f,1.00f))
{
    m_pkSpline = NULL;
    m_pkModule = NULL;
}
//----------------------------------------------------------------------------
Cloth::~Cloth ()
{
}
//----------------------------------------------------------------------------
bool Cloth::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    CreateScene();

    // center-and-fit for camera viewing
    m_spkScene->UpdateGS(0.0f);
    Bound kWBound = m_spkScene->WorldBound();
    m_spkTrnNode->Translate() = -kWBound.Center();

    ms_spkCamera->SetFrustum(1.0f,100.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLeft(-1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(0.0f,1.0f,0.0f);
    Vector3f kCLoc = -3.0f*kWBound.Radius()*kCDir;
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.001f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();
    return true;
}
//----------------------------------------------------------------------------
void Cloth::OnTerminate ()
{
    delete m_pkModule;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Cloth::OnIdle ()
{
    MeasureTime();
    MoveCamera();

    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

#ifndef SINGLE_STEP
    DoPhysical();
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
void Cloth::OnKeyDown (unsigned char ucKey, int iX, int iY)
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
        DoPhysical();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
void Cloth::CreateSprings ()
{
    // set up the mass-spring system
    int iRows = 8;
    int iCols = 16;
    float fStep = 0.01f;
    Vector3f kGravity(0.0f,0.0f,-1.0f);
    Vector3f kWind(0.5f,0.0f,0.0f);
    float fViscosity = 10.0f;
    float fMaxAmplitude = 2.0f;
    m_pkModule = new PhysicsModule(iRows,iCols,fStep,kGravity,kWind,
        fViscosity,fMaxAmplitude);

    // The top row of the mesh is immovable (infinite mass).  All other
    // masses are constant.
    int iRow, iCol;
    for (iCol = 0; iCol < iCols; iCol++)
        m_pkModule->SetMass(iRows-1,iCol,Mathf::MAX_REAL);
    for (iRow = 0; iRow < iRows-1; iRow++)
    {
        for (iCol = 0; iCol < iCols; iCol++)
            m_pkModule->SetMass(iRow,iCol,1.0f);
    }

    // initial position on a vertical axis-aligned rectangle, zero velocity
    float fRFactor = 1.0f/(float)(iRows-1);
    float fCFactor = 1.0f/(float)(iCols-1);
    for (iRow = 0; iRow < iRows; iRow++)
    {
        for (iCol = 0; iCol < iCols; iCol++)
        {
            m_pkModule->Position(iRow,iCol) =
                Vector3f(iCol*fCFactor,0.0f,iRow*fRFactor);
            m_pkModule->Velocity(iRow,iCol) = Vector3f::ZERO;
        }
    }

    // springs are at rest in the initial configuration
    const float fRConstant = 1000.0f;
    const float fBConstant = 100.0f;
    Vector3f kDiff;
    for (iRow = 0; iRow < iRows; iRow++)
    {
        for (iCol = 0; iCol < iCols-1; iCol++)
        {
            m_pkModule->ConstantC(iRow,iCol) = fRConstant;
            kDiff = m_pkModule->Position(iRow,iCol+1) -
                m_pkModule->Position(iRow,iCol);
            m_pkModule->LengthC(iRow,iCol) = kDiff.Length();
        }
    }

    for (iRow = 0; iRow < iRows-1; iRow++)
    {
        for (iCol = 0; iCol < iCols; iCol++)
        {
            m_pkModule->ConstantR(iRow,iCol) = fBConstant;
            kDiff = m_pkModule->Position(iRow,iCol) -
                m_pkModule->Position(iRow+1,iCol);
            m_pkModule->LengthR(iRow,iCol) = kDiff.Length();
        }
    }
}
//----------------------------------------------------------------------------
void Cloth::CreateCloth ()
{
    // create quadratic spline using particles as control points
    m_pkSpline = new BSplineRectanglef(m_pkModule->GetRows(),
        m_pkModule->GetCols(),m_pkModule->Positions2D(),2,2,false,false,
        true,true);

    // generate a rectangle surface
    int iUSamples = 16;
    int iVSamples = 32;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bDoubleSided = true;
    Vector2f kTextureMin(0.0f,0.0f), kTextureMax(1.0f,1.0f);
    m_spkCloth = new RectangleSurface(m_pkSpline,iUSamples,iVSamples,
        bWantNormals,bWantColors,bDoubleSided,&kTextureMin,&kTextureMax);

    // attach a texture for the surface
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("purplecloth.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Apply() = Texture::AM_REPLACE;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkCloth->SetRenderState(pkTS);

    m_spkTrnNode->AttachChild(m_spkCloth);
}
//----------------------------------------------------------------------------
void Cloth::CreateScene ()
{
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

    CreateSprings();
    CreateCloth();
}
//----------------------------------------------------------------------------
void Cloth::DoPhysical ()
{
    m_pkModule->Update(GetTimeInSeconds());

    // Update spline surface.  Remember that the spline maintains its own
    // copy of the control points, so this update is necessary.
    int iRows = m_pkModule->GetRows();
    int iCols = m_pkModule->GetCols();
    Vector3f** aakCtrlPoint = m_pkModule->Positions2D();
    for (int iRow = 0; iRow < iRows; iRow++)
    {
        for (int iCol = 0; iCol < iCols; iCol++)
            m_pkSpline->SetControlPoint(iRow,iCol,aakCtrlPoint[iRow][iCol]);
    }

    m_spkCloth->UpdateSurface();
}
//----------------------------------------------------------------------------
