// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "GelatinCube.h"

GelatinCube g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
GelatinCube::GelatinCube ()
    :
    Application("GelatinCube",0,0,640,480,
        ColorRGB(0.713725f,0.807843f,0.929411f))
{
    m_pkSpline = NULL;
    m_pkModule = NULL;
}
//----------------------------------------------------------------------------
GelatinCube::~GelatinCube ()
{
}
//----------------------------------------------------------------------------
bool GelatinCube::OnInitialize ()
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

    // sort the box faces based on current camera parameters
    m_spkBox->SortFaces(ms_spkCamera->GetDirection());

    // camera turret and tumble mode
    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;
    m_bTurretActive = true;
    SetTurretAxes();
    return true;
}
//----------------------------------------------------------------------------
void GelatinCube::OnTerminate ()
{
    delete m_pkModule;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void GelatinCube::OnIdle ()
{
    MeasureTime();

    bool bNeedSort = MoveCamera();
    if ( MoveObject() )
    {
        m_spkScene->UpdateGS(0.0f);
        bNeedSort = true;
    }
    if ( bNeedSort )
        m_spkBox->SortFaces(ms_spkCamera->GetDirection());

#ifndef SINGLE_STEP
    DoPhysical();
#endif

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void GelatinCube::OnKeyDown (unsigned char ucKey, int iX, int iY)
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
void GelatinCube::CreateSprings ()
{
    // The inner 4-by-4-by-4 particles are used as the control points of a
    // B-spline volume.  The outer layer of particles are immovable to
    // prevent the cuboid from collapsing into itself.
    int iSlices = 6;
    int iRows = 6;
    int iCols = 6;

    // Viscous forces applied.  If you set viscosity to zero, the cuboid
    // wiggles indefinitely since there is no dissipation of energy.  If
    // the viscosity is set to a positive value, the oscillations eventually
    // stop.  The length of time to steady state is inversely proportional
    // to the viscosity.
#ifdef _DEBUG
    float fStep = 0.1f;
#else
    float fStep = 0.01f;  // simulation needs to run slower in release mode
#endif
    float fViscosity = 0.01f;
    m_pkModule = new PhysicsModule(iSlices,iRows,iCols,fStep,fViscosity);

    // The initial cuboid is axis-aligned.  The outer shell is immovable.
    // All other masses are constant.
    float fSFactor = 1.0f/(float)(iSlices-1);
    float fRFactor = 1.0f/(float)(iRows-1);
    float fCFactor = 1.0f/(float)(iCols-1);
    int iSlice, iRow, iCol;
    for (iSlice = 0; iSlice < iSlices; iSlice++)
    {
        for (iRow = 0; iRow < iRows; iRow++)
        {
            for (iCol = 0; iCol < iCols; iCol++)
            {
                m_pkModule->Position(iSlice,iRow,iCol) =
                    Vector3f(iCol*fCFactor,iRow*fRFactor,iSlice*fSFactor);

                if ( 1 <= iSlice && iSlice < iSlices-1
                &&   1 <= iRow && iRow < iRows-1
                &&   1 <= iCol && iCol < iCols-1 )
                {
                    m_pkModule->SetMass(iSlice,iRow,iCol,1.0f);
                    m_pkModule->Velocity(iSlice,iRow,iCol) =
                        0.1f*Vector3f(Mathf::SymmetricRandom(),
                        Mathf::SymmetricRandom(),Mathf::SymmetricRandom());
                }
                else
                {
                    m_pkModule->SetMass(iSlice,iRow,iCol,Mathf::MAX_REAL);
                    m_pkModule->Velocity(iSlice,iRow,iCol) = Vector3f::ZERO;
                }
            }
        }
    }

    // springs are at rest in the initial configuration
    const float fConstant = 10.0f;
    Vector3f kDiff;

    for (iSlice = 0; iSlice < iSlices-1; iSlice++)
    {
        for (iRow = 0; iRow < iRows; iRow++)
        {
            for (iCol = 0; iCol < iCols; iCol++)
            {
                m_pkModule->ConstantS(iSlice,iRow,iCol) = fConstant;
                kDiff = m_pkModule->Position(iSlice+1,iRow,iCol) -
                    m_pkModule->Position(iSlice,iRow,iCol);
                m_pkModule->LengthS(iSlice,iRow,iCol) = kDiff.Length();
            }
        }
    }

    for (iSlice = 0; iSlice < iSlices; iSlice++)
    {
        for (iRow = 0; iRow < iRows-1; iRow++)
        {
            for (iCol = 0; iCol < iCols; iCol++)
            {
                m_pkModule->ConstantR(iSlice,iRow,iCol) = fConstant;
                kDiff = m_pkModule->Position(iSlice,iRow+1,iCol) -
                    m_pkModule->Position(iSlice,iRow,iCol);
                m_pkModule->LengthR(iSlice,iRow,iCol) = kDiff.Length();
            }
        }
    }

    for (iSlice = 0; iSlice < iSlices; iSlice++)
    {
        for (iRow = 0; iRow < iRows; iRow++)
        {
            for (iCol = 0; iCol < iCols-1; iCol++)
            {
                m_pkModule->ConstantC(iSlice,iRow,iCol) = fConstant;
                kDiff = m_pkModule->Position(iSlice,iRow,iCol+1) -
                    m_pkModule->Position(iSlice,iRow,iCol);
                m_pkModule->LengthC(iSlice,iRow,iCol) = kDiff.Length();
            }
        }
    }
}
//----------------------------------------------------------------------------
void GelatinCube::CreateBox ()
{
    // Create a quadratic spline using the interior particles as control
    // points.
    int iSlices = m_pkModule->GetSlices()-2;
    int iRows = m_pkModule->GetRows()-2;
    int iCols = m_pkModule->GetCols()-2;
    m_pkSpline = new BSplineVolumef(iCols,iRows,iSlices,2,2,2);

    for (int iSlice = 0; iSlice < iSlices; iSlice++)
    {
        for (int iRow = 0; iRow < iRows; iRow++)
        {
            for (int iCol = 0; iCol < iCols; iCol++)
            {
                m_pkSpline->ControlPoint(iCol,iRow,iSlice) =
                    m_pkModule->Position(iSlice+1,iRow+1,iCol+1);
            }
        }
    }

    // generate a rectangle surface
    int iUSamples = 8;
    int iVSamples = 8;
    int iWSamples = 8;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bWantTextures = true;
    m_spkBox = new BoxSurface(m_pkSpline,iUSamples,iVSamples,iWSamples,
        bWantNormals,bWantColors,bWantTextures);

    // texture for the water objects
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("WaterA.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkBox->SetRenderState(pkTS);

    // the texture has an alpha channel of 1/2
    AlphaState* pkAS = new AlphaState;
    pkAS->BlendEnabled() = true;
    m_spkBox->SetRenderState(pkAS);

    m_spkBox->EnableSorting();

    m_spkTrnNode->AttachChild(m_spkBox);
}
//----------------------------------------------------------------------------
void GelatinCube::CreateScene ()
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
    CreateBox();
}
//----------------------------------------------------------------------------
void GelatinCube::DoPhysical ()
{
    m_pkModule->Update(GetTimeInSeconds());

    // Update spline surface.  Remember that the spline maintains its own
    // copy of the control points, so this update is necessary.
    int iSlices = m_pkModule->GetSlices()-2;
    int iRows = m_pkModule->GetRows()-2;
    int iCols = m_pkModule->GetCols()-2;

    for (int iSlice = 0; iSlice < iSlices; iSlice++)
    {
        for (int iRow = 0; iRow < iRows; iRow++)
        {
            for (int iCol = 0; iCol < iCols; iCol++)
            {
                m_pkSpline->ControlPoint(iCol,iRow,iSlice) =
                    m_pkModule->Position(iSlice+1,iRow+1,iCol+1);
            }
        }
    }

    m_spkBox->UpdateSurface();
}
//----------------------------------------------------------------------------
