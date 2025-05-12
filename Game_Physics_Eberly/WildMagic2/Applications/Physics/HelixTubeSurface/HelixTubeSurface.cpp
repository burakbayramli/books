// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "HelixTubeSurface.h"
#include "WmlTubeSurface.h"

HelixTubeSurface g_kTheApp;

//----------------------------------------------------------------------------
HelixTubeSurface::HelixTubeSurface ()
    :
    Application("HelixTubeSurface",0,0,640,480,ColorRGB(0.9f,0.9f,0.9f))
{
    m_fDeltaTime = 0.01f;
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
MultipleCurve3f* HelixTubeSurface::CreateCurve ()
{
    // sample points on a looped helix (first and last point must match)
    const float fFourPi = 4.0f*Mathf::PI;
    int iSegments = 32;
    int iSegmentsP1 = iSegments + 1;
    float* afTime = new float[iSegmentsP1];
    Vector3f* akPoint = new Vector3f[iSegmentsP1];
    int i;
    for (i = 0; i <= iSegmentsP1/2; i++)
    {
        afTime[i] = i*fFourPi/iSegmentsP1;
        akPoint[i].X() = Mathf::Cos(afTime[i]);
        akPoint[i].Y() = Mathf::Sin(afTime[i]);
        akPoint[i].Z() = afTime[i];
    }

    for (i = iSegmentsP1/2 + 1; i < iSegments; i++)
    {
        afTime[i] = i*fFourPi/iSegments;
        akPoint[i].X() = 2.0f - Mathf::Cos(afTime[i]);
        akPoint[i].Y() = Mathf::Sin(afTime[i]);
        akPoint[i].Z() = fFourPi - afTime[i];
    }

    afTime[iSegments] = fFourPi;
    akPoint[iSegments] = akPoint[0];

    // save min and max times
    m_fMinCurveTime = 0.0f;
    m_fMaxCurveTime = fFourPi;
    m_fCurvePeriod = m_fMaxCurveTime - m_fMinCurveTime;
    m_fCurveTime = m_fMinCurveTime;

    // create a closed cubic curve containing the sample points
    NaturalSpline3f* pkCurve = new NaturalSpline3f(NaturalSpline3f::BT_CLOSED,
        iSegments,afTime,akPoint);

    return pkCurve;
}
//----------------------------------------------------------------------------
float HelixTubeSurface::Radial (float)
{
    return 0.0625f;
}
//----------------------------------------------------------------------------
bool HelixTubeSurface::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    m_fN = 0.01f;
    m_fF = 10.0f;
    m_fL = -0.0055f;
    m_fR = 0.0055f;
    m_fT = 0.004125f;
    m_fB = -0.004125f;
    ms_spkCamera->SetFrustum(m_fN,m_fF,m_fL,m_fR,m_fT,m_fB);

    m_spkScene = new Node;

    m_spkTexture = new Texture;
    Image* pkImage = Image::Load("grating.mif");
    if ( !pkImage )
        return true;

    m_spkTexture->SetImage(pkImage);
    m_spkTexture->Filter() = Texture::FM_LINEAR;
    m_spkTexture->Mipmap() = Texture::MM_LINEAR;
    m_spkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    m_spkTextureState = new TextureState;
    m_spkTextureState->Set(0,m_spkTexture);
    m_spkScene->SetRenderState(m_spkTextureState);

    m_spkWireframeState = new WireframeState;
    m_spkWireframeState->Enabled() = false;
    m_spkScene->SetRenderState(m_spkWireframeState);

    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    m_pkCurve = CreateCurve();
    bool bClosed = true;
    Vector3f kUpVector = Vector3f::UNIT_Z;
    int iMedialSamples = 256;
    int iSliceSamples = 32;
    bool bWantNormals = true;
    bool bWantColors = false;
    bool bSampleByArcLength = false;
    bool bInsideView = true;
    Vector2f kTextureMin(0.0f,0.0f), kTextureMax(1.0f,32.0f);

    TubeSurface* pkTube = new TubeSurface(m_pkCurve,Radial,bClosed,kUpVector,
        iMedialSamples,iSliceSamples,bWantNormals,bWantColors,
        bSampleByArcLength,bInsideView,&kTextureMin,&kTextureMax);

    m_spkScene->AttachChild(pkTube);

    MoveCamera();

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void HelixTubeSurface::OnTerminate ()
{
    m_spkTexture = NULL;
    m_spkTextureState = NULL;
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
bool HelixTubeSurface::MoveCamera ()
{
    Vector3f kP = m_pkCurve->GetPosition(m_fCurveTime);
    Vector3f kT = m_pkCurve->GetTangent(m_fCurveTime);
    Vector3f kB = kT.UnitCross(Vector3f::UNIT_Z);
    Vector3f kN = kB.UnitCross(kT);

    ms_spkCamera->SetLocation(kP);
    ms_spkCamera->SetAxes(-kB,kN,kT);
    ms_spkCamera->Update();

    return true;
}
//----------------------------------------------------------------------------
void HelixTubeSurface::OnIdle ()
{
    MeasureTime();

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
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of grating.mif failed.  "
                "Make sure this file is in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void HelixTubeSurface::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'w':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        return;
    case '+':
    case '=':
        m_fDeltaTime *= 2.0f;
        return;
    case '-':
    case '_':
        m_fDeltaTime *= 0.5f;
        return;

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
    }
}
//----------------------------------------------------------------------------
void HelixTubeSurface::OnSpecialKeyDown (int iKey, int, int)
{
    if ( iKey == KEY_UP_ARROW )
    {
        m_fCurveTime += m_fDeltaTime;
        if ( m_fCurveTime > m_fMaxCurveTime )
            m_fCurveTime -= m_fCurvePeriod;
        MoveCamera();
        return;
    }

    if ( iKey == KEY_DOWN_ARROW )
    {
        m_fCurveTime -= m_fDeltaTime;
        if ( m_fCurveTime < m_fMinCurveTime )
            m_fCurveTime += m_fCurvePeriod;
        MoveCamera();
    }
}
//----------------------------------------------------------------------------
