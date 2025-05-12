// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Polylines.h"

Polylines g_kTheApp;

//----------------------------------------------------------------------------
Polylines::Polylines ()
    :
    Application("Polylines",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
bool Polylines::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(4.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // set up scene
    m_spkScene = new Node(1);

    int iVertexQuantity = 128;
    Vector3f* akVertex = new Vector3f[iVertexQuantity];
    ColorRGB* akColor = new ColorRGB[iVertexQuantity];
    for (int i = 0; i < iVertexQuantity; i++)
    {
        akVertex[i].X() = Mathf::SymmetricRandom();
        akVertex[i].Y() = Mathf::SymmetricRandom();
        akVertex[i].Z() = Mathf::SymmetricRandom();
        akColor[i].r = Mathf::UnitRandom();
        akColor[i].g = Mathf::UnitRandom();
        akColor[i].b = Mathf::UnitRandom();
    }

    bool bClosed = true;
    bool bContiguous = true;
    Polyline* pkPoints = new Polyline(iVertexQuantity,akVertex,NULL,
        akColor,NULL,bClosed);
    pkPoints->Contiguous() = bContiguous;

    m_spkScene->AttachChild(pkPoints);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.001f;

    return true;
}
//----------------------------------------------------------------------------
void Polylines::OnTerminate ()
{
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void Polylines::OnIdle ()
{
    MeasureTime();

    MoveCamera();
    if ( MoveObject() )
        m_spkScene->UpdateGS(0.0f);

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
void Polylines::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
