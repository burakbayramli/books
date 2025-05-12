// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "PolygonOffset.h"

PolygonOffset g_kTheApp;

//----------------------------------------------------------------------------
PolygonOffset::PolygonOffset ()
    :
    Application("PolygonOffset",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
void PolygonOffset::CreateScene ()
{
    m_spkScene = new Node;

    Vector3f* akVertex1 = new Vector3f[4];
    ColorRGB* akColor1 = new ColorRGB[4];
    int* aiConnect1 = new int[6];
    Vector3f* akVertex2 = new Vector3f[4];
    ColorRGB* akColor2 = new ColorRGB[4];
    int* aiConnect2 = new int[6];
    Vector3f* akVertex3 = new Vector3f[4];
    ColorRGB* akColor3 = new ColorRGB[4];
    int* aiConnect3 = new int[6];
    Vector3f* akVertex4 = new Vector3f[4];
    ColorRGB* akColor4 = new ColorRGB[4];
    int* aiConnect4 = new int[6];

    akVertex1[0] = Vector3f(-1.0f,0.0f,-1.0f);
    akVertex1[1] = Vector3f(-1.0f,0.0f,+1.0f);
    akVertex1[2] = Vector3f(+1.0f,0.0f,+1.0f);
    akVertex1[3] = Vector3f(+1.0f,0.0f,-1.0f);

    akColor1[0] = ColorRGB(1.0f,0.0f,0.0f);
    akColor1[1] = ColorRGB(1.0f,0.0f,0.0f);
    akColor1[2] = ColorRGB(1.0f,0.0f,0.0f);
    akColor1[3] = ColorRGB(1.0f,0.0f,0.0f);
    
    akColor2[0] = ColorRGB(0.0f,1.0f,0.0f);
    akColor2[1] = ColorRGB(0.0f,1.0f,0.0f);
    akColor2[2] = ColorRGB(0.0f,1.0f,0.0f);
    akColor2[3] = ColorRGB(0.0f,1.0f,0.0f);

    aiConnect1[0] = 0;
    aiConnect1[1] = 1;
    aiConnect1[2] = 3;
    aiConnect1[3] = 3;
    aiConnect1[4] = 1;
    aiConnect1[5] = 2;

    memcpy(akVertex2,akVertex1,4*sizeof(Vector3f));
    memcpy(akVertex3,akVertex1,4*sizeof(Vector3f));
    memcpy(akVertex4,akVertex1,4*sizeof(Vector3f));
    memcpy(akColor3,akColor1,4*sizeof(ColorRGB));
    memcpy(akColor4,akColor2,4*sizeof(ColorRGB));
    memcpy(aiConnect2,aiConnect1,6*sizeof(int));
    memcpy(aiConnect3,aiConnect1,6*sizeof(int));
    memcpy(aiConnect4,aiConnect1,6*sizeof(int));

	TriMesh* pkMesh;

    pkMesh = new TriMesh(4,akVertex1,NULL,akColor1,NULL,2,aiConnect1);
    pkMesh->Translate() = Vector3f(+2.0f,-4.0f,0.0f);
    m_spkScene->AttachChild(pkMesh);

    pkMesh = new TriMesh(4,akVertex2,NULL,akColor2,NULL,2,aiConnect2);
    pkMesh->Translate() = Vector3f(+2.0f,-4.0f,0.0f);
    pkMesh->Scale() = 0.5f;
    m_spkScene->AttachChild(pkMesh);

    pkMesh = new TriMesh(4,akVertex3,NULL,akColor3,NULL,2,aiConnect3);
    pkMesh->Translate() = Vector3f(-2.0f,-4.0f,0.0f);
    m_spkScene->AttachChild(pkMesh);

    pkMesh = new TriMesh(4,akVertex4,NULL,akColor4,NULL,2,aiConnect4);
    pkMesh->Translate() = Vector3f(-2.0f,-4.0f,0.0f);
    pkMesh->Scale() = 0.5f;
    m_spkScene->AttachChild(pkMesh);

    // Set up the polygon offset.  The default values bring the polygon
    // closer to the eye.  Attach it to the last TriMesh created above.
    PolygonOffsetState* pkPolygonOffsetState = new PolygonOffsetState;
    pkPolygonOffsetState->FillEnabled() = true;
    pkMesh->SetRenderState(pkPolygonOffsetState);
}
//----------------------------------------------------------------------------
bool PolygonOffset::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,-16.0f);
    Vector3f kCLeft(1.0f,0.0f,0.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(0.0f,0.0f,1.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    CreateScene();

    m_spkZBuffer = new ZBufferState;
    m_spkZBuffer->Enabled() = true;
    m_spkZBuffer->Writeable() = true;
    m_spkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBuffer);

    m_spkWireframe = new WireframeState;
    m_spkWireframe->Enabled() = false;
    m_spkScene->SetRenderState(m_spkWireframe);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;

    return true;
}
//----------------------------------------------------------------------------
void PolygonOffset::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkZBuffer = NULL;
    m_spkWireframe = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void PolygonOffset::OnIdle ()
{
    MoveCamera();

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
void PolygonOffset::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    if ( ucKey == 'w' || ucKey == 'W' )
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
}
//----------------------------------------------------------------------------
