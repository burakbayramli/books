// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "ClodFace.h"

ClodFace g_kTheApp;

//----------------------------------------------------------------------------
ClodFace::ClodFace ()
    :
    Application("ClodFace",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
    m_bInitialized = false;
}
//----------------------------------------------------------------------------
bool ClodFace::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    ms_spkCamera->SetFrustum(1.0f,10000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(0.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,0.0f,-1.0f);
    Vector3f kCUp(0.0f,1.0f,0.0f);
    Vector3f kCDir(1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

#if 1
    // load already clodified face
    Stream kStream;
    bool bLoaded = kStream.Load("ClodFace.mgc");
    if ( !bLoaded )
        return true;

    m_spkScene = (Node*) kStream.GetObjectAt(0);
    m_spkClod = WmlSmartPointerCast(ClodMesh,m_spkScene->GetChild(0));
#else
    // load trimesh face and clodify it
    Stream kStream;
    bool bLoaded = kStream.Load("Face.mgc");
    if ( !bLoaded )
        return true;

    m_spkScene = (Node*) kStream.GetObjectAt(0);
    TriMesh* pkMesh = MgcSmartPointerCast(TriMesh,m_spkScene->GetChild(0));
    int iVQuantity = pkMesh->GetVertexQuantity();
    int iTQuantity = pkMesh->GetTriangleQuantity();
    Vector3f* akVertex = new Vector3f[iVQuantity];
    Vector3f* akNormal = new Vector3f[iVQuantity];
    int* aiConnect = new int[3*iTQuantity];
    memcpy(akVertex,pkMesh->Vertices(),iVQuantity*sizeof(Vector3f));
    memcpy(akNormal,pkMesh->Normals(),iVQuantity*sizeof(Vector3f));
    memcpy(aiConnect,pkMesh->Connectivity(),3*iTQuantity*sizeof(int));

    m_spkClod = new ClodMesh(iVQuantity,akVertex,akNormal,NULL,NULL,
        iTQuantity,aiConnect);
    m_spkClod->Translate() = pkMesh->Translate();
    m_spkClod->Rotate() = pkMesh->Rotate();
    m_spkClod->SetRenderState(pkMesh->GetRenderState(
        RenderState::RS_LIGHT));
    m_spkClod->SetRenderState(pkMesh->GetRenderState(
        RenderState::RS_MATERIAL));

    m_spkScene->SetChild(0,m_spkClod);
#endif

    RenderState* pkState =
        m_spkScene->GetRenderState(RenderState::RS_WIREFRAME);
    m_spkWireframeState = WmlSmartPointerCast(WireframeState,pkState);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    m_spkMotionObject = m_spkScene;
    m_fTrnSpeed = 5.0f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();

    m_bInitialized = true;
    return true;
}
//----------------------------------------------------------------------------
void ClodFace::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkClod = NULL;
    m_spkWireframeState = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void ClodFace::OnIdle ()
{
    MeasureTime();

    MoveCamera();

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        if ( m_bInitialized )
        {
            ms_spkRenderer->Draw(m_spkScene);
            DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
            DrawTriangleQuantity(128,GetHeight()-8,ColorRGB::WHITE);
        }
        else
        {
            ms_spkRenderer->Draw(8,16,ColorRGB::WHITE,
                "Load of ClodFace.mgc failed.  "
                "Make sure this file is in the same directory as the "
                "executable.");
        }

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void ClodFace::DrawTriangleQuantity (int iX, int iY, const ColorRGB& rkColor)
{
    if ( m_spkClod && ms_spkRenderer )
    {
        char acMessage[32];
        sprintf(acMessage,"triangles: %d",m_spkClod->GetTriangleQuantity());
        ms_spkRenderer->Draw(iX,iY,rkColor,acMessage);
    }
}
//----------------------------------------------------------------------------
void ClodFace::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case '0':  // reset frame rate measurements
        ResetTime();
        return;
    case '-':  // decrease triangle quantity
        if ( m_spkClod->TargetRecord() < m_spkClod->GetRecordQuantity()-1 )
            m_spkClod->TargetRecord()++;
        m_spkClod->UpdateGS(0.0f);
        return;
    case '+':  // increase triangle quantity
        if ( m_spkClod->TargetRecord() > 0 )
            m_spkClod->TargetRecord()--;
        m_spkClod->UpdateGS(0.0f);
        return;
    case 'w':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        return;
    }
}
//----------------------------------------------------------------------------
void ClodFace::MoveForward ()
{
    Application::MoveForward();

    Vector3f kDiff = m_spkScene->WorldBound().Center() -
        ms_spkCamera->GetLocation();
    float fDepth = kDiff.Dot(ms_spkCamera->GetDirection());
    if ( fDepth <= ms_spkCamera->GetFrustumNear() )
    {
        m_spkClod->TargetRecord() = 0;
    }
    else if ( fDepth >= ms_spkCamera->GetFrustumFar() )
    {
        m_spkClod->TargetRecord() = m_spkClod->GetRecordQuantity() - 1;
    }
    else
    {
        // distance along camera direction controls triangle quantity
        float fN = ms_spkCamera->GetFrustumNear();
        float fF = ms_spkCamera->GetFrustumFar();
        float fRatio = (fDepth - fN)/(fF - fN);

        // allow nonlinear drop-off
        fRatio = Mathf::Pow(fRatio,0.5f);

        int iMaxIndex = m_spkClod->GetRecordQuantity() - 1;
        int iIndex = (int)(iMaxIndex*fRatio);
        m_spkClod->TargetRecord() = iIndex;
    }
}
//----------------------------------------------------------------------------
void ClodFace::MoveBackward ()
{
    Application::MoveBackward();

    Vector3f kDiff = m_spkScene->WorldBound().Center() -
        ms_spkCamera->GetLocation();
    float fDepth = kDiff.Dot(ms_spkCamera->GetDirection());
    if ( fDepth <= ms_spkCamera->GetFrustumNear() )
    {
        m_spkClod->TargetRecord() = 0;
    }
    else if ( fDepth >= ms_spkCamera->GetFrustumFar() )
    {
        m_spkClod->TargetRecord() = m_spkClod->GetRecordQuantity() - 1;
    }
    else
    {
        // distance along camera direction controls triangle quantity
        float fN = ms_spkCamera->GetFrustumNear();
        float fF = ms_spkCamera->GetFrustumFar();
        float fRatio = (fDepth - fN)/(fF - fN);

        // allow nonlinear drop-off
        fRatio = Mathf::Pow(fRatio,0.5f);

        int iMaxIndex = m_spkClod->GetRecordQuantity() - 1;
        int iIndex = (int)(iMaxIndex*fRatio);
        m_spkClod->TargetRecord() = iIndex;
    }
}
//----------------------------------------------------------------------------
