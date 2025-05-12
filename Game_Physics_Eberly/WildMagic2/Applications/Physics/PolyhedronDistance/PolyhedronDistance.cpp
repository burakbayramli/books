// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "PolyhedronDistance.h"

PolyhedronDistance g_kTheApp;

//----------------------------------------------------------------------------
PolyhedronDistance::PolyhedronDistance ()
    :
    Application("PolyhedronDistance",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
bool PolyhedronDistance::OnInitialize()
{
    if ( !Application::OnInitialize() )
        return false;

    // ** layout of scene graph **
    // scene
    //     tetra[4]
    //     plane
    //     line[2]

    // create objects
    m_spkScene = new Node(7);
    TriMesh* pkPlane = CreatePlane();
    int i;
    for (i = 0; i < 2; i++)
    {
        // build the display tetrahedra
        float fSize = 0.3f+0.2f*(i+1);
        if ( i == 0 )
            m_fEdgeLength = fSize;
        m_apkTetra[i] = CreateTetra(fSize,0);
        m_aspkLine[i] = CreateLine();

        // build the point tetrahedr;
        m_fSmall = 0.02f;
        m_apkTetra[i+2] = CreateTetra(m_fSmall,1);
    }

    // convenience for line manipulations
    for(i = 0; i < 2; i++)
        m_apkVertex[i] = m_aspkLine[i]->Vertices();

    // tetrahedra faces
    m_akFace = new Tuple<3>[4];
    m_akFace[0][0] = 1, m_akFace[0][1] = 2, m_akFace[0][2] = 0;
    m_akFace[1][0] = 0, m_akFace[1][1] = 3, m_akFace[1][2] = 2;
    m_akFace[2][0] = 0, m_akFace[2][1] = 1, m_akFace[2][2] = 3;
    m_akFace[3][0] = 1, m_akFace[3][1] = 2, m_akFace[3][2] = 3;

    InitialState();

    // set parent-child links
    m_spkScene->AttachChild(pkPlane);
    for (i = 0; i < 2; i++)
    {
        m_spkScene->AttachChild(m_apkTetra[i]);
        m_spkScene->AttachChild(m_aspkLine[i]);
        m_spkScene->AttachChild(m_apkTetra[i+2]);
    }

    // wireframe
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);

    // depth buffer
    m_spkZBuffer = new ZBufferState;
    m_spkZBuffer->Enabled() = true;
    m_spkZBuffer->Writeable() = true;
    m_spkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBuffer);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();
    PolyhedronDistance::UpdateLine();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;

    return true;
}
//----------------------------------------------------------------------------
void PolyhedronDistance::OnTerminate()
{
    m_aspkLine[0] = NULL;
    m_aspkLine[1] = NULL;
    m_spkWireframe = NULL;
    m_spkZBuffer = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void PolyhedronDistance::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    if ( Transform(ucKey) )
        return;

    switch ( ucKey )
    {
    case '0':  // restart
        InitialState();
        m_spkScene->UpdateGS(0.0f);
        UpdateLine();
        return;
    case 'w':  // toggle wireframe
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        return;
    }
}
//----------------------------------------------------------------------------
void PolyhedronDistance::OnIdle ()
{
    MoveCamera();

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);

        char acMsg[256];
        sprintf(acMsg,"separation = %3.2f",
            m_fSeparation/(Mathf::Sqrt(2.0)*m_fEdgeLength));
        ms_spkRenderer->Draw(8,GetHeight()-8,ColorRGB::WHITE,acMsg);
        sprintf(acMsg," small tetrahedron sides." );
        ms_spkRenderer->Draw(140,GetHeight()-8,ColorRGB::WHITE,acMsg);
        
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
TriMesh* PolyhedronDistance::CreateTetra (float fSize, int iColor)
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = -(fSize/3.0f)*Vector3f(1.0f,1.0f,1.0f);
    akVertex[1] = Vector3f(+fSize,0.0f,0.0f);
    akVertex[2] = Vector3f(0.0f,+fSize,0.0f);
    akVertex[3] = Vector3f(0.0f,0.0f,+fSize);

    ColorRGB* akColor = new ColorRGB[4];
    if ( iColor == 0 )
    {
        // colorful colors for the tetrahedra under study
        akColor[0] = ColorRGB(0.0f,0.0f,1.0f);
        akColor[1] = ColorRGB(0.0f,1.0f,0.0f);
        akColor[2] = ColorRGB(1.0f,0.0f,0.0f);
        akColor[3] = ColorRGB(1.0f,1.0f,1.0f);
    }
    else
    {
        // black tetrahedra for the small ones used as points.
        akColor[0] = ColorRGB(0.0f,0.0f,0.0f);
        akColor[1] = ColorRGB(0.0f,0.0f,0.0f);
        akColor[2] = ColorRGB(0.0f,0.0f,0.0f);
        akColor[3] = ColorRGB(0.0f,0.0f,0.0f);
    }
    int* aiConnect = new int[12];
    aiConnect[ 0] = 0; aiConnect[ 1] = 2; aiConnect[ 2] = 1;
    aiConnect[ 3] = 0; aiConnect[ 4] = 3; aiConnect[ 5] = 2;
    aiConnect[ 6] = 0; aiConnect[ 7] = 1; aiConnect[ 8] = 3;
    aiConnect[ 9] = 1; aiConnect[10] = 2; aiConnect[11] = 3;

    return new TriMesh(4,akVertex,NULL,akColor,NULL,4,aiConnect);
}
//----------------------------------------------------------------------------
Polyline* PolyhedronDistance::CreateLine ()
{
    Vector3f* akVertex = new Vector3f[2];
    akVertex[0] = Vector3f::ZERO;
    akVertex[1] = Vector3f::UNIT_X;
    ColorRGB* akColor = new ColorRGB[2];
    akColor[0] = ColorRGB::WHITE;
    akColor[1] = ColorRGB::WHITE;

    return new Polyline(2,akVertex,NULL,akColor,NULL,false);
}
//----------------------------------------------------------------------------
void PolyhedronDistance::UpdateLine ()
{
    // two lines make the line easier to see
    Vector3f aakU[2][4];

    // Offset the polyhedra so far into the first octant that we are unlikely
    // to translate them out of that octant during a run.
    m_fOffset = 20.0f;
    Vector3f kTOffset = m_fOffset*Vector3f(1.0f,1.0f,1.0f);
    
    int i;
    for (i = 0; i < 2; i++)
    {
        m_akTV[i] = m_apkTetra[i]->WorldTranslate();
        m_akTM[i] = m_apkTetra[i]->WorldRotate();
        for (int j = 0; j < 4; j++)
        {
            aakU[i][j] = m_akTV[i] + kTOffset +
                m_akTM[i]*m_apkTetra[i]->Vertex(j);
        }
    }

    int iStatusCode;

    LCPPolyDist3(4,aakU[0],4,m_akFace,4,aakU[1],4,m_akFace,iStatusCode,
        m_fSeparation,m_apkVertex[0]);

    if ( iStatusCode != LCPPolyDist3::SC_FOUND_SOLUTION &&
         iStatusCode != LCPPolyDist3::SC_TEST_POINTS_TEST_FAILED &&
         iStatusCode != LCPPolyDist3::SC_FOUND_TRIVIAL_SOLUTION ||
         m_fSeparation < 0.0f )
    {
        // Don't draw the line joining nearest points if returns from
        // LCPPolyDist are not appropriate
        for (i = 0; i < 2; i++)
            m_apkVertex[0][i] = -kTOffset;
    }

    // correct for the offset and set up end "points" for the line
    for (i = 0; i < 2; i++)
    {
        m_apkVertex[0][i] = m_apkVertex[0][i]-kTOffset;

        // the adjustment with m_fSmall "centers" the end point tetra
        // on the solution points
        m_apkTetra[i+2]->Translate() = m_apkVertex[0][i]
            -(m_fSmall/3.0f)*Vector3f(1.0f,1.0f,1.0f);
    }
    const float fEpsilon = 0.002f; // double-up the line for better visibility
    for (i = 0; i < 2; i++)
    {
        m_apkVertex[1][i] = m_apkVertex[0][i] +
            fEpsilon*Vector3f(1.0f,1.0f,1.0f);
    }

    for (i = 0; i < 2; i++)
    {
        m_aspkLine[i]->UpdateModelBound();
        m_aspkLine[i]->UpdateGS(0.0f);
    }
    m_spkScene->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
TriMesh* PolyhedronDistance::CreatePlane ()
{
    int iVertexQuantity = 4;
    int iTriangleQuantity = 2;

    Vector3f* akVertex = new Vector3f[iVertexQuantity];
    float fSize = 16.0f;
    akVertex[0] = Vector3f(-fSize,-fSize,-0.1f);
    akVertex[1] = Vector3f(+fSize,-fSize,-0.1f);
    akVertex[2] = Vector3f(+fSize,+fSize,-0.1f);
    akVertex[3] = Vector3f(-fSize,+fSize,-0.1f);

    ColorRGB* akColor = new ColorRGB[iVertexQuantity];
    akColor[0] = ColorRGB(0.0f,0.50f,0.00f);
    akColor[1] = ColorRGB(0.0f,0.25f,0.00f);
    akColor[2] = ColorRGB(0.0f,0.75f,0.00f);
    akColor[3] = ColorRGB(0.0f,1.0f,0.00f);

    int* aiConnect = new int[3*iTriangleQuantity];
    aiConnect[0] = 0; aiConnect[1] = 1; aiConnect[2] = 2;
    aiConnect[3] = 0; aiConnect[4] = 2; aiConnect[5] = 3;

    return new TriMesh(iVertexQuantity,akVertex,NULL,akColor,NULL,
        iTriangleQuantity,aiConnect);
}
//----------------------------------------------------------------------------
void PolyhedronDistance::InitialState ()
{
    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCamLoc(0.0f,-2.0f,0.5f);
    Matrix3f kCamOrient(-1.0f,0.0f,0.0f,0.0f,0.0f,1.0f,0.0f,1.0f,0.0f);
    ms_spkCamera->SetFrame(kCamLoc,kCamOrient);

    // transform objects
    m_apkTetra[0]->Rotate().FromAxisAngle(Vector3f::UNIT_Z,1.1f);
    m_apkTetra[0]->Translate() = Vector3f(-0.25f,0.1f,0.3f);
    m_apkTetra[1]->Rotate().FromAxisAngle(Vector3f::UNIT_Z,0.3f);
    m_apkTetra[1]->Translate() = Vector3f(0.25f,0.4f,0.5f);
}
//----------------------------------------------------------------------------
bool PolyhedronDistance::Transform (unsigned char ucKey)
{
    Matrix3f kRot, kIncr;
    float fTrnSpeed = 0.1f;
    float fRotSpeed = 0.1f;

    switch ( ucKey )
    {
        case 'x':
            m_apkTetra[0]->Translate().X() -= fTrnSpeed;
            break;
        case 'X':
            m_apkTetra[0]->Translate().X() += fTrnSpeed;
            break;
        case 'y':
            m_apkTetra[0]->Translate().Y() -= fTrnSpeed;
            break;
        case 'Y':
            m_apkTetra[0]->Translate().Y() += fTrnSpeed;
            break;
        case 'z':
            m_apkTetra[0]->Translate().Z() -= fTrnSpeed;
            break;
        case 'Z':
            m_apkTetra[0]->Translate().Z() += fTrnSpeed;
            break;
        case 's':
            m_apkTetra[1]->Translate().X() -= fTrnSpeed;
            break;
        case 'S':
            m_apkTetra[1]->Translate().X() += fTrnSpeed;
            break;
        case 't':
            m_apkTetra[1]->Translate().Y() -= fTrnSpeed;
            break;
        case 'T':
            m_apkTetra[1]->Translate().Y() += fTrnSpeed;
            break;
        case 'u':
            m_apkTetra[1]->Translate().Z() -= fTrnSpeed;
            break;
        case 'U':
            m_apkTetra[1]->Translate().Z() += fTrnSpeed;
            break;
        case 'a':
            kRot = m_apkTetra[0]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Y,fRotSpeed);
            m_apkTetra[0]->Rotate() = kIncr*kRot;
            break;
        case 'A':
            kRot = m_apkTetra[0]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Y,-fRotSpeed);
            m_apkTetra[0]->Rotate() = kIncr*kRot;
            break;
        case 'b':
            kRot = m_apkTetra[1]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Z,fRotSpeed);
            m_apkTetra[1]->Rotate() = kIncr*kRot;
            break;
        case 'B':
            kRot = m_apkTetra[1]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_X,-fRotSpeed);
            m_apkTetra[1]->Rotate() = kIncr*kRot;
            break;
        case 'c':
            kRot = m_apkTetra[0]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_X,1.3f*fRotSpeed);
            m_apkTetra[0]->Rotate() = kIncr*kRot;
            kRot = m_apkTetra[1]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Z,-fRotSpeed);
            m_apkTetra[1]->Rotate() = kIncr*kRot;
            break;
        case 'C':
            kRot = m_apkTetra[0]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_X,-1.3f*fRotSpeed);
            m_apkTetra[0]->Rotate() = kIncr*kRot;
            kRot = m_apkTetra[1]->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Z,fRotSpeed);
            m_apkTetra[1]->Rotate() = kIncr*kRot;
            break;
        default:
            return false;
    }

    // Prevent solution point coordinates from being < 0.
    // The polyhedron distance calculator expects solution points
    // to be in the first octant. Vertices are offset by
    // m_fOffset*Vector3f(1,1,1) in UpdateLine() before the call to the 
    // distance routine.
    // Here we make sure that no translation of a polyhedron takes it so
    // far into one of the other 7 octants that the offset will not be
    // sufficient to guarantee that the solution points lie in the first 
    // octant.
    float fThreshold = -m_fOffset+fTrnSpeed;
    for (int j = 0; j < 2; j++)
    {
        Vector3f kTV = m_apkTetra[j]->WorldTranslate();
        Matrix3f kTM = m_apkTetra[j]->WorldRotate();
        for (int i = 0; i < 4; i++)
        {
            Vector3f kTemp = m_apkTetra[j]->Vertex(i);
            kTemp = kTV+kTM*kTemp;
            if ( kTemp[0] < fThreshold )
                m_apkTetra[j]->Translate().X() += fTrnSpeed;
            if ( kTemp[1] < fThreshold )
                m_apkTetra[j]->Translate().Y() += fTrnSpeed;
            if ( kTemp[2] < fThreshold )
                m_apkTetra[j]->Translate().Z() += fTrnSpeed;
        }
    }

    m_spkScene->UpdateGS(0.0f);
    UpdateLine();
    return true;
}
//----------------------------------------------------------------------------
