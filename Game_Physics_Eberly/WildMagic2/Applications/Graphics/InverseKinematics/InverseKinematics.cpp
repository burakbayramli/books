// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "InverseKinematics.h"

InverseKinematics g_kTheApp;

//----------------------------------------------------------------------------
InverseKinematics::InverseKinematics ()
    :
    Application("InverseKinematics",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
TriMesh* InverseKinematics::CreateCube ()
{
    int iVertexQuantity = 8;
    int iTriangleQuantity = 12;

    Vector3f* akVertex = new Vector3f[iVertexQuantity];
    float fSize = 0.1f;
    akVertex[0] = Vector3f(-fSize,-fSize,-fSize);
    akVertex[1] = Vector3f(+fSize,-fSize,-fSize);
    akVertex[2] = Vector3f(+fSize,+fSize,-fSize);
    akVertex[3] = Vector3f(-fSize,+fSize,-fSize);
    akVertex[4] = Vector3f(-fSize,-fSize,+fSize);
    akVertex[5] = Vector3f(+fSize,-fSize,+fSize);
    akVertex[6] = Vector3f(+fSize,+fSize,+fSize);
    akVertex[7] = Vector3f(-fSize,+fSize,+fSize);

    ColorRGB* akColor = new ColorRGB[iVertexQuantity];
    akColor[0] = ColorRGB(0.0f,0.0f,1.0f);
    akColor[1] = ColorRGB(0.0f,1.0f,0.0f);
    akColor[2] = ColorRGB(1.0f,0.0f,0.0f);
    akColor[3] = ColorRGB(0.0f,0.0f,0.0f);
    akColor[4] = ColorRGB(0.0f,0.0f,1.0f);
    akColor[5] = ColorRGB(1.0f,0.0f,1.0f);
    akColor[6] = ColorRGB(1.0f,1.0f,0.0f);
    akColor[7] = ColorRGB(1.0f,1.0f,1.0f);

    int* aiConnect = new int[3*iTriangleQuantity];
    aiConnect[ 0] = 0; aiConnect[ 1] = 2; aiConnect[ 2] = 1;
    aiConnect[ 3] = 0; aiConnect[ 4] = 3; aiConnect[ 5] = 2;
    aiConnect[ 6] = 4; aiConnect[ 7] = 5; aiConnect[ 8] = 6;
    aiConnect[ 9] = 4; aiConnect[10] = 6; aiConnect[11] = 7;
    aiConnect[12] = 1; aiConnect[13] = 6; aiConnect[14] = 5;
    aiConnect[15] = 1; aiConnect[16] = 2; aiConnect[17] = 6;
    aiConnect[18] = 0; aiConnect[19] = 7; aiConnect[20] = 3;
    aiConnect[21] = 0; aiConnect[22] = 4; aiConnect[23] = 7;
    aiConnect[24] = 0; aiConnect[25] = 1; aiConnect[26] = 5;
    aiConnect[27] = 0; aiConnect[28] = 5; aiConnect[29] = 4;
    aiConnect[30] = 3; aiConnect[31] = 6; aiConnect[32] = 2;
    aiConnect[33] = 3; aiConnect[34] = 7; aiConnect[35] = 6;

    TriMesh* pkCube = new TriMesh(iVertexQuantity,akVertex,NULL,akColor,
        NULL,iTriangleQuantity,aiConnect);

    return pkCube;
}
//----------------------------------------------------------------------------
Polyline* InverseKinematics::CreateLine ()
{
    Vector3f* akVertex = new Vector3f[2];
    akVertex[0] = Vector3f::ZERO;
    akVertex[1] = Vector3f::UNIT_X;
    ColorRGB* akColor = new ColorRGB[2];
    akColor[0] = ColorRGB::WHITE;
    akColor[1] = ColorRGB::WHITE;

    Polyline* pkLine = new Polyline(2,akVertex,NULL,akColor,NULL,false);

    return pkLine;
}
//----------------------------------------------------------------------------
TriMesh* InverseKinematics::CreatePlane ()
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
    akColor[0] = ColorRGB(0.0f,0.0f,0.25f);
    akColor[1] = ColorRGB(0.0f,0.0f,0.50f);
    akColor[2] = ColorRGB(0.0f,0.0f,0.75f);
    akColor[3] = ColorRGB(0.0f,0.0f,1.00f);

    int* aiConnect = new int[3*iTriangleQuantity];
    aiConnect[0] = 0; aiConnect[1] = 1; aiConnect[2] = 2;
    aiConnect[3] = 0; aiConnect[4] = 2; aiConnect[5] = 3;

    TriMesh* pkPlane = new TriMesh(iVertexQuantity,akVertex,NULL,akColor,
        NULL,iTriangleQuantity,aiConnect);

    return pkPlane;
}
//----------------------------------------------------------------------------
void InverseKinematics::UpdateLine ()
{
    Vector3f* pkVertex = m_spkLine->Vertices();
    pkVertex[0] = m_spkRoot->WorldTranslate();
    pkVertex[1] = m_spkEffector->WorldTranslate();
    m_spkLine->UpdateModelBound();
    m_spkLine->UpdateGS(0.0f);
}
//----------------------------------------------------------------------------
bool InverseKinematics::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCamLoc(0.0f,-2.0f,0.5f);
    Matrix3f kCamOrient(-1.0f,0.0f,0.0f,0.0f,0.0f,1.0f,0.0f,1.0f,0.0f);
    ms_spkCamera->SetFrame(kCamLoc,kCamOrient);


    // ** layout of scene graph **
    // scene
    //     line
    //     iknode
    //         plane
    //         target
    //             goal
    //         root
    //             origin
    //             effector
    //                 end

    // create objects
    m_spkScene = new Node(2);
    Node* pkIKNode = new Node(3);
    m_spkRoot = new Node(2);
    m_spkEffector = new Node(1);
    m_spkTarget = new Node;
    m_spkLine = CreateLine();
    TriMesh* pkPlane = CreatePlane();
    TriMesh* pkGoal = CreateCube();
    TriMesh* pkOrigin = CreateCube();
    TriMesh* pkEnd = CreateCube();

    // transform objects
    pkIKNode->Rotate().FromAxisAngle(Vector3f::UNIT_Y,0.1f);
    pkIKNode->Translate() = Vector3f(0.1f,0.1f,0.1f);
    m_spkTarget->Translate() = 2.0f*Vector3f::UNIT_Y;
    m_spkEffector->Translate() = Vector3f::UNIT_X;

    // set parent-child links
    m_spkScene->AttachChild(m_spkLine);
    m_spkScene->AttachChild(pkIKNode);
    pkIKNode->AttachChild(pkPlane);
    pkIKNode->AttachChild(m_spkTarget);
    pkIKNode->AttachChild(m_spkRoot);
    m_spkTarget->AttachChild(pkGoal);
    m_spkRoot->AttachChild(pkOrigin);
    m_spkRoot->AttachChild(m_spkEffector);
    m_spkEffector->AttachChild(pkEnd);

    // create joints
    IKJoint** apkJoint = new IKJoint*[2];
    apkJoint[0] = new IKJoint(m_spkRoot);
    apkJoint[0]->AllowRotation(2) = true;
    apkJoint[1] = new IKJoint(m_spkEffector);
    apkJoint[1]->AllowTranslation(2) = true;

    // create goal
    IKGoal** apkGoal = new IKGoal*[1];
    apkGoal[0] = new IKGoal(m_spkTarget,m_spkEffector,1.0f);

    // create IK controller
    m_pkIKCtrl = new IKController(2,apkJoint,1,apkGoal,1);
    m_pkIKCtrl->Active() = false;
    m_spkRoot->AttachControl(m_pkIKCtrl);

    // set desired render state
    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);
    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();
    UpdateLine();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.1f;
    m_fRotSpeed = 0.01f;

    return true;
}
//----------------------------------------------------------------------------
void InverseKinematics::OnTerminate ()
{
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkTarget = NULL;
    m_spkLine = NULL;
    m_spkRoot = NULL;
    m_spkEffector = NULL;

    // m_pkIKCtrl will be destroyed by the scene graph destruction
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void InverseKinematics::OnIdle ()
{
    MeasureTime();

    MoveCamera();

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
bool InverseKinematics::Transform (unsigned char ucKey)
{
    Matrix3f rot, incr;

    switch ( ucKey )
    {
        case 'x':
            m_spkTarget->Translate().X() -= m_fTrnSpeed;
            break;
        case 'X':
            m_spkTarget->Translate().X() += m_fTrnSpeed;
            break;
        case 'y':
            m_spkTarget->Translate().Y() -= m_fTrnSpeed;
            break;
        case 'Y':
            m_spkTarget->Translate().Y() += m_fTrnSpeed;
            break;
        case 'z':
            m_spkTarget->Translate().Z() -= m_fTrnSpeed;
            break;
        case 'Z':
            m_spkTarget->Translate().Z() += m_fTrnSpeed;
            break;
        case 'r':
            rot = m_spkTarget->Rotate();
            incr.FromAxisAngle(Vector3f::UNIT_X,m_fRotSpeed);
            m_spkTarget->Rotate() = incr*rot;
            break;
        case 'R':
            rot = m_spkTarget->Rotate();
            incr.FromAxisAngle(Vector3f::UNIT_X,-m_fRotSpeed);
            m_spkTarget->Rotate() = incr*rot;
            break;
        case 'a':
            rot = m_spkTarget->Rotate();
            incr.FromAxisAngle(Vector3f::UNIT_Y,m_fRotSpeed);
            m_spkTarget->Rotate() = incr*rot;
            break;
        case 'A':
            rot = m_spkTarget->Rotate();
            incr.FromAxisAngle(Vector3f::UNIT_Y,-m_fRotSpeed);
            m_spkTarget->Rotate() = incr*rot;
            break;
        case 'p':
            rot = m_spkTarget->Rotate();
            incr.FromAxisAngle(Vector3f::UNIT_Z,m_fRotSpeed);
            m_spkTarget->Rotate() = incr*rot;
            break;
        case 'P':
            rot = m_spkTarget->Rotate();
            incr.FromAxisAngle(Vector3f::UNIT_Z,-m_fRotSpeed);
            m_spkTarget->Rotate() = incr*rot;
            break;
        default:
            return false;
    }

    m_spkScene->UpdateGS(0.0f);
    UpdateLine();
    return true;
}
//----------------------------------------------------------------------------
void InverseKinematics::OnKeyDown (unsigned char ucKey, int, int)
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
    case '0':  // reset frame rate measurements
        ResetTime();
        return;
    case 'w':
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        return;
    case 'i':
        m_pkIKCtrl->Active() = !m_pkIKCtrl->Active();
        m_spkScene->UpdateGS(0.0f);
        UpdateLine();
        return;
    }
}
//----------------------------------------------------------------------------
