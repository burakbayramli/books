// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "IntersectingCylinders.h"

IntersectingCylinders g_kTheApp;

Vector2f IntersectingCylinders::ms_kBlueUV(0.25f,0.25f);
Vector2f IntersectingCylinders::ms_kCyanUV(0.75f,0.25f);
Vector2f IntersectingCylinders::ms_kRedUV(0.25f,0.75f);
Vector2f IntersectingCylinders::ms_kYellowUV(0.75f,0.75f);

//----------------------------------------------------------------------------
IntersectingCylinders::IntersectingCylinders ()
    :
    Application("IntersectingCylinders",0,0,640,480,ColorRGB(0.5f,0.0f,1.0f))
{
}
//----------------------------------------------------------------------------
void IntersectingCylinders::CreateCamera ()
{
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    Vector3f kCLoc(4.0f,0.0f,0.0f);
    Vector3f kCLeft(0.0f,-1.0f,0.0f);
    Vector3f kCUp(0.0f,0.0f,1.0f);
    Vector3f kCDir(-1.0f,0.0f,0.0f);
    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);
    ms_spkCamera->Update();

    m_bTurretActive = true;
    SetTurretAxes();
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;
}
//----------------------------------------------------------------------------
TriMesh* IntersectingCylinders::CreateCylinder (float fRadius, float fHeight,
    int iRadialSamples, int iHeightSamples, bool bBlue)
{
    int iTotalSamples = iRadialSamples*iHeightSamples;
    int iVertexQuantity = iTotalSamples + 2;
    int iTriangleQuantity = 2*iTotalSamples;
    float fHalfHeight = 0.5f*fHeight;

    Vector3f* akVertex = new Vector3f[iVertexQuantity];
    int iV0, iV1, iIndex;
    for (iV1 = 0; iV1 < iHeightSamples; iV1++)
    {
        float fZ = -fHalfHeight + fHeight*iV1/float(iHeightSamples-1);
        for (iV0 = 0; iV0 < iRadialSamples; iV0++)
        {
            float fAngle = Mathf::TWO_PI*iV0/iRadialSamples;
            float fX = fRadius*Mathf::Cos(fAngle);
            float fY = fRadius*Mathf::Sin(fAngle);
            iIndex = iV0 + iRadialSamples*iV1;
            akVertex[iIndex] = Vector3f(fX,fY,fZ);
        }
    }

    akVertex[iTotalSamples] = Vector3f(0.0f,0.0f,-fHalfHeight);
    akVertex[iTotalSamples+1] = Vector3f(0.0f,0.0f,fHalfHeight);

    Vector2f* akTexture = new Vector2f[iVertexQuantity];
    if ( bBlue )
    {
        for (iV0 = 0; iV0 < iVertexQuantity; iV0++)
            akTexture[iV0] = ms_kBlueUV;
    }
    else
    {
        for (iV0 = 0; iV0 < iVertexQuantity; iV0++)
            akTexture[iV0] = ms_kRedUV;
    }

    int* aiConnect = new int[3*iTriangleQuantity];
    int i = 0;

    // bottom circle
    int i00, i10, i01, i11;
    for (iV0 = 0; iV0+1 < iRadialSamples; iV0++)
    {
        aiConnect[i++] = iV0+1;
        aiConnect[i++] = iV0;
        aiConnect[i++] = iTotalSamples;
    }
    aiConnect[i++] = 0;
    aiConnect[i++] = iRadialSamples-1;
    aiConnect[i++] = iTotalSamples;

    // top circle
    iV1 = iHeightSamples-1;
    for (iV0 = 0; iV0+1 < iRadialSamples; iV0++)
    {
        iIndex = iV0 + iRadialSamples*iV1;
        aiConnect[i++] = iIndex;
        aiConnect[i++] = iIndex+1;
        aiConnect[i++] = iTotalSamples+1;
    }
    aiConnect[i++] = iTotalSamples-1;
    aiConnect[i++] = iTotalSamples-iRadialSamples;
    aiConnect[i++] = iTotalSamples+1;

    // side wall
    for (iV1 = 0; iV1+1 < iHeightSamples; iV1++)
    {
        for (iV0 = 0; iV0+1 < iRadialSamples; iV0++)
        {
            i00 = iV0 + iRadialSamples*iV1;
            i10 = i00 + 1;
            i01 = i00 + iRadialSamples;
            i11 = i01 + 1;

            aiConnect[i++] = i01;
            aiConnect[i++] = i00;
            aiConnect[i++] = i10;

            aiConnect[i++] = i01;
            aiConnect[i++] = i10;
            aiConnect[i++] = i11;
        }

        i00 = iRadialSamples-1 + iRadialSamples*iV1;
        i10 = iRadialSamples*iV1;
        i01 = i00 + iRadialSamples;
        i11 = iRadialSamples*(iV1+1);

        aiConnect[i++] = i01;
        aiConnect[i++] = i00;
        aiConnect[i++] = i10;

        aiConnect[i++] = i01;
        aiConnect[i++] = i10;
        aiConnect[i++] = i11;
    }

    TriMesh* pkCyln = new TriMesh(iVertexQuantity,akVertex,NULL,NULL,
        akTexture,iTriangleQuantity,aiConnect);

    return pkCyln;
}
//----------------------------------------------------------------------------
void IntersectingCylinders::CreateScene ()
{
    // root of scene will have two cylinders as children
    m_spkScene = new Node(2);

    // set up wireframe state
    m_spkWireframe = new WireframeState;
    m_spkWireframe->Enabled() = false;
    m_spkScene->SetRenderState(m_spkWireframe);

    // set up z-buffer state
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    // set up texture image, texture, and texture state
    unsigned int* auiData = new unsigned int[4];
#ifdef WML_BIG_ENDIAN
    auiData[0] = 0x0000FFFF;  // (0.25,0.25) maps to blue
    auiData[1] = 0x00FFFFFF;  // (0.25,0.75) maps to cyan
    auiData[2] = 0xFF0000FF;  // (0.75,0.25) maps to red
    auiData[3] = 0xFFFF00FF;  // (0.75,0.75) maps to yellow
#else
    auiData[0] = 0xFFFF0000;  // (0.25,0.25) maps to blue
    auiData[1] = 0xFFFFFF00;  // (0.25,0.75) maps to cyan
    auiData[2] = 0xFF0000FF;  // (0.75,0.25) maps to red
    auiData[3] = 0xFF00FFFF;  // (0.75,0.75) maps to yellow
#endif
    Image* pkImage = new Image(Image::IT_RGBA8888,2,2,
        (unsigned char*)auiData);

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(pkImage);
    pkTexture->Filter() = Texture::FM_LINEAR;

    TextureState* pkTextureState = new TextureState;
    pkTextureState->Set(0,pkTexture);
    m_spkScene->SetRenderState(pkTextureState);

    // create two cylinders, one short/think, one tall/thin
    m_spkCyln0 = CreateCylinder(1.0,2.0,16,8,true);
    m_spkScene->AttachChild(m_spkCyln0);
    m_spkCyln1 = CreateCylinder(0.25,4.0,8,16,false);
    m_spkScene->AttachChild(m_spkCyln1);

    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // Set up collision stuff.  Record0 handles the collision response.
    // Record1 is not given a callback so that 'double processing' of the
    // events does not occur.  The bounding volume type is one of
    // BV_BOX, BV_CAPSULE, BV_LOZENGE, BV_SPHERE
    BoundingVolume::Type eType = BoundingVolume::BV_BOX;

    CollisionRecord* pkRec0 = new CollisionRecord(m_spkCyln0,0,this,
        Response,eType,1,false);

    CollisionRecord* pkRec1 = new CollisionRecord(m_spkCyln1,0,0,0,
        eType,1,false);

    m_kGroup.Add(pkRec0);
    m_kGroup.Add(pkRec1);

    m_kGroup.TestIntersection();
}
//----------------------------------------------------------------------------
bool IntersectingCylinders::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    CreateCamera();
    CreateScene();

    return true;
}
//----------------------------------------------------------------------------
void IntersectingCylinders::OnTerminate ()
{
    m_spkScene = NULL;
    m_spkCyln0 = NULL;
    m_spkCyln1 = NULL;
    m_spkWireframe = NULL;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void IntersectingCylinders::OnIdle ()
{
    MeasureTime();

    MoveCamera();

    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-1,ColorRGB::WHITE);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
bool IntersectingCylinders::Transform (unsigned char ucKey)
{
    // Move the tall/thin cylinder.  After each motion, reset the texture
    // coordinates to the "no intersection" state, then let the collision
    // system test for intersection.  Any intersecting triangles have their
    // texture coordinates changed to the "intersection" state.

    float fTrnSpeed = 0.1f;
    float fRotSpeed = 0.1f;

    Matrix3f kRot, kIncr;

    switch ( ucKey )
    {
        case 'x':
            m_spkCyln1->Translate().X() -= fTrnSpeed;
            break;
        case 'X':
            m_spkCyln1->Translate().X() += fTrnSpeed;
            break;
        case 'y':
            m_spkCyln1->Translate().Y() -= fTrnSpeed;
            break;
        case 'Y':
            m_spkCyln1->Translate().Y() += fTrnSpeed;
            break;
        case 'z':
            m_spkCyln1->Translate().Z() -= fTrnSpeed;
            break;
        case 'Z':
            m_spkCyln1->Translate().Z() += fTrnSpeed;
            break;
        case 'r':
            kRot = m_spkCyln1->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_X,fRotSpeed);
            m_spkCyln1->Rotate() = kIncr*kRot;
            break;
        case 'R':
            kRot = m_spkCyln1->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_X,-fRotSpeed);
            m_spkCyln1->Rotate() = kIncr*kRot;
            break;
        case 'a':
            kRot = m_spkCyln1->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Y,fRotSpeed);
            m_spkCyln1->Rotate() = kIncr*kRot;
            break;
        case 'A':
            kRot = m_spkCyln1->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Y,-fRotSpeed);
            m_spkCyln1->Rotate() = kIncr*kRot;
            break;
        case 'p':
            kRot = m_spkCyln1->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Z,fRotSpeed);
            m_spkCyln1->Rotate() = kIncr*kRot;
            break;
        case 'P':
            kRot = m_spkCyln1->Rotate();
            kIncr.FromAxisAngle(Vector3f::UNIT_Z,-fRotSpeed);
            m_spkCyln1->Rotate() = kIncr*kRot;
            break;
        default:
            return false;
    }

    // activate the collision system
    m_spkCyln1->UpdateGS(0.0f);
    ResetColors();
    m_kGroup.TestIntersection();
    return true;
}
//----------------------------------------------------------------------------
void IntersectingCylinders::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    // move the tall/thin cylinder and activate the collision system
    if ( Transform(ucKey) )
        return;

    switch ( ucKey )
    {
    case '0':
        // reset the frame rate counter
        ResetTime();
        break;
    case 'w':
    case 'W':
        // toggle wireframe mode
        m_spkWireframe->Enabled() = !m_spkWireframe->Enabled();
        break;
    }
}
//----------------------------------------------------------------------------
void IntersectingCylinders::ResetColors ()
{
    int i, iVMax = m_spkCyln0->GetVertexQuantity();
    Vector2f* akTexture = m_spkCyln0->Textures();
    for (i = 0; i < iVMax; i++)
        akTexture[i] = ms_kBlueUV;

    iVMax = m_spkCyln1->GetVertexQuantity();
    akTexture = m_spkCyln1->Textures();
    for (i = 0; i < iVMax; i++)
        akTexture[i] = ms_kRedUV;
}
//----------------------------------------------------------------------------
void IntersectingCylinders::Response (CollisionRecord& rkRecord0,
    int iT0, CollisionRecord& rkRecord1, int iT1, void*)
{
    TriMesh* pkMesh;
    Vector2f* akTexture;
    int i0, i1, i2;

    // mesh0 triangles that are intersecting change from blue to cyan
    pkMesh = rkRecord0.GetMesh();
    pkMesh->GetTriangle(iT0,i0,i1,i2);
    akTexture = pkMesh->Textures();
    akTexture[i0] = ms_kCyanUV;
    akTexture[i1] = ms_kCyanUV;
    akTexture[i2] = ms_kCyanUV;

    // mesh1 triangles that are intersecting change from red to yellow
    pkMesh = rkRecord1.GetMesh();
    pkMesh->GetTriangle(iT1,i0,i1,i2);
    akTexture = pkMesh->Textures();
    akTexture[i0] = ms_kYellowUV;
    akTexture[i1] = ms_kYellowUV;
    akTexture[i2] = ms_kYellowUV;
}
//----------------------------------------------------------------------------
