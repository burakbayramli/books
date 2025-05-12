// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "GelatinBlob.h"
#include "WmlEdgeKey.h"
#include <set>
using namespace std;

GelatinBlob g_kTheApp;

//#define SINGLE_STEP

//----------------------------------------------------------------------------
GelatinBlob::GelatinBlob ()
    :
    Application("GelatinBlob",0,0,640,480,
        ColorRGB(0.713725f,0.807843f,0.929411f))
{
    m_pkModule = NULL;
}
//----------------------------------------------------------------------------
GelatinBlob::~GelatinBlob ()
{
}
//----------------------------------------------------------------------------
bool GelatinBlob::OnInitialize ()
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
    m_fTrnSpeed = 0.01f;
    m_fRotSpeed = 0.01f;
    m_bTurretActive = true;
    SetTurretAxes();
    return true;
}
//----------------------------------------------------------------------------
void GelatinBlob::OnTerminate ()
{
    delete m_pkModule;
    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void GelatinBlob::OnIdle ()
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
        DrawFrameRate(8,GetHeight()-8,ColorRGB::WHITE);
        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();

    UpdateClicks();
}
//----------------------------------------------------------------------------
void GelatinBlob::OnKeyDown (unsigned char ucKey, int iX, int iY)
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
void GelatinBlob::CreateSphere ()
{
    TriMesh* pkMesh = NULL;
    bool bWantNormals = false;
    bool bWantColors = false;
    bool bWantUVs = true;
    bool bOutsideView = true;
    CreateIcosahedronMesh(pkMesh,bWantNormals,bWantColors,bWantUVs,
        bOutsideView);

    m_spkSphere = pkMesh;

    // texture for the water objects
    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("WaterA.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR_LINEAR;
    pkTexture->Wrap() = Texture::WM_WRAP_S_WRAP_T;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkSphere->SetRenderState(pkTS);

    // the texture has an alpha channel of 1/2
    AlphaState* pkAS = new AlphaState;
    pkAS->BlendEnabled() = true;
    m_spkSphere->SetRenderState(pkAS);
}
//----------------------------------------------------------------------------
void GelatinBlob::CreateSprings ()
{
    // The icosahedron has 12 vertices and 30 edges.  Each vertex is a
    // particle in the system.  Each edge represents a spring.  To keep the
    // icosahedron from collapsing, 12 immovable particles are added, each
    // outside the icosahedron in the normal direction above a vertex.  The
    // immovable particles are connected to their corresponding vertices with
    // springs.
    int iNumParticles = 24, iNumSprings = 42;

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
    m_pkModule = new PhysicsModule(iNumParticles,iNumSprings,fStep,
        fViscosity);

    // Set positions and velocities.  The first 12 positions are the vertices
    // of the icosahedron.  The last 12 are the extra particles added to
    // stabilize the system.
    const Vector3f* akVertex = m_spkSphere->Vertices();
    int i;
    for (i = 0; i < 12; i++)
    {
        m_pkModule->SetMass(i,1.0f);
        m_pkModule->Position(i) = akVertex[i];
        m_pkModule->Velocity(i) = 0.1f*Vector3f(Mathf::SymmetricRandom(),
            Mathf::SymmetricRandom(),Mathf::SymmetricRandom());
    }
    for (i = 12; i < 24; i++)
    {
        m_pkModule->SetMass(i,Mathf::MAX_REAL);
        m_pkModule->Position(i) = 2.0f*akVertex[i-12];
        m_pkModule->Velocity(i) = Vector3f::ZERO;
    }

    // get unique set of edges for icosahedron
    set<EdgeKey> kEdge;
    int iTQuantity = m_spkSphere->GetTriangleQuantity();
    const int* piConnect = m_spkSphere->Connectivity();
    for (i = 0; i < iTQuantity; i++)
    {
        int iV0 = *piConnect++;
        int iV1 = *piConnect++;
        int iV2 = *piConnect++;
        kEdge.insert(EdgeKey(iV0,iV1));
        kEdge.insert(EdgeKey(iV1,iV2));
        kEdge.insert(EdgeKey(iV2,iV0));
    }

    // springs are at rest in the initial configuration
    const float fConstant = 10.0f;
    Vector3f kDiff;
    int iSpring = 0;
    set<EdgeKey>::iterator pkIter;
    for (pkIter = kEdge.begin(); pkIter != kEdge.end(); pkIter++)
    {
        const EdgeKey& rkKey = *pkIter;
        int iV0 = rkKey.V[0], iV1 = rkKey.V[1];
        kDiff = m_pkModule->Position(iV1) - m_pkModule->Position(iV0);
        m_pkModule->SetSpring(iSpring,iV0,iV1,fConstant,kDiff.Length());
        iSpring++;
    }
    for (i = 0; i < 12; i++)
    {
        kDiff = m_pkModule->Position(i+12) - m_pkModule->Position(i);
        m_pkModule->SetSpring(iSpring,i,i+12,fConstant,kDiff.Length());
        iSpring++;
    }
}
//----------------------------------------------------------------------------
void GelatinBlob::CreateSegments ()
{
    int iNumSprings = m_pkModule->GetNumSprings();
    m_spkSegments = new Node(iNumSprings);
    for (int i = 0; i < iNumSprings; i++)
    {
        int iV0, iV1;
        float fConstant, fLength;
        m_pkModule->GetSpring(i,iV0,iV1,fConstant,fLength);

        Vector3f* akVertex = new Vector3f[2];
        akVertex[0] = m_pkModule->Position(iV0);
        akVertex[1] = m_pkModule->Position(iV1);

        ColorRGB* akColor = new ColorRGB[2];
        akColor[0] = ColorRGB::WHITE;
        akColor[1] = ColorRGB::WHITE;

        Polyline* pkPoly = new Polyline(2,akVertex,NULL,akColor,NULL,false);
        m_spkSegments->AttachChild(pkPoly);
    }
}
//----------------------------------------------------------------------------
void GelatinBlob::CreateScene ()
{
    m_spkScene = new Node(1);
    m_spkTrnNode = new Node(2);
    m_spkScene->AttachChild(m_spkTrnNode);
    m_spkWireframe = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframe);
    ZBufferState* pkZBuffer = new ZBufferState;
    pkZBuffer->Enabled() = true;
    pkZBuffer->Writeable() = true;
    pkZBuffer->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(pkZBuffer);

    CreateSphere();
    CreateSprings();
    CreateSegments();

    // segments are opaque--draw first (sphere is transparent)
    m_spkTrnNode->AttachChild(m_spkSegments);
    m_spkTrnNode->AttachChild(m_spkSphere);
}
//----------------------------------------------------------------------------
void GelatinBlob::DoPhysical ()
{
    m_pkModule->Update(GetTimeInSeconds());

    // Update sphere surface.  The particle system and sphere maintain their
    // own copy of the vertices, so this update is necessary.
    Vector3f* akVertex = m_spkSphere->Vertices();
    int i;
    for (i = 0; i < 12; i++)
        akVertex[i] = m_pkModule->Position(i);

    m_spkSphere->UpdateModelBound();

    // update the segments representing the springs
    int iNumSprings = m_pkModule->GetNumSprings();
    for (i = 0; i < iNumSprings; i++)
    {
        int iV0, iV1;
        float fConstant, fLength;
        m_pkModule->GetSpring(i,iV0,iV1,fConstant,fLength);

        Polyline* pkPoly = WmlStaticCast(Polyline,m_spkSegments->GetChild(i));
        Vector3f* akVertex = pkPoly->Vertices();
        akVertex[0] = m_pkModule->Position(iV0);
        akVertex[1] = m_pkModule->Position(iV1);
    }
}
//----------------------------------------------------------------------------
