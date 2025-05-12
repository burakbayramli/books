// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "BouncingTetrahedra.h"
#include "WmlLCPPolyDist.h"
using namespace Wml;

BouncingTetrahedra g_kTheApp;

//----------------------------------------------------------------------------
BouncingTetrahedra::BouncingTetrahedra ()
    :
    Application("BouncingTetrahedra",0,0,640,480,ColorRGB(1.0f,1.0f,1.0f))
{
    m_fTolerance = 1e-12f;
    m_fTotalKE = 0.0f;
    m_fSimTime = 0.0f;
    m_fSimDelta = 0.01f;
}
//----------------------------------------------------------------------------
BouncingTetrahedra::~BouncingTetrahedra ()
{
}
//----------------------------------------------------------------------------
bool BouncingTetrahedra::OnInitialize ()
{
    if ( !Application::OnInitialize() )
        return false;

    // set up camera
    ms_spkCamera->SetFrustum(1.0f,1000.0f,-0.55f,0.55f,0.4125f,-0.4125f);
    float fAngle = 0.02f*Mathf::PI;
    float fCos = Mathf::Cos(fAngle), fSin = Mathf::Sin(fAngle);
    Vector3f kCUp(-fSin,0.0f,fCos);
    Vector3f kCDir(-fCos,0.0f,-fSin);
    Vector3f kCLeft = kCUp.Cross(kCDir);
    Vector3f kCLoc(27.5f,8.0f,8.9f);

    ms_spkCamera->SetFrame(kCLoc,kCLeft,kCUp,kCDir);

    // create scene
    CreateTetra();
    CreateFloor();
    CreateBackWall();
    CreateSideWall1();
    CreateSideWall2();

    // ** layout of scene graph **
    // scene
    //     room
    //         floor
    //         back wall
    //         side wall 1
    //         side wall 2
    //     tetra

    m_spkScene = new Node(2);
    Node* pkRoom = new Node(4);
    Node* pkTetra = new Node(NUM_TETRA);
    m_spkScene->AttachChild(pkRoom);
    pkRoom->AttachChild(m_spkFloor);
    pkRoom->AttachChild(m_spkBackWall);
    pkRoom->AttachChild(m_spkSideWall1);
    pkRoom->AttachChild(m_spkSideWall2);
    m_spkScene->AttachChild(pkTetra);
    int i;
    for (i = 0; i < NUM_TETRA; i++)
        pkTetra->AttachChild(m_aspkTetra[i]);

    // wireframe
    m_spkWireframeState = new WireframeState;
    m_spkScene->SetRenderState(m_spkWireframeState);

    // depth buffer
    m_spkZBufferState = new ZBufferState;
    m_spkZBufferState->Enabled() = true;
    m_spkZBufferState->Writeable() = true;
    m_spkZBufferState->Compare() = ZBufferState::CF_LEQUAL;
    m_spkScene->SetRenderState(m_spkZBufferState);

    // initial update of objects
    ms_spkCamera->Update();
    m_spkScene->UpdateGS(0.0f);
    m_spkScene->UpdateRS();

    // The tetrahedra are constrained to bounce around in a rectangular solid
    // region.  The six defining planes are defined to be immovable rigid
    // bodies.  The boundaries are parallel to coordinate axes and pass
    // through the points indicated by the value other than +-100.  That is,
    // the back wall is at x = 1, the left wall is at y = 2, the floor is at
    // z = 1, the right wall is at y = 15, the ceiling is at z = 17, and the
    // front wall is at x = 8.  The ceiling and front wall are invisible
    // objects (not rendered), but you will see balls bouncing against it
    // and reflecting away from it towards the back wall.
    m_akBLocation[0] = Vector3f(1.0,-100.0,-100.0);
    m_akBNormal[0] = Vector3f(1.0,0,0);
    m_akBLocation[1] = Vector3f(-100.0,2.0,-100.0);
    m_akBNormal[1] = Vector3f(0,1.0,0);
    m_akBLocation[2] = Vector3f( -100.0,-100.0,1.0);
    m_akBNormal[2] = Vector3f(0,0,1.0);
    m_akBLocation[3] = Vector3f( 100.0,15.0,100.0);
    m_akBNormal[3] = Vector3f(0,-1.0,0);
    m_akBLocation[4] = Vector3f( 100.0,100.0,17.0);
    m_akBNormal[4] = Vector3f(0,0,-1.0);
    m_akBLocation[5] = Vector3f( 8.0,100.0,100.0);
    m_akBNormal[5] = Vector3f(-1.0,0,0);
    for (i = 0; i < 6; i++)
    {
        m_akBoundary[i].SetMass(0.0f);
        m_akBoundary[i].SetPosition(m_akBLocation[i]);
    }

    // The face connectivity is the same for all the tetrahedra.  This
    // information is required by the LCP solver.
    m_akFaces[0][0] = 0, m_akFaces[0][1] = 2, m_akFaces[0][2] = 1;
    m_akFaces[1][0] = 0, m_akFaces[1][1] = 3, m_akFaces[1][2] = 2;
    m_akFaces[2][0] = 0, m_akFaces[2][1] = 1, m_akFaces[2][2] = 3;
    m_akFaces[3][0] = 1, m_akFaces[3][1] = 2, m_akFaces[3][2] = 3;

    // initialize the simulation
    DoPhysical();
    return true;
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::OnTerminate()
{
    for (int i = 0; i < NUM_TETRA; i++)
    {
        delete m_apkTetra[i];
        m_aspkTetra[i] = NULL;
    }

    m_spkFloor = NULL;
    m_spkBackWall = NULL;
    m_spkSideWall1 = NULL;
    m_spkSideWall2 = NULL;
    m_spkWireframeState = NULL;
    m_spkZBufferState = NULL;
    m_spkScene = NULL;

    Application::OnTerminate();
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::OnIdle ()
{
    MeasureTime();
    DoPhysical();
    DoVisual();
    UpdateClicks();
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'w':  // toggle wireframe
        m_spkWireframeState->Enabled() = !m_spkWireframeState->Enabled();
        break;

#ifdef SINGLE_STEP
    case 'g':
        m_fSimTime += m_fSimDelta;
        DoPhysical();
        break;
#endif
    }
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::CreateTetra ()
{
    for (int i = 0; i < NUM_TETRA; i++)
    {
        float fSize = 3.0f - 0.5f*i;
        float fMass = 8.0f*fSize;
        Vector3f kPos = Vector3f(3.0f,3.0f+3.0f*i,14.0f-3.0f*i);
        Vector3f kLinMom = 0.01f*Vector3f(
            2.0f + 100.0f*Mathf::SymmetricRandom(),
            2.0f + 100.0f*Mathf::SymmetricRandom(),
            -1.2f + 100.0f*Mathf::SymmetricRandom());
        Vector3f kAngMom = 0.01f*Vector3f(
            1.0f + 100.0f*Mathf::SymmetricRandom(),
            2.0f + 100.0f*Mathf::SymmetricRandom(),
            3.0f + 100.0f*Mathf::SymmetricRandom());

        m_apkTetra[i] = new RigidTetra(fSize,fMass,kPos,kLinMom,kAngMom);

        m_apkTetra[i]->Force = Force;
        m_apkTetra[i]->Torque = Torque;
        m_apkTetra[i]->SetInternalForce(Vector3f::ZERO);
        m_apkTetra[i]->SetInternalTorque(Vector3f::ZERO);
        m_apkTetra[i]->SetExternalForce(Vector3f::ZERO);
        m_apkTetra[i]->SetExternalTorque(Vector3f::ZERO);

        m_aspkTetra[i] = new Node(1);
        m_aspkTetra[i]->AttachChild(m_apkTetra[i]->Mesh());
    }
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::CreateFloor ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(1.0f,1.0f,1.0f);
    akVertex[1] = Vector3f(17.0f,1.0f,1.0f);
    akVertex[2] = Vector3f(17.0f,20.0f,1.0f);
    akVertex[3] = Vector3f(1.0f,20.0f,1.0f);

    ColorRGB kFloorColor(155.0f/255.0f,177.0f/255.0f,164.0f/255.0f);
    ColorRGB* akColor = new ColorRGB[4];
    akColor[0] = kFloorColor;
    akColor[1] = kFloorColor;
    akColor[2] = kFloorColor;
    akColor[3] = kFloorColor;

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    m_spkFloor = new TriMesh(4,akVertex,NULL,akColor,NULL,2,aiConnect);
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::CreateSideWall1 ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(1.0f,15.0f,1.0f);
    akVertex[1] = Vector3f(17.0f,15.0f,1.0f);
    akVertex[2] = Vector3f(17.0f,15.0f,17.0f);
    akVertex[3] = Vector3f(1.0f,15.0f,17.0f);

    ColorRGB kSideWall1Color(170.0f/255.0f,187.0f/255.0f,219.0f/255.0f);
    ColorRGB* akColor = new ColorRGB[4];
    akColor[0] = kSideWall1Color;
    akColor[1] = kSideWall1Color;
    akColor[2] = kSideWall1Color;
    akColor[3] = kSideWall1Color;

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    m_spkSideWall1 = new TriMesh(4,akVertex,NULL,akColor,NULL,2,aiConnect);
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::CreateSideWall2 ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(17.0f,2.0f,1.0f);
    akVertex[1] = Vector3f(1.0f,2.0f,1.0f);
    akVertex[2] = Vector3f(1.0f,2.0f,17.0f);
    akVertex[3] = Vector3f(17.0f,2.0f,17.0f);

    ColorRGB kSideWall2Color(170.0f/255.0f,187.0f/255.0f,219.0f/255.0f);
    ColorRGB* akColor = new ColorRGB[4];
    akColor[0] = kSideWall2Color;
    akColor[1] = kSideWall2Color;
    akColor[2] = kSideWall2Color;
    akColor[3] = kSideWall2Color;

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    m_spkSideWall2 = new TriMesh(4,akVertex,NULL,akColor,NULL,2,aiConnect);
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::CreateBackWall ()
{
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = Vector3f(1.0f,1.0f,1.0f);
    akVertex[1] = Vector3f(1.0f,20.0f,1.0f);
    akVertex[2] = Vector3f(1.0f,20.0f,17.0f);
    akVertex[3] = Vector3f(1.0f,1.0f,17.0f);

    ColorRGB kBackWallColor(209.0f/255.0f,204.0f/255.0f,180.0f/255.0f);
    ColorRGB* akColor = new ColorRGB[4];
    akColor[0] = kBackWallColor;
    akColor[1] = kBackWallColor;
    akColor[2] = kBackWallColor;
    akColor[3] = kBackWallColor;

    int* aiConnect = new int[6];
    aiConnect[0] = 0;  aiConnect[1] = 1;  aiConnect[2] = 2;
    aiConnect[3] = 0;  aiConnect[4] = 2;  aiConnect[5] = 3;

    m_spkBackWall = new TriMesh(4,akVertex,NULL,akColor,NULL,2,aiConnect);
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::DoVisual ()
{
    ms_spkRenderer->ClearBuffers();
    if ( ms_spkRenderer->BeginScene() )
    {
        ms_spkRenderer->Draw(m_spkScene);
        DrawFrameRate(8,GetHeight()-8,ColorRGB::BLACK);

        char acMsg[256];
        sprintf(acMsg,"Time = %5.2f",m_fSimTime);
        ms_spkRenderer->Draw(95,GetHeight()-8,ColorRGB::BLACK,acMsg);

        ms_spkRenderer->EndScene();
    }
    ms_spkRenderer->DisplayBackBuffer();
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::DoPhysical ()
{
    DoCollisionDetection();
    DoCollisionResponse();

    m_fTotalKE = 0.0f;
    for (int i = 0; i < NUM_TETRA; i++)
    {
        const RigidTetra& rkTetra = *m_apkTetra[i];
        float fInvMass = rkTetra.GetInverseMass();
        const Matrix3f& rkInertia = rkTetra.GetWorldInertia();
        const Vector3f& rkPos = rkTetra.GetPosition();
        const Vector3f& rkLinMom = rkTetra.GetLinearMomentum();
        const Matrix3f& rkROrient = rkTetra.GetROrientation();
        const Vector3f& rkAngVel = rkTetra.GetAngularVelocity();

        m_aspkTetra[i]->Translate() = rkPos;
        m_aspkTetra[i]->Rotate() = rkROrient;

        m_fTotalKE += fInvMass*rkLinMom.Dot(rkLinMom) +
            rkAngVel.Dot(rkInertia*rkAngVel);
    }
    m_fTotalKE *= 0.5f;

    // update the scene graph
    m_spkScene->UpdateGS(0.0f);

    // next simulation time: see DoMotion for calculation of m_fSimDelta
    m_fSimTime += m_fSimDelta;
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::DoCollisionDetection ()
{
    int i, j;
    Contact kContact;
    m_kContact.clear();

    // test for tetrahedron-boundary collisions
    for (i = 0; i < NUM_TETRA; i++)
    {
        m_apkTetra[i]->Moved() = false;
        if ( FarFromBoundary(i) )
            continue;

        // These checks are done in pairs under the assumption that the tetra 
        // have smaller diameters than the separation of opposite boundaries, 
        // hence only one of each opposite pair of boundaries may be touched 
        // at any one time.
        Vector3f akVertex[4];
        float afDistance[4];
        m_apkTetra[i]->GetVertices(akVertex);
        float fRadius = m_apkTetra[i]->GetRadius();
        Vector3f kPos = m_apkTetra[i]->GetPosition();

        // rear [0] and front[5] boundaries
        if ( kPos.X() - fRadius < m_akBLocation[0].X() )
        {
            for (j = 0; j < 4; j++)
                afDistance[j] = akVertex[j].X() - m_akBLocation[0].X();
            TetraBoundaryIntersection(i,0,afDistance,kContact);
        }
        else if ( kPos.X() + fRadius > m_akBLocation[5].X() )
        {
            for (j = 0; j < 4; j++)
                afDistance[j] = m_akBLocation[5].X() - akVertex[j].X();
            TetraBoundaryIntersection(i,5,afDistance,kContact);
        }

        // left [1] and right [3] boundaries
        if ( kPos.Y() - fRadius < m_akBLocation[1].Y() )
        {
            for (j = 0; j < 4; j++)
                afDistance[j] = akVertex[j].Y() - m_akBLocation[1].Y();
            TetraBoundaryIntersection(i,1,afDistance,kContact);
        }
        else if ( kPos.Y() + fRadius > m_akBLocation[3].Y() )
        {
            for (j = 0; j < 4; j++)
                afDistance[j] = m_akBLocation[3].Y() - akVertex[j].Y();
            TetraBoundaryIntersection(i,3,afDistance,kContact);
        }

        // bottom [2] and top [4] boundaries
        if ( kPos.Z() - fRadius < m_akBLocation[2].Z() )
        {
            for (j = 0; j < 4; j++)
                afDistance[j] = akVertex[j].Z() - m_akBLocation[2].Z();
            TetraBoundaryIntersection(i,2,afDistance,kContact);
        }
        else if ( kPos.Z()+fRadius > m_akBLocation[4].Z() )
        {
            for (j = 0; j < 4; j++)
                afDistance[j] = m_akBLocation[4].Z() - akVertex[j].Z();
            TetraBoundaryIntersection(i,4,afDistance,kContact);
        }
    }    

    // test for tetrahedron-tetrahedron collisions
    m_iLCPCount = 0;
    for (i = 0; i < NUM_TETRA-1; i++)
    {
        Vector3f akVertex0[4];
        m_apkTetra[i]->GetVertices(akVertex0);

        for (j = i+1; j < NUM_TETRA; j++)
        {
            Vector3f akVertex1[4];
            m_apkTetra[j]->GetVertices(akVertex1);

            if ( !FarApart(i,j) )
            {
                float fDist = 1.0f;
                int iStatusCode = 0;
                Vector3f akRes[2];
                LCPPolyDist3(4,akVertex0,4,m_akFaces,4,akVertex1,4,m_akFaces,
                    iStatusCode,fDist,akRes);
                m_iLCPCount++;
                if ( fDist <= m_fTolerance )
                {
                    // collision with good LCPPolyDist results
                    Reposition(i,j,kContact);
                    m_kContact.push_back(kContact);
                }
            }
        }
    }

    m_iNumContacts = (int)m_kContact.size();
}
//----------------------------------------------------------------------------
bool BouncingTetrahedra::FarFromBoundary (int i)
{
    // The tests are arranged so that the most likely to be encountered
    // (the floor) is tested first and the least likely to be encountered
    // (the ceiling) is tested last.

    Vector3f kPos = m_apkTetra[i]->GetPosition();
    float fRadius = m_apkTetra[i]->GetRadius();

    return kPos.Z()-fRadius >= m_akBLocation[2].Z()
        && kPos.X()-fRadius >= m_akBLocation[0].X()
        && kPos.X()+fRadius <= m_akBLocation[5].X()
        && kPos.Y()-fRadius >= m_akBLocation[1].Y()
        && kPos.Y()+fRadius <= m_akBLocation[3].Y()
        && kPos.Z()+fRadius <= m_akBLocation[4].Z();
}
//----------------------------------------------------------------------------
bool BouncingTetrahedra::FarApart (int iT0, int iT1)
{
    Vector3f kPos0 = m_apkTetra[iT0]->GetPosition();
    float fRadius0 = m_apkTetra[iT0]->GetRadius();
    Vector3f kPos1 = m_apkTetra[iT1]->GetPosition();
    float fRadius1 = m_apkTetra[iT1]->GetRadius();
    return (kPos0-kPos1).Length() >= fRadius0 + fRadius1;
}
//----------------------------------------------------------------------------
bool BouncingTetrahedra::TetraBoundaryIntersection (int iTetra,
    int iBoundary, float* afDistance, Contact& rkContact)
{
    int iHitIndex = -1;
    float fDepthMax = 0.0f;
    for (int j = 0; j < 4; j++)
    {
        float fDepth = afDistance[j];
        if ( fDepth < fDepthMax )
        {
            fDepthMax = fDepth;
            iHitIndex = j;
        }
    }
    if ( iHitIndex != -1 )
    {
        BuildContactMoveTetra(iTetra,iBoundary,iHitIndex,fDepthMax,rkContact);
        m_kContact.push_back(rkContact);
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::BuildContactMoveTetra (int iTetra, int iBoundary,
    int iHitIndex, float fDepthMax, Contact& rkContact)
{
    rkContact.A = &m_akBoundary[iBoundary];
    rkContact.B = m_apkTetra[iTetra];
    rkContact.isVFContact = true;
    rkContact.N = m_akBNormal[iBoundary];
    rkContact.PA = Vector3f::ZERO;

    Vector3f akVertex[4];
    m_apkTetra[iTetra]->GetVertices(akVertex);
    rkContact.PB = akVertex[iHitIndex];
    
    // move intersecting tetra to surface of boundary
    Vector3f kPos = m_apkTetra[iTetra]->GetPosition();
    m_apkTetra[iTetra]->SetPosition(kPos-fDepthMax*rkContact.N);
    m_apkTetra[iTetra]->Moved() = true;
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::Reposition (int iT0, int iT1, Contact& rkContact)
{
    RigidTetra& rkTetra0 = *m_apkTetra[iT0];
    RigidTetra& rkTetra1 = *m_apkTetra[iT1];

    // compute the centroids of the tetrahedra
    Vector3f akVertex0[4], akVertex1[4];
    rkTetra0.GetVertices(akVertex0);
    rkTetra1.GetVertices(akVertex1);
    Vector3f kCentroid0 = Vector3f::ZERO;
    Vector3f kCentroid1 = Vector3f::ZERO;
    int i;
    for (i = 0; i < 4; i++)
    {
        kCentroid0 += akVertex0[i];
        kCentroid1 += akVertex1[i];
    }
    kCentroid0 *= 0.25f;
    kCentroid1 *= 0.25f;

    // Randomly perturb the tetrahedra vertices by a small amount.  This is
    // done to help prevent the LCP solver from getting into cycles and
    // degenerate cases.
    const float fReduction = 0.95f;
    float fReduceI = fReduction*Mathf::IntervalRandom(0.9999f,1.0001f);
    float fReduceJ = fReduction*Mathf::IntervalRandom(0.9999f,1.0001f);
    for (i = 0; i < 4; i++)
    {
        akVertex0[i] = kCentroid0 + (akVertex0[i] - kCentroid0)*fReduceI;
        akVertex1[i] = kCentroid1 + (akVertex1[i] - kCentroid1)*fReduceJ;
    }

    // compute the distance between the tetrahedra
    float fDist = 1.0f;
    int iStatusCode = 0;
    Vector3f akRes[2];
    LCPPolyDist3(4,akVertex0,4,m_akFaces,4,akVertex1,4,m_akFaces,iStatusCode,
        fDist,akRes);
    m_iLCPCount++;

    // In theory, LCPPolyDist3 should always find a valid distance, but just
    // in case numerical round-off errors cause problems, let us trap it.
    assert( fDist >= 0.0f );

    // This assertion is satisfied on Windows runs, but not on Macintosh
    // runs.
    //
    //assert(
    //    iStatusCode == LCPPolyDist3::SC_FOUND_SOLUTION ||
    //    iStatusCode == LCPPolyDist3::SC_TEST_POINTS_TEST_FAILED ||
    //    iStatusCode == LCPPolyDist3::SC_FOUND_TRIVIAL_SOLUTION
    //    );

    // reposition the tetrahedra to the theoretical points of contact
    akRes[0] = kCentroid0+(akRes[0]-kCentroid0)/fReduceI;
    akRes[1] = kCentroid1+(akRes[1]-kCentroid1)/fReduceJ;
    for (i = 0; i < 4; i++)
    {
        akVertex0[i] = kCentroid0 + (akVertex0[i] - kCentroid0)/fReduceI;
        akVertex1[i] = kCentroid1 + (akVertex1[i] - kCentroid1)/fReduceJ;
    }

    // Numerical round-off errors can cause interpenetration.  Move the
    // tetrahedra to back out of this situation.  The length of kDiff
    // estimates the depth of penetration when fDist > 0 was reported.
    Vector3f kDiff = akRes[0] - akRes[1];

    // Apply the separation distance along the line containing the centroids
    // of the tetrahedra.
    Vector3f kDiff2 = kCentroid1 - kCentroid0;
    kDiff = kDiff2/kDiff2.Length()*kDiff.Length();

    // Move each tetrahedron by half of kDiff when the distance was large,
    // but move each by twice kDiff when the distance is really small.
    float fMult = ( fDist >= m_fTolerance ? 0.5f : 1.0f );
    Vector3f kDelta = fMult*kDiff;

    // undo the interpenetration
    if ( rkTetra0.Moved() && !rkTetra1.Moved() )
    {
        // iT0 has been moved but iT1 has not
        rkTetra1.SetPosition(rkTetra1.GetPosition()+2.0f*kDelta);
        rkTetra1.Moved() = true;
    }
    else if ( !rkTetra0.Moved() && rkTetra1.Moved() )
    {
        // iT1 has been moved but iT0 has not
        rkTetra0.SetPosition(rkTetra0.GetPosition()-2.0f*kDelta);
        rkTetra0.Moved() = true;
    }
    else
    {
        // both moved or both did not move
        rkTetra0.SetPosition(rkTetra0.GetPosition()-kDelta);
        rkTetra0.Moved() = true;
        rkTetra1.SetPosition(rkTetra1.GetPosition()+kDelta);
        rkTetra1.Moved() = true;
    }

    // test if the two tetrahedra intersect in a vertex-face configuration
    rkContact.isVFContact = IsVertex(akVertex0,akRes[0]);
    if ( rkContact.isVFContact )
    {
        rkContact.A = m_apkTetra[iT1];
        rkContact.B = m_apkTetra[iT0];
        CalculateNormal(akVertex1,akRes[1],rkContact);
    }
    else
    {
        rkContact.isVFContact = IsVertex(akVertex1,akRes[1]);
        if ( rkContact.isVFContact )
        {
            rkContact.A = m_apkTetra[iT0];
            rkContact.B = m_apkTetra[iT1];
            CalculateNormal(akVertex0,akRes[0],rkContact);
        }
    }

    // test if the two tetrahedra intersect in an edge-edge configuration
    if ( !rkContact.isVFContact )
    {
        rkContact.A = m_apkTetra[iT0];
        rkContact.B = m_apkTetra[iT1];
        Vector3f kOtherVertexA = Vector3f::UNIT_X;
        Vector3f kOtherVertexB = Vector3f::ZERO;
        rkContact.EA = ClosestEdge(akVertex0,akRes[0],kOtherVertexA);
        rkContact.EB = ClosestEdge(akVertex1,akRes[1],kOtherVertexB);
        Vector3f kNorm = rkContact.EA.UnitCross(rkContact.EB);
        if ( kNorm.Dot(kOtherVertexA-akRes[0]) < 0.0f )
            rkContact.N = kNorm;
        else
            rkContact.N = -kNorm;
    }

    // reposition results to correspond to relocaton of tetra
    rkContact.PA = akRes[0] - kDelta;
    rkContact.PB = akRes[1] + kDelta;
}
//----------------------------------------------------------------------------
bool BouncingTetrahedra::IsVertex (const Vector3f* akVertex,
    const Vector3f& rkRes)
{
    for (int i = 0; i < 4; i++)
    {
        Vector3f kDiff = akVertex[i] - rkRes;
        if ( kDiff.Length() < m_fTolerance )
            return true;
    }
    return false;
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::CalculateNormal (const Vector3f* akVertex,
    const Vector3f& rkRes, Contact& rkContact)
{
    float fDiff = Mathf::MAX_REAL;
    for (int i = 0; i < 4; i++ )
    {
        Plane3f kPlane = Plane3f(akVertex[m_akFaces[i][0]],
            akVertex[m_akFaces[i][1]],akVertex[m_akFaces[i][2]]);
        kPlane.Normalize();
        float fTemp = Mathf::FAbs(kPlane.DistanceTo(rkRes));
        if ( fTemp < fDiff )
        {
            rkContact.N = kPlane.GetNormal();
            fDiff = fTemp;
        }
    }
}
//----------------------------------------------------------------------------
Vector3f BouncingTetrahedra::ClosestEdge (const Vector3f* akVertex,
    const Vector3f& rkRes, Vector3f& rkOtherVertex)
{
    // Find the edge of the tetrahedra nearest to the contact point.
    // If rkOtherVertexB is ZERO, then ClosestEdge skips the calculation of
    // an unneeded other vertex for the tetrahedron B.

    Vector3f kClosestEdge;
    float fMinDist = Mathf::MAX_REAL;
    for (int i = 0; i < 3; i++)
    {
        for (int j = i+1; j < 4; j++)
        {
            Vector3f kEdge = akVertex[j] - akVertex[i];
            Vector3f kDiff = rkRes - akVertex[i];
            float fDdE = kDiff.Dot(kEdge);
            float fELen = kEdge.Length();
            float fDLen = kDiff.Length();
            float fDist = Mathf::FAbs(fDdE/(fELen*fDLen)-1.0f);
            if ( fDist < fMinDist )
            {
                fMinDist = fDist;
                kClosestEdge = kEdge;
                for (int k = 0; rkOtherVertex != Vector3f::ZERO, k < 3; k++)
                {
                    if ( k != i && k != j )
                    {
                        rkOtherVertex = akVertex[k];
                        continue;
                    }
                }
            }
        }
    }
    return kClosestEdge;
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::DoCollisionResponse ()
{
    if ( m_iNumContacts > 0 )
    {
        float* afPreRelVel = new float[m_iNumContacts];
        float* afImpulseMag = new float[m_iNumContacts];

        ComputePreimpulseVelocity(afPreRelVel);
        ComputeImpulseMagnitude(afPreRelVel,afImpulseMag);
        DoImpulse(afImpulseMag);

        delete[] afPreRelVel;
        delete[] afImpulseMag;
    }

    DoMotion();
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::ComputePreimpulseVelocity (float* afPreVel)
{
    for (int i = 0; i < m_iNumContacts; i++)
    {
        const Contact& rkContact = m_kContact[i];
        const RigidBodyf& rkBodyA = *rkContact.A;
        const RigidBodyf& rkBodyB = *rkContact.B;

        Vector3f kXA = rkBodyA.GetPosition();
        Vector3f kXB = rkBodyB.GetPosition();
        Vector3f kVA = rkBodyA.GetLinearVelocity();
        Vector3f kVB = rkBodyB.GetLinearVelocity();
        Vector3f kWA = rkBodyA.GetAngularVelocity();
        Vector3f kWB = rkBodyB.GetAngularVelocity();

        Vector3f kRelA = rkContact.PA - kXA;
        Vector3f kRelB = rkContact.PB - kXB;
        Vector3f kVelA = kVA + kWA.Cross(kRelA);
        Vector3f kVelB = kVB + kWB.Cross(kRelB);
        afPreVel[i] = rkContact.N.Dot(kVelB-kVelA);
    }
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::ComputeImpulseMagnitude (float* afPreRelVel,
    float* afImpulseMag)
{
    // coefficient of restitution
    float fRestitution = 0.8f;
    float fTemp = 20.0f*NUM_TETRA;
    if ( m_fTotalKE < fTemp )
        fRestitution *= 0.5f*m_fTotalKE/fTemp;
    float fCoeff = -(1.0f + fRestitution);

    for (int i = 0; i < m_iNumContacts; i++)
    {
        if ( afPreRelVel[i] < 0.0f )
        {
            const Contact& rkContact = m_kContact[i];
            const RigidBodyf& rkBodyA = *rkContact.A;
            const RigidBodyf& rkBodyB = *rkContact.B;

            Vector3f kVDiff = rkBodyA.GetLinearVelocity() -
                rkBodyB.GetLinearVelocity();
            Vector3f kRelA = rkContact.PA - rkBodyA.GetPosition();
            Vector3f kRelB = rkContact.PB - rkBodyB.GetPosition();
            Vector3f kAxN = kRelA.Cross(rkContact.N);
            Vector3f kBxN = kRelB.Cross(rkContact.N);
            Vector3f kJInvAxN = rkBodyA.GetWorldInverseInertia()*kAxN;
            Vector3f kJInvBxN = rkBodyB.GetWorldInverseInertia()*kBxN;
            float fNumer = fCoeff*(rkContact.N.Dot(kVDiff)
                + rkBodyA.GetAngularVelocity().Dot(kAxN)
                - rkBodyB.GetAngularVelocity().Dot(kBxN));
            float fDenom = rkBodyA.GetInverseMass() + rkBodyB.GetInverseMass()
                + kAxN.Dot(kJInvAxN) + kBxN.Dot(kJInvBxN);
            afImpulseMag[i] = fNumer/fDenom;
        }
        else
        {
            afImpulseMag[i] = 0.0f;
        }
    }
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::DoImpulse (float* afImpulseMag)
{
    for (int i = 0; i < m_iNumContacts; i++)
    {
        Contact& rkContact = m_kContact[i];
        RigidBodyf& rkBodyA = *rkContact.A;
        RigidBodyf& rkBodyB = *rkContact.B;

        Vector3f kPA = rkBodyA.GetLinearMomentum();
        Vector3f kPB = rkBodyB.GetLinearMomentum();
        Vector3f kLA = rkBodyA.GetAngularMomentum();
        Vector3f kLB = rkBodyB.GetAngularMomentum();

        // update linear/angular momentum/velocity
        Vector3f kImpulse = afImpulseMag[i]*rkContact.N;
        kPA += kImpulse;
        kPB -= kImpulse;
        Vector3f kRelA = rkContact.PA - rkBodyA.GetPosition();
        kLA += kRelA.Cross(kImpulse);
        Vector3f kRelB = rkContact.PB - rkBodyB.GetPosition();
        kLB -= kRelB.Cross(kImpulse);

        rkBodyA.SetLinearMomentum(kPA);
        rkBodyB.SetLinearMomentum(kPB);
        rkBodyA.SetAngularMomentum(kLA);
        rkBodyB.SetAngularMomentum(kLB);
    }
}
//----------------------------------------------------------------------------
void BouncingTetrahedra::DoMotion ()
{
    for (int i = 0; i < NUM_TETRA; i++)
        m_apkTetra[i]->Update(m_fSimTime,m_fSimDelta);
}
//----------------------------------------------------------------------------
Vector3f BouncingTetrahedra::Force (float, float fMass, const Vector3f&,
    const Quaternionf&, const Vector3f&, const Vector3f&,
    const Matrix3f&, const Vector3f&, const Vector3f&)
{
    const float fGravityConstant = 9.81f;
    const Vector3f kGravityDirection = -Vector3f::UNIT_Z;
    return (fGravityConstant*fMass)*kGravityDirection;
}
//----------------------------------------------------------------------------
Vector3f BouncingTetrahedra::Torque (float, float, const Vector3f&,
    const Quaternionf&, const Vector3f&, const Vector3f&,
    const Matrix3f&, const Vector3f&, const Vector3f&)
{
    return Vector3f::ZERO;
}
//----------------------------------------------------------------------------
