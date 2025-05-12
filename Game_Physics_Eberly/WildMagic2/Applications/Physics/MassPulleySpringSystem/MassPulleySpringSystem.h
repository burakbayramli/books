// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef MASSPULLEYSPRINGSYSTEM_H
#define MASSPULLEYSPRINGSYSTEM_H

#include "WmlApplication.h"
#include "WmlBSplineCurve3.h"
#include "WmlTubeSurface.h"
#include "PhysicsModule.h"
using namespace Wml;

class MassPulleySpringSystem : public Application
{
public:
    MassPulleySpringSystem ();
    virtual ~MassPulleySpringSystem ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void CreateScene ();
    TriMesh* CreateFloor ();
    TubeSurface* CreateCable ();
    TriMesh* CreateMass (float fRadius);
    void CreatePulley ();
    void CreateSpring ();
    TubeSurface* CreateHelix ();

    void DoPhysical ();
    void UpdatePulley ();
    void UpdateCable ();
    void UpdateHelix ();

    // root of scene and floor mesh
    NodePtr m_spkScene;
    TriMeshPtr m_spkFloor;
    WireframeStatePtr m_spkWireframe;

    // assembly to parent the cable root and pulley root
    NodePtr m_spkAssembly;

    // cable modeled as a tube surface, masses attached to ends
    NodePtr m_spkCableRoot;
    BSplineCurve3f* m_pkCableSpline;
    TubeSurfacePtr m_spkCable;
    static float CableRadial (float) { return 0.5f; }
    TriMeshPtr m_spkMass1, m_spkMass2;

    // node to parent the pulley and spring
    NodePtr m_spkPulleyRoot;

    // pulley modeled as a disk with thickness
    NodePtr m_spkPulley;
    TriMeshPtr m_spkPlate0, m_spkPlate1, m_spkCylinder;
    TextureStatePtr m_spkTSMetal;

    // Spring modeled as a tube surface in the shape of a helix, then attached
    // to a U-bracket to hold the pulley disk.
    NodePtr m_spkSpring;
    TriMeshPtr m_spkSide0, m_spkSide1, m_spkTop;
    BSplineCurve3f* m_pkHelixSpline;
    TubeSurfacePtr m_spkHelix;
    static float HelixRadial (float) { return 0.25f; }

    PhysicsModule m_kModule;

    // controlled frame rate
    float m_fLastIdle;
};

#endif
