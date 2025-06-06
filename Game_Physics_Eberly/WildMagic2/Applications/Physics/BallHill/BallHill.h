// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef BALLHILL_H
#define BALLHILL_H

#include "WmlApplication.h"
#include "PhysicsModule.h"
using namespace Wml;

class BallHill : public Application
{
public:
    BallHill ();
    virtual ~BallHill ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    TriMesh* CreateGround ();
    TriMesh* CreateHill ();
    TriMesh* CreateBall ();
    Polyline* CreatePath ();
    void DoPhysical ();
    Vector3f UpdateBall ();

    // scene graph
    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframe;
    TriMeshPtr m_spkGround, m_spkHill, m_spkBall;
    PolylinePtr m_spkPath;
    float m_fRadius;
    int m_iNextPoint;

    // physics
    PhysicsModule m_kModule;
};

#endif
