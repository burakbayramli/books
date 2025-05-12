// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef FOUCAULTPENDULUM_H
#define FOUCAULTPENDULUM_H

#include "WmlApplication.h"
#include "PhysicsModule.h"
using namespace Wml;

class FoucaultPendulum : public Application
{
public:
    FoucaultPendulum ();
    virtual ~FoucaultPendulum ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    TriMesh* CreateFloor ();
    Polypoint* CreatePath ();
    Node* CreatePendulum ();
    void DoPhysical ();

    // the scene graph
    NodePtr m_spkScene, m_spkPendulum;
    WireframeStatePtr m_spkWireframe;
    PolypointPtr m_spkPath;
    int m_iNextPoint;
    float m_fColorDiff;

    // physics
    PhysicsModule m_kModule;
};

#endif
