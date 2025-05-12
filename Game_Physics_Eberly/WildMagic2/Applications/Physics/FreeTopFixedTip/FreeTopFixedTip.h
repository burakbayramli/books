// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef FREETOPFIXEDTIP_H
#define FREETOPFIXEDTIP_H

#include "WmlApplication.h"
#include "PhysicsModule.h"
using namespace Wml;

class FreeTopFixedTip : public Application
{
public:
    FreeTopFixedTip ();
    virtual ~FreeTopFixedTip ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    // scene graph
    void CreateScene ();
    TriMesh* CreateFloor ();
    TriMesh* CreateTop ();
    Polyline* CreateAxisTop ();
    Polyline* CreateAxisVertical ();

    NodePtr m_spkScene, m_spkTopRoot;
    WireframeStatePtr m_spkWireframe;
    TriMeshPtr m_spkTop, m_spkFloor;
    PolylinePtr m_spkAxisTop, m_spkAxisVertical;
    float m_fMaxPhi;

    // physics
    void DoPhysical ();
    PhysicsModule m_kModule;

    // controlled frame rate
    float m_fLastIdle;
};

#endif
