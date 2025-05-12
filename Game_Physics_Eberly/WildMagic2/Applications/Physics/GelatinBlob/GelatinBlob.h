// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef GELATINBLOB_H
#define GELATINBLOB_H

#include "WmlApplication.h"
#include "WmlBSplineVolume.h"
#include "WmlBoxSurface.h"
#include "PhysicsModule.h"
using namespace Wml;


class GelatinBlob : public Application
{
public:
    GelatinBlob ();
    virtual ~GelatinBlob ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    // a mass-spring system
    PhysicsModule* m_pkModule;
    void DoPhysical ();

    // scene graph
    void CreateSphere ();
    void CreateSprings ();
    void CreateSegments ();
    void CreateScene ();
    NodePtr m_spkScene, m_spkTrnNode, m_spkSegments;
    WireframeStatePtr m_spkWireframe;
    TriMeshPtr m_spkSphere;
};

#endif
