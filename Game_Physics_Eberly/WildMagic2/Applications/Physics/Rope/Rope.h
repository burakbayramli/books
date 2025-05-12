// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef ROPE_H
#define ROPE_H

#include "WmlApplication.h"
#include "WmlBSplineCurve3.h"
#include "WmlTubeSurface.h"
#include "PhysicsModule.h"
using namespace Wml;


class Rope : public Application
{
public:
    Rope ();
    virtual ~Rope ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    // The masses are located at the control points of a spline curve.  The
    // control points are connected by a mass-spring system.
    BSplineCurve3f* m_pkSpline;
    PhysicsModule* m_pkModule;
    void DoPhysical ();

    // scene graph
    void CreateSprings ();
    void CreateRope ();
    void CreateScene ();
    static float Radial (float) { return 0.025f; }
    NodePtr m_spkScene, m_spkTrnNode;
    WireframeStatePtr m_spkWireframe;
    TubeSurfacePtr m_spkRope;

    // controlled frame rate
    float m_fLastIdle;
};

#endif


