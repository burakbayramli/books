// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef WATERDROPFORMATION_H
#define WATERDROPFORMATION_H

#include "WmlApplication.h"
#include "WmlNURBSCurve2.h"
#include "WmlRevolutionSurface.h"
using namespace Wml;

class WaterDropFormation : public Application
{
public:
    WaterDropFormation ();
    virtual ~WaterDropFormation ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    // scene graph
    void CreatePlane ();
    void CreateWall ();
    void CreateWaterRoot ();

    NodePtr m_spkScene, m_spkTrnNode, m_spkWaterRoot;
    WireframeStatePtr m_spkWireframe;
    TriMeshPtr m_spkPlane, m_spkWall;
    RevolutionSurfacePtr m_spkWaterSurface, m_spkWaterDrop;

    // water sphere curves and simulation parameters
    void Configuration0 ();  // water surface
    void Configuration1 ();  // split into water surface and water drop
    void DoPhysical1 ();
    void DoPhysical2 ();
    void DoPhysical3 ();
    void DoPhysical ();
    void DoVisual ();

    NURBSCurve2f* m_pkSpline;
    NURBSCurve2f* m_pkCircle;
    Vector2f* m_akCtrlPoint;
    Vector2f* m_akTarget;
    float m_fSimTime, m_fSimDelta, m_fLastSeconds;
};

#endif

