// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef CLOTH_H
#define CLOTH_H

#include "WmlApplication.h"
#include "WmlBSplineRectangle.h"
#include "WmlRectangleSurface.h"
#include "PhysicsModule.h"
using namespace Wml;


class Cloth : public Application
{
public:
    Cloth ();
    virtual ~Cloth ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    // masses are located at the control points of a spline surface
    BSplineRectanglef* m_pkSpline;

    // a mass-spring system
    PhysicsModule* m_pkModule;
    void DoPhysical ();

    // scene graph
    void CreateSprings ();
    void CreateCloth ();
    void CreateScene ();
    NodePtr m_spkScene, m_spkTrnNode;
    WireframeStatePtr m_spkWireframe;
    RectangleSurfacePtr m_spkCloth;
};

#endif
