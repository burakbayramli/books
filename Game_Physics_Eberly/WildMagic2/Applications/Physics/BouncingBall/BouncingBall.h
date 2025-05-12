// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef BOUNCINGBALL_H
#define BOUNCINGBALL_H

#include "WmlApplication.h"
#include "DeformableBall.h"
using namespace Wml;

class BouncingBall : public Application
{
public:
    BouncingBall ();
    virtual ~BouncingBall ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void CreateBall ();
    void CreateFloor ();
    void CreateWall ();
    void DoPhysical ();
    void DoVisual ();

    // representation of body
    DeformableBall* m_pkBall;

    // simulated clock
    float m_fSimTime, m_fSimDelta;

    // the scene graph
    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;
    NodePtr m_spkBall;
    TriMeshPtr m_spkFloor;
    TriMeshPtr m_spkWall;
};

#endif
