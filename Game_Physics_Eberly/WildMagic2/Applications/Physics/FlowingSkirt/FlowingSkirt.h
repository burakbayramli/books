// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef FLOWINGSKIRT_H
#define FLOWINGSKIRT_H

#include "WmlApplication.h"
#include "WmlBSplineCurve3.h"
using namespace Wml;

class FlowingSkirt : public Application
{
public:
    FlowingSkirt ();
    virtual ~FlowingSkirt ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void CreateSkirt ();
    void UpdateSkirt ();
    void ModifyCurves ();

    // the scene graph
    NodePtr m_spkScene, m_spkTrnNode;
    TriMeshPtr m_spkSkirt;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;

    // the skirt is a generalized Bezier cylinder
    int m_iNumCtrl, m_iDegree;
    float m_fATop, m_fBTop, m_fABot, m_fBBot;
    BSplineCurve3f* m_pkSkirtTop;
    BSplineCurve3f* m_pkSkirtBot;
    float* m_afFrequency;
};

#endif
