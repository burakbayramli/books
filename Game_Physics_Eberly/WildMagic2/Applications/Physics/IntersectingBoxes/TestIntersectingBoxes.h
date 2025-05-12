// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef TESTINTERSECTINGBOXES_H
#define TESTINTERSECTINGBOXES_H

#include "WmlApplication.h"
#include "WmlIntersectingBoxes.h"
using namespace Wml;

class TestIntersectingBoxes : public Application
{
public:
    TestIntersectingBoxes ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void ModifyBoxes ();
    void ModifyMesh (int i);

    std::vector<AxisAlignedBox3f> m_kBoxes;
    IntersectingBoxesf* m_pkIB;
    bool m_bDoSimulation;
    float m_fLastIdle;

    NodePtr m_spkScene;
    MaterialStatePtr m_spkBlue, m_spkRed;
    WireframeStatePtr m_spkWireframe;

    static float ms_fSize;
};

#endif
