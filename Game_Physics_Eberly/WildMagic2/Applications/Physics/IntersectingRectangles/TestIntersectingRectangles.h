// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef TESTINTERSECTINGRECTANGLES_H
#define TESTINTERSECTINGRECTANGLES_H

#include "WmlApplication2.h"
#include "WmlIntersectingRectangles.h"

class TestIntersectingRectangles : public Application2
{
public:
    TestIntersectingRectangles ();
    virtual ~TestIntersectingRectangles ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void ModifyRectangles ();
    void DrawRectangles ();

    std::vector<AxisAlignedBox2f> m_kRects;
    IntersectingRectanglesf* m_pkIR;
    bool m_bMouseDown;
    float m_fLastIdle;
};

#endif
