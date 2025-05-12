// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef NURBSCURVEEXAMPLE_H
#define NURBSCURVEEXAMPLE_H

#include "WmlApplication2.h"
#include "WmlNURBSCurve2.h"

class NURBSCurveExample : public Application2
{
public:
    NURBSCurveExample ();
    virtual ~NURBSCurveExample ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void DoSimulation1 ();
    void DoSimulation2 ();
    void InitialConfiguration ();
    void NextConfiguration ();

    NURBSCurve2f* m_pkSpline;
    NURBSCurve2f* m_pkCircle;
    Vector2f* m_akCtrlPoint;
    Vector2f* m_akTarget;
    float m_fH, m_fD;
    float m_fSimTime, m_fSimDelta;

    bool m_bDrawControlPoints;
};

#endif


