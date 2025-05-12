// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef BSPLINECURVEEXAMPLES_H
#define BSPLINECURVEEXAMPLES_H

#include "WmlApplication2.h"
#include "WmlBSplineCurve2.h"

class BSplineCurveExamples : public Application2
{
public:
    BSplineCurveExamples ();
    virtual ~BSplineCurveExamples ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    BSplineCurve2f* m_pkSpline;
    int m_iNumCtrlPoints, m_iDegree;
    Vector2f* m_akCtrlPoint;
    float* m_afKnot;
    float m_afLocCtrlMin[6], m_afLocCtrlMax[6];
    int m_iCurveType;
    bool m_bModified;
};

#endif


