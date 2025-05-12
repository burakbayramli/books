// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef INTERSECTCIRCLEBOX_H
#define INTERSECTCIRCLEBOX_H

#include "WmlApplication2.h"
#include "WmlIntrCir2Box2.h"

class IntersectCircleBox : public Application2
{
public:
    IntersectCircleBox ();
    virtual ~IntersectCircleBox ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);
    virtual void OnMotion (int iX, int iY, unsigned int uiModifiers);

protected:
    void DrawCircle ();
    void DrawRectangle ();
    void DrawIntersection ();

    // moving circle
    Circle2f m_kCircle;
    Vector2f m_kVelocity;

    // rectangle
    Box2f m_kBox;

    // intersection
    int m_iType;
    float m_fTFirst;
    Vector2f m_kIntr;

    bool m_bMouseDown;
};

#endif
