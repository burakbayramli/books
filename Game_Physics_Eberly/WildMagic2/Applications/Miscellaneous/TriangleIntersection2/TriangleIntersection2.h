// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef TRIANGLEINTERSECTION2_H
#define TRIANGLEINTERSECTION2_H

#include "WmlApplication2.h"

class TriangleIntersection2 : public Application2
{
public:
    TriangleIntersection2 ();
    virtual ~TriangleIntersection2 ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void ScreenOverlay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);
    virtual void OnMotion (int iX, int iY, unsigned int uiModifiers);

protected:
    Vector2f GetVelocity (unsigned int uiSpeed, unsigned int uiAngle);
    void GetIntersecting ();

    int Flip (int iY)  { return GetHeight() - 1 - iY; }
    void DrawTriangle (int aiX[3], int aiY[3], const Color& rkColor);
    void DrawMovedTriangle (int aiX[3], int aiY[3], unsigned int uiSpeed,
        unsigned int uiAngle, const Color& rkColor);
    void DrawIntersection ();

    bool MouseNearVertex (int iXPos, int iYPos, int iXVer, int iYVer);
    bool MouseInTriangle (int iXPos, int iYPos, int aiX[3], int aiY[3]);
    void AdjustTriangle (int iXPos, int iYPos);

    bool m_bMouseDown, m_bIntersecting;
    int m_iTriangle, m_iSelect, m_iXMouseStart, m_iYMouseStart;

    enum Type { TT_TEST, TT_FIND, TT_TEST_VEL, TT_FIND_VEL, TT_MAX };
    Type m_eType;

    // two triangles
    int m_aiX0[3], m_aiY0[3], m_aiX1[3], m_aiY1[3];

    // velocity information
    enum { MAX_SPEED = 64, MAX_ANGLE = 32 };
    unsigned int m_uiSpeed0, m_uiSpeed1;
    unsigned int m_uiAngle0, m_uiAngle1;
    float m_fTFirst;

    // intersection of triangles
    int m_iQuantity;
    Vector2f m_akVertex[6];

    // text display
    char m_acHeader[64], m_acFooter[256];

    static Color BLACK, GRAY, RED, BLUE, PURPLE, LIGHT_RED, LIGHT_BLUE,
        LIGHT_PURPLE;
};

#endif



