// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "IntersectCircleBox.h"

const int g_iSize = 256;
IntersectCircleBox g_kTheApp;

//----------------------------------------------------------------------------
IntersectCircleBox::IntersectCircleBox ()
    :
    Application2("IntersectCircleBox",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
    m_bMouseDown = false;
}
//----------------------------------------------------------------------------
IntersectCircleBox::~IntersectCircleBox ()
{
}
//----------------------------------------------------------------------------
bool IntersectCircleBox::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    m_kBox.Center() = Vector2f(150.0f,125.0f);
    m_kBox.Axis(0) = Vector2f::UNIT_X;
    m_kBox.Axis(1) = Vector2f::UNIT_Y;
    m_kBox.Extent(0) = 50.0f;
    m_kBox.Extent(1) = 25.0f;

    m_kCircle.Center().X() = 20.0f;
    m_kCircle.Center().Y() = 135.0f;
    m_kCircle.Radius() = 10.0f;

    m_kVelocity.X() = 1.0f;
    m_kVelocity.Y() = 0.0f;

    m_iType = FindIntersection(m_kCircle,m_kVelocity,m_kBox,m_fTFirst,
        m_kIntr);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void IntersectCircleBox::OnTerminate ()
{
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void IntersectCircleBox::OnDisplay ()
{
    ClearScreen();

    DrawCircle();
    DrawRectangle();
    if ( m_iType == 1 )
        DrawIntersection();

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void IntersectCircleBox::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
void IntersectCircleBox::OnMouseClick (int iButton, int iState, int iX,
    int iY, unsigned int)
{
    if ( iButton != MOUSE_LEFT_BUTTON )
        return;

    if ( iState == MOUSE_DOWN )
    {
        m_bMouseDown = true;

        float fDx = iX - m_kCircle.Center().X();
        float fDy = iY - m_kCircle.Center().Y();

        if ( fDx*fDx +fDy*fDy <= m_kCircle.Radius()*m_kCircle.Radius() )
        {
            // mouse click inside circle, move circle
            m_kCircle.Center().X() = (float)iX;
            m_kCircle.Center().Y() = (float)iY;
        }
        else
        {
            // mouse click outside circle, change velocity
            m_kVelocity.X() = fDx;
            m_kVelocity.Y() = fDy;
            m_kVelocity.Normalize();
        }

        m_iType = FindIntersection(m_kCircle,m_kVelocity,m_kBox,m_fTFirst,
            m_kIntr);
        OnDisplay();
    }
    else if ( iState == MOUSE_UP )
    {
        m_bMouseDown = false;
    }
}
//----------------------------------------------------------------------------
void IntersectCircleBox::OnMotion (int iX, int iY, unsigned int)
{
    if ( m_bMouseDown )
    {
        float fDx = iX - m_kCircle.Center().X();
        float fDy = iY - m_kCircle.Center().Y();

        if ( fDx*fDx +fDy*fDy <= m_kCircle.Radius()*m_kCircle.Radius() )
        {
            // mouse click inside circle, move circle
            m_kCircle.Center().X() = (float)iX;
            m_kCircle.Center().Y() = (float)iY;
        }
        else
        {
            // mouse click outside circle, change velocity
            m_kVelocity.X() = fDx;
            m_kVelocity.Y() = fDy;
            m_kVelocity.Normalize();
        }

        m_iType = FindIntersection(m_kCircle,m_kVelocity,m_kBox,m_fTFirst,
            m_kIntr);
        OnDisplay();
    }
}
//----------------------------------------------------------------------------
void IntersectCircleBox::DrawCircle ()
{
    // draw circle
    int iXC = (int)m_kCircle.Center().X();
    int iYC = (int)m_kCircle.Center().Y();
    int iR = (int)m_kCircle.Radius();
    Application2::DrawCircle(iXC,iYC,iR,Color(0,0,255));

    // draw rays of motion
    float fT = (float)g_iSize;
    Vector2f kVPerp = m_kVelocity.Perp();

    Vector2f kStart = m_kCircle.Center();
    Vector2f kFinal = m_kCircle.Center() + fT*m_kVelocity;
    int iX0 = (int)kStart.X();
    int iY0 = (int)kStart.Y();
    int iX1 = (int)kFinal.X();
    int iY1 = (int)kFinal.Y();
    DrawLine(iX0,iY0,iX1,iY1,Color(192,0,0));

    kStart = m_kCircle.Center() + m_kCircle.Radius()*kVPerp;
    kFinal = kStart + fT*m_kVelocity;
    iX0 = (int)kStart.X();
    iY0 = (int)kStart.Y();
    iX1 = (int)kFinal.X();
    iY1 = (int)kFinal.Y();
    DrawLine(iX0,iY0,iX1,iY1,Color(192,0,0));

    kStart = m_kCircle.Center() - m_kCircle.Radius()*kVPerp;
    kFinal = kStart + fT*m_kVelocity;
    iX0 = (int)kStart.X();
    iY0 = (int)kStart.Y();
    iX1 = (int)kFinal.X();
    iY1 = (int)kFinal.Y();
    DrawLine(iX0,iY0,iX1,iY1,Color(192,0,0));
}
//----------------------------------------------------------------------------
void IntersectCircleBox::DrawRectangle ()
{
    int iXMin = (int)(m_kBox.Center().X() - m_kBox.Extent(0));
    int iXMax = (int)(m_kBox.Center().X() + m_kBox.Extent(0));
    int iYMin = (int)(m_kBox.Center().Y() - m_kBox.Extent(1));
    int iYMax = (int)(m_kBox.Center().Y() + m_kBox.Extent(1));

    DrawLine(iXMin,0,iXMin,g_iSize-1,Color(192,192,192));
    DrawLine(iXMax,0,iXMax,g_iSize-1,Color(192,192,192));
    DrawLine(0,iYMin,g_iSize-1,iYMin,Color(192,192,192));
    DrawLine(0,iYMax,g_iSize-1,iYMax,Color(192,192,192));

    Application2::DrawRectangle(iXMin,iYMin,iXMax,iYMax,Color(0,0,0));
}
//----------------------------------------------------------------------------
void IntersectCircleBox::DrawIntersection ()
{
    Vector2f kMoved = m_kCircle.Center() + m_fTFirst*m_kVelocity;
    int iXC = (int)kMoved.X();
    int iYC = (int)kMoved.Y();
    int iR = (int)m_kCircle.Radius();
    Application2::DrawCircle(iXC,iYC,iR,Color(255,0,255));

    int iX = (int)m_kIntr.X();
    int iY = (int)m_kIntr.Y();
    SetPixel(iX-1,iY-1,Color(0,255,0));
    SetPixel(iX-1,iY  ,Color(0,255,0));
    SetPixel(iX-1,iY+1,Color(0,255,0));
    SetPixel(iX  ,iY-1,Color(0,255,0));
    SetPixel(iX  ,iY  ,Color(0,255,0));
    SetPixel(iX  ,iY+1,Color(0,255,0));
    SetPixel(iX+1,iY-1,Color(0,255,0));
    SetPixel(iX+1,iY  ,Color(0,255,0));
    SetPixel(iX+1,iY+1,Color(0,255,0));
}
//----------------------------------------------------------------------------
