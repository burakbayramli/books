// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "DrawImplicitSurface.h"
#include "Function.h"
#include "WmlMatrix3.h"

const int g_iSize = 256;
DrawImplicitSurface g_kTheApp;

//----------------------------------------------------------------------------
DrawImplicitSurface::DrawImplicitSurface ()
    :
    Application2("DrawImplicitSurface",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f)),
    m_kRT(F,DF,g_iSize,g_iSize)
{
    m_iMaxSample = 100;
    m_bBlur = false;
}
//----------------------------------------------------------------------------
DrawImplicitSurface::~DrawImplicitSurface ()
{
}
//----------------------------------------------------------------------------
bool DrawImplicitSurface::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // initialize camera and view frustum
    m_kRT.Location() = Vector3f(2.0f,0.0f,0.0f);
    m_kRT.Direction() = Vector3f(-1.0f,0.0f,0.0f);
    m_kRT.Up() = Vector3f(0.0f,1.0f,0.0f);
    m_kRT.Right() = m_kRT.Direction().Cross(m_kRT.Up());
    m_kRT.Near() = 0.1f;
    m_kRT.Far() = 10.0f;
    m_kRT.HalfWidth() = 2.0f*m_kRT.Near();  // 90 degree horizontal FOV
    m_kRT.HalfHeight() = 2.0f*m_kRT.Near();  // 90 degree vertical FOV

    // The light direction will be the camera direction so that we can see
    // the surface from all camera locations.

    // draw level surface
    m_kRT.DrawSurface(m_iMaxSample,m_kRT.Direction(),m_bBlur);
    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void DrawImplicitSurface::OnTerminate ()
{
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void DrawImplicitSurface::OnDisplay ()
{
    ClearScreen();

    const float* afImage = m_kRT.GetImage();
    for (int iY = 0, i = 0; iY < m_kRT.GetHeight(); iY++)
    {
        for (int iX = 0; iX < m_kRT.GetWidth(); iX++)
        {
            int iValue = (int)(255.0f*afImage[i++]);
            SetPixel(iX,iY,Color(iValue,iValue,iValue));
        }
    }

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void DrawImplicitSurface::OnSpecialKeyDown (int iKey, int, int)
{
    // TO DO:  These are chosen for the specific functions in this
    // application.  Allow the application to modify these, either by key
    // strokes or automatically.
    const float fTrnDelta = 0.25f, fRotDelta = 0.1f;

    float fLength;
    Matrix3f kRot;

    bool bMoved = false;

    if ( iKey == KEY_UP_ARROW )
    {
        // translate forward in camera direction
        m_kRT.Location() += fTrnDelta*m_kRT.Direction();
        bMoved = true;
    }
    else if ( iKey == KEY_DOWN_ARROW )
    {
        // translate backward in camera direction
        m_kRT.Location() -= fTrnDelta*m_kRT.Direction();
        bMoved = true;
    }
    else if ( iKey == KEY_F1 )
    {
        // rotate about camera right, move up on view sphere
        fLength = m_kRT.Location().Length();
        kRot.FromAxisAngle(m_kRT.Right(),fRotDelta);
        m_kRT.Direction() = kRot*m_kRT.Direction();
        m_kRT.Up() = kRot*m_kRT.Up();
        m_kRT.Location() = -fLength*m_kRT.Direction();
        bMoved = true;
    }
    else if ( iKey == KEY_F2 )
    {
        // rotate about camera right, move down on view sphere
        fLength = m_kRT.Location().Length();
        kRot.FromAxisAngle(m_kRT.Right(),-fRotDelta);
        m_kRT.Direction() = kRot*m_kRT.Direction();
        m_kRT.Up() = kRot*m_kRT.Up();
        m_kRT.Location() = -fLength*m_kRT.Direction();
        bMoved = true;
    }
    else if ( iKey == KEY_F3 )
    {
        // rotate about camera up, move right on view sphere
        fLength = m_kRT.Location().Length();
        kRot.FromAxisAngle(m_kRT.Up(),fRotDelta);
        m_kRT.Direction() = kRot*m_kRT.Direction();
        m_kRT.Right() = kRot*m_kRT.Right();
        m_kRT.Location() = -fLength*m_kRT.Direction();
        bMoved = true;
    }
    else if ( iKey == KEY_F4 )
    {
        // rotate about camera up, move left on view sphere
        fLength = m_kRT.Location().Length();
        kRot.FromAxisAngle(m_kRT.Up(),-fRotDelta);
        m_kRT.Direction() = kRot*m_kRT.Direction();
        m_kRT.Right() = kRot*m_kRT.Right();
        m_kRT.Location() = -fLength*m_kRT.Direction();
        bMoved = true;
    }
    else if ( iKey == KEY_F5 )
    {
        // rotate about camera direction, roll counterclockwise
        fLength = m_kRT.Location().Length();
        kRot.FromAxisAngle(m_kRT.Direction(),fRotDelta);
        m_kRT.Up() = kRot*m_kRT.Up();
        m_kRT.Right() = kRot*m_kRT.Right();
        bMoved = true;
    }
    else if ( iKey == KEY_F6 )
    {
        // rotate about camera direction, roll clockwise
        fLength = m_kRT.Location().Length();
        kRot.FromAxisAngle(m_kRT.Direction(),-fRotDelta);
        m_kRT.Up() = kRot*m_kRT.Up();
        m_kRT.Right() = kRot*m_kRT.Right();
        bMoved = true;
    }

    if ( bMoved )
    {
        m_kRT.DrawSurface(m_iMaxSample,m_kRT.Direction(),m_bBlur);
        OnDisplay();
    }
}
//----------------------------------------------------------------------------
void DrawImplicitSurface::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case '+':  // increase ray sample size
    case '=':
        m_iMaxSample += 100;
        m_kRT.DrawSurface(m_iMaxSample,m_kRT.Direction(),m_bBlur);
        OnDisplay();
        break;
    case '-':  // decrease ray sample size
    case '_':
        if ( m_iMaxSample > 100 )
        {
            m_iMaxSample -= 100;
            m_kRT.DrawSurface(m_iMaxSample,m_kRT.Direction(),m_bBlur);
            OnDisplay();
        }
        break;
    case 'b':  // toggle blur of output image
    case 'B':
        m_bBlur = !m_bBlur;
        m_kRT.DrawSurface(m_iMaxSample,m_kRT.Direction(),m_bBlur);
        OnDisplay();
        break;
    }
}
//----------------------------------------------------------------------------
