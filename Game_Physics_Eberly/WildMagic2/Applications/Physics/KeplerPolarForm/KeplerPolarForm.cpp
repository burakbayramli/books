// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "KeplerPolarForm.h"
using namespace std;

const int g_iSize = 256;
KeplerPolarForm g_kTheApp;

//----------------------------------------------------------------------------
KeplerPolarForm::KeplerPolarForm ()
    :
    Application2("KeplerPolarForm",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
}
//----------------------------------------------------------------------------
KeplerPolarForm::~KeplerPolarForm ()
{
}
//----------------------------------------------------------------------------
bool KeplerPolarForm::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // set up the physics module
    m_kModule.Gravity = 10.0;
    m_kModule.Mass = 1.0;

    double dTime = 0.0;
    double dDeltaTime = 0.01;
    double dRadius = 10.0;
    double dDRadius = 0.1;
    double dTheta = 0.25*Mathd::PI;
    double dDTheta = 0.1;
    m_kModule.Initialize(dTime,dDeltaTime,dRadius,dDRadius,dTheta,dDTheta);

    const int iMax = (int)(m_kModule.GetPeriod()/dDeltaTime);
    m_kPosition.resize(iMax);
    for (int i = 0; i < iMax; i++)
    {
        double dX = 0.5*g_iSize + 10.0*dRadius*Mathd::Cos(dTheta);
        double dY = 0.5*g_iSize + 10.0*dRadius*Mathd::Sin(dTheta);
        m_kPosition[i] = Vector2d(dX,dY);

        m_kModule.Update();

        dTime = m_kModule.GetTime();
        dRadius = m_kModule.GetRadius();
        dDRadius = m_kModule.GetDRadius();
        dTheta = m_kModule.GetTheta();
        dDTheta = m_kModule.GetDTheta();
    }

    // All drawing is in flipped y-values to show the objects in right-handed
    // coordinates.
    DoFlip(true);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void KeplerPolarForm::OnTerminate ()
{
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void KeplerPolarForm::OnDisplay ()
{
    ClearScreen();

    const int iHSize = g_iSize/2, iSizeM1 = g_iSize-1;

    // Draw the coordinate axes.
    Color kGray(192,192,192);
    DrawLine(0,iHSize,iSizeM1,iHSize,kGray);
    DrawLine(iHSize,0,iHSize,iSizeM1,kGray);

    // Draw a ray from the Sun's location to the initial point.
    int iX = (int)(m_kPosition[1].X() + 0.5);
    int iY = (int)(m_kPosition[1].Y() + 0.5);
    DrawLine(iHSize,iHSize,iX,iY,kGray);

    // Draw the Sun's location.  The Sun is at the origin which happens to
    // be a focal point of the ellipse.
    const int iThick = 1;
    Color kRed(255,0,0);
    for (int iDY = -iThick; iDY <= iThick; iDY++)
    {
        for (int iDX = -iThick; iDX <= iThick; iDX++)
            SetPixel(iHSize+iDX,iHSize+iDY,kRed);
    }

    // Draw Earth's orbit.  The orbit starts in green, finishes in blue, and
    // is a blend of the two colors between.
    int iPSize = (int)m_kPosition.size();
    float fInvPSize = 1.0f/iPSize;
    for (int i = 1; i < iPSize; i++)
    {
        float fW = i*fInvPSize, fOmW = 1.0f - fW;
        unsigned char ucB = (unsigned char)(255.0f*fOmW);
        unsigned char ucG = (unsigned char)(255.0f*fW);
        iX = (int)(m_kPosition[i].X() + 0.5);
        iY = (int)(m_kPosition[i].Y() + 0.5);
        SetPixel(iX,iY,Color(0,ucG,ucB));
    }

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void KeplerPolarForm::OnKeyDown (unsigned char ucKey, int iX, int iY)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
