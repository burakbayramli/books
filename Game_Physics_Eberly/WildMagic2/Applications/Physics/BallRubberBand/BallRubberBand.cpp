// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "BallRubberBand.h"
using namespace std;

const int g_iSize = 256;
BallRubberBand g_kTheApp;

//----------------------------------------------------------------------------
BallRubberBand::BallRubberBand ()
    :
    Application2("BallRubberBand",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
}
//----------------------------------------------------------------------------
BallRubberBand::~BallRubberBand ()
{
}
//----------------------------------------------------------------------------
bool BallRubberBand::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // set up the physics module
    m_kModule.SpringConstant = 16.0;
    m_kModule.Mass = 1.0;

    double dTime = 0.0;
    double dDeltaTime = 0.01;
    Vector2d kPosition(96.0,96.0);
    Vector2d kVelocity(64.0,0.0);
    m_kModule.Initialize(dTime,dDeltaTime,kPosition,kVelocity);

    const int iMax = 128;
    m_kPosition.resize(iMax);
    for (int i = 0; i < iMax; i++)
    {
        m_kPosition[i] = m_kModule.GetPosition();
        m_kModule.Update();
    }

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void BallRubberBand::OnTerminate ()
{
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void BallRubberBand::OnDisplay ()
{
    ClearScreen();

    const int iHSize = g_iSize/2, iSizeM1 = g_iSize-1;
    const float fHSize = (float)iHSize;

    // Draw the coordinate axes.
    Color kGray(192,192,192);
    DrawLine(0,iHSize,iSizeM1,iHSize,kGray);
    DrawLine(iHSize,0,iHSize,iSizeM1,kGray);

    // Draw the ball's path.  The orbit starts in green, finishes in blue,
    // and is a blend of the two colors between.
    int iPSize = (int)m_kPosition.size();
    float fInvPSize = 1.0f/iPSize;
    for (int i = 0; i < iPSize-1; i++)
    {
        float fW = i*fInvPSize, fOmW = 1.0f - fW;
        unsigned char ucB = (unsigned char)(255.0f*fOmW);
        unsigned char ucG = (unsigned char)(255.0f*fW);
        int iX0 = (int)(m_kPosition[i].X() + fHSize + 0.5);
        int iY0 = (int)(m_kPosition[i].Y() + fHSize + 0.5);
        int iX1 = (int)(m_kPosition[i+1].X() + fHSize + 0.5);
        int iY1 = (int)(m_kPosition[i+1].Y() + fHSize + 0.5);
        DrawLine(iX0,iY0,iX1,iY1,Color(0,ucG,ucB));
    }

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void BallRubberBand::OnKeyDown (unsigned char ucKey, int iX, int iY)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
