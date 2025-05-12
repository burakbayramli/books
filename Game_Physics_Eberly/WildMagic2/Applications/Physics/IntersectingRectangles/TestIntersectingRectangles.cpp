// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "TestIntersectingRectangles.h"
using namespace std;

const int g_iSize = 256;
TestIntersectingRectangles g_kTheApp;

// #define SINGLE_STEP

//----------------------------------------------------------------------------
TestIntersectingRectangles::TestIntersectingRectangles ()
    :
    Application2("IntersectingRectangles",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
    m_bMouseDown = false;
    m_pkIR = NULL;
    m_fLastIdle = 0.0f;
}
//----------------------------------------------------------------------------
TestIntersectingRectangles::~TestIntersectingRectangles ()
{
}
//----------------------------------------------------------------------------
bool TestIntersectingRectangles::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    for (int i = 0; i < 16; i++)
    {
        float fXMin = Mathf::IntervalRandom(0.125f*g_iSize,0.875f*g_iSize);
        float fXMax = fXMin + Mathf::IntervalRandom(4.0f,32.0f);
        float fYMin = Mathf::IntervalRandom(0.125f*g_iSize,0.875f*g_iSize);
        float fYMax = fYMin + Mathf::IntervalRandom(4.0f,32.0f);
        m_kRects.push_back(AxisAlignedBox2f(fXMin,fXMax,fYMin,fYMax));
    }

    m_pkIR = new IntersectingRectanglesf(m_kRects);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void TestIntersectingRectangles::OnTerminate ()
{
    delete m_pkIR;
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void TestIntersectingRectangles::OnIdle ()
{
#ifndef SINGLE_STEP
    float fCurrIdle = GetTimeInSeconds();
    float fDiff = fCurrIdle - m_fLastIdle;
    if ( fDiff >= 1.0f/30.0f )
    {
        ModifyRectangles();
        OnDisplay();
        m_fLastIdle = fCurrIdle;
    }
#endif
}
//----------------------------------------------------------------------------
void TestIntersectingRectangles::OnDisplay ()
{
    ClearScreen();
    DrawRectangles();
    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void TestIntersectingRectangles::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

#ifdef SINGLE_STEP
    switch ( ucKey )
    {
    case 'g':
    case 'G':
        ModifyRectangles();
        OnDisplay();
        break;
    }
#endif
}
//----------------------------------------------------------------------------
void TestIntersectingRectangles::ModifyRectangles ()
{
    for (int i = 0; i < (int)m_kRects.size(); i++)
    {
        AxisAlignedBox2f kRect = m_kRects[i];

        float fDX = Mathf::IntervalRandom(-4.0f,4.0f);
        if ( kRect.XMin()+fDX >= 0.0f && kRect.XMax()+fDX < g_iSize )
        {
            kRect.XMin() += fDX;
            kRect.XMax() += fDX;
        }

        float fDY = Mathf::IntervalRandom(-4.0f,4.0f);
        if ( kRect.YMin()+fDY >= 0.0f && kRect.YMax()+fDY < g_iSize )
        {
            kRect.YMin() += fDY;
            kRect.YMax() += fDY;
        }

        m_pkIR->SetRectangle(i,kRect);
    }

    m_pkIR->Update();
}
//----------------------------------------------------------------------------
void TestIntersectingRectangles::DrawRectangles ()
{
    Color kGray(192,192,192), kBlack(0,0,0), kRed(255,0,0);
    int i, iXMin, iXMax, iYMin, iYMax;
    for (i = 0; i < (int)m_kRects.size(); i++)
    {
        const AxisAlignedBox2f& rkRect = m_kRects[i];
        iXMin = (int)rkRect.GetXMin();
        iXMax = (int)rkRect.GetXMax();
        iYMin = (int)rkRect.GetYMin();
        iYMax = (int)rkRect.GetYMax();
        DrawRectangle(iXMin,iYMin,iXMax,iYMax,kGray,true);
        DrawRectangle(iXMin,iYMin,iXMax,iYMax,kBlack);
    }

    const set<pair<int,int> >& rkOverlap = m_pkIR->GetOverlap();
    set<pair<int,int> >::const_iterator pkIter = rkOverlap.begin();
    for (/**/; pkIter != rkOverlap.end(); pkIter++)
    {
        int i0 = pkIter->first;
        int i1 = pkIter->second;
        const AxisAlignedBox2f& rkR0 = m_kRects[i0];
        const AxisAlignedBox2f& rkR1 = m_kRects[i1];
        AxisAlignedBox2f kIntr;
        if ( rkR0.FindIntersection(rkR1,kIntr) )
        {
            iXMin = (int)kIntr.GetXMin();
            iXMax = (int)kIntr.GetXMax();
            iYMin = (int)kIntr.GetYMin();
            iYMax = (int)kIntr.GetYMax();
            DrawRectangle(iXMin,iYMin,iXMax,iYMax,kRed,true);
            DrawRectangle(iXMin,iYMin,iXMax,iYMax,kBlack);
        }
    }
}
//----------------------------------------------------------------------------
