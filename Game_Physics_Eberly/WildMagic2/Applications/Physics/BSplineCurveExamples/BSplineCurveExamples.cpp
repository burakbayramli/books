// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "BSplineCurveExamples.h"

const int g_iSize = 128;
const float g_fV0 = g_iSize/16.0f;
const float g_fV1 = 0.5f*g_iSize;
const float g_fV2 = 15.0f*g_iSize/16.0f;

BSplineCurveExamples g_kTheApp;

//----------------------------------------------------------------------------
BSplineCurveExamples::BSplineCurveExamples ()
    :
    Application2("BSplineCurveExamples",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
    m_pkSpline = NULL;
    m_akCtrlPoint = NULL;
    m_afKnot = NULL;
    m_iCurveType = 0;
    m_bModified = false;

    m_afLocCtrlMin[0] = 1.0f/3.0f;
    m_afLocCtrlMax[0] = 5.0f/6.0f;
    m_afLocCtrlMin[1] = 1.0f/5.0f;
    m_afLocCtrlMax[1] = 4.0f/5.0f;
    m_afLocCtrlMin[2] = 1.0f/3.0f;
    m_afLocCtrlMax[2] = 5.0f/6.0f;
    m_afLocCtrlMin[3] = 2.0f/7.0f;
    m_afLocCtrlMax[3] = 5.0f/7.0f;
    m_afLocCtrlMin[4] = 1.0f/5.0f;
    m_afLocCtrlMax[4] = 4.0f/5.0f;
    m_afLocCtrlMin[5] = 1.0f/4.0f;
    m_afLocCtrlMax[5] = 5.0f/8.0f;
}
//----------------------------------------------------------------------------
BSplineCurveExamples::~BSplineCurveExamples ()
{
}
//----------------------------------------------------------------------------
bool BSplineCurveExamples::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    m_iNumCtrlPoints = 8;
    m_iDegree = 2;
    m_akCtrlPoint = new Vector2f[m_iNumCtrlPoints];

    m_akCtrlPoint[0] = Vector2f(g_fV0,g_fV0);
    m_akCtrlPoint[1] = Vector2f(g_fV1,g_fV0);
    m_akCtrlPoint[2] = Vector2f(g_fV2,g_fV0);
    m_akCtrlPoint[3] = Vector2f(g_fV2,g_fV1);
    m_akCtrlPoint[4] = Vector2f(g_fV2,g_fV2);
    m_akCtrlPoint[5] = Vector2f(g_fV1,g_fV2);
    m_akCtrlPoint[6] = Vector2f(g_fV0,g_fV2);
    m_akCtrlPoint[7] = Vector2f(g_fV0,g_fV1);

    // open uniform, not closed
    m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,m_iDegree,
        false,true);

    // Knots for later use.  The first 5 are used for the not-closed curve.
    // The first 6 are used for the closed curve.
    m_afKnot = new float[6];
    m_afKnot[0] = 0.1f;
    m_afKnot[1] = 0.2f;
    m_afKnot[2] = 0.4f;
    m_afKnot[3] = 0.7f;
    m_afKnot[4] = 0.8f;
    m_afKnot[5] = 0.9f;

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void BSplineCurveExamples::OnTerminate ()
{
    delete m_pkSpline;
    delete[] m_akCtrlPoint;
    delete[] m_afKnot;
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void BSplineCurveExamples::OnDisplay ()
{
    ClearScreen();

    // draw axes
    int i;
    for (i = g_iSize/16; i < g_iSize; i++)
    {
        SetPixel(g_iSize/16,g_iSize-1-i,Color(224,224,224));
        SetPixel(i,g_iSize-1-g_iSize/16,Color(224,224,224));
    }

    // draw control points
    int iMax = m_pkSpline->GetNumCtrlPoints();
    int iX, iY;
    for (i = 0; i < iMax; i++)
    {
        const Vector2f& rkCtrl = m_pkSpline->GetControlPoint(i);
        iX = int(rkCtrl.X()+0.5f);
        iY = g_iSize-1-int(rkCtrl.Y()+0.5f);
        const int iThick = 2;
        for (int iDY = -iThick; iDY <= iThick; iDY++)
        {
            for (int iDX = -iThick; iDX <= iThick; iDX++)
                SetPixel(iX+iDX,iY+iDY,Color(128,128,128));
        }
    }

    // draw spline
    iMax = 2048;
    for (i = 0; i <= iMax; i++)
    {
        // draw point
        float fU = i/(float)iMax;
        Vector2f kPos = m_pkSpline->GetPosition(fU);
        iX = int(kPos.X()+0.5f);
        iY = g_iSize-1-int(kPos.Y()+0.5f);

        if ( m_bModified
        &&  m_afLocCtrlMin[m_iCurveType] <= fU
        &&  fU <= m_afLocCtrlMax[m_iCurveType] )
        {
            SetPixel(iX,iY,Color(192,192,192));
        }
        else
        {
            SetPixel(iX,iY,Color(0,0,0));
        }
    }

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void BSplineCurveExamples::OnKeyDown (unsigned char ucKey, int iX, int iY)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case '0':  // open uniform, not-closed
        delete m_pkSpline;
        m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,
            m_iDegree,false,true);
        m_bModified = false;
        m_iCurveType = 0;
        OnDisplay();
        break;
    case '1':  // open nonuniform, not-closed
        delete m_pkSpline;
        m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,
            m_iDegree,false,m_afKnot);
        m_bModified = false;
        m_iCurveType = 1;
        OnDisplay();
        break;
    case '2':  // periodic, not-closed
        delete m_pkSpline;
        m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,
            m_iDegree,false,false);
        m_bModified = false;
        m_iCurveType = 2;
        OnDisplay();
        break;
    case '3':  // open uniform, closed
        delete m_pkSpline;
        m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,
            m_iDegree,true,true);
        m_bModified = false;
        m_iCurveType = 3;
        OnDisplay();
        break;
    case '4':  // open nonuniform, closed
        delete m_pkSpline;
        m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,
            m_iDegree,true,m_afKnot);
        m_bModified = false;
        m_iCurveType = 4;
        OnDisplay();
        break;
    case '5':  // periodic, closed
        delete m_pkSpline;
        m_pkSpline = new BSplineCurve2f(m_iNumCtrlPoints,m_akCtrlPoint,
            m_iDegree,true,false);
        m_bModified = false;
        m_iCurveType = 5;
        OnDisplay();
        break;
    case 'm':  // modify a control point
        m_pkSpline->SetControlPoint(4,Vector2f(g_iSize-2,g_iSize-2));
        m_bModified = true;
        OnDisplay();
        break;
    case 'r':  // restore a control point
        m_pkSpline->SetControlPoint(4,Vector2f(g_fV2,g_fV2));
        m_bModified = false;
        OnDisplay();
        break;
    }
}
//----------------------------------------------------------------------------
