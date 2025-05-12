// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "ClodPolyline.h"

const int g_iSize = 256;
ClodPolyline g_kTheApp;

//----------------------------------------------------------------------------
ClodPolyline::ClodPolyline ()
    :
    Application2("ClodPolyline",0,0,g_iSize,g_iSize,ColorRGB(1.0f,1.0f,1.0f))
{
    m_pkPolyline = NULL;
}
//----------------------------------------------------------------------------
ClodPolyline::~ClodPolyline ()
{
}
//----------------------------------------------------------------------------
bool ClodPolyline::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // generate points on unit circle, then adjust the distances to center
    int iVQuantity = 16;
    Vector3f* akVertex = new Vector3f[iVQuantity];
    int i;
    for (i = 0; i < iVQuantity; i++)
    {
        float fAngle = Mathf::TWO_PI*i/iVQuantity;
        akVertex[i].X() = Mathf::Cos(fAngle);
        akVertex[i].Y() = Mathf::Sin(fAngle);
        akVertex[i].Z() = 0.0f;

        float fAdjust = 1.0f + 0.25f*Mathf::SymmetricRandom();
        akVertex[i] *= fAdjust;
    }

    m_pkPolyline = new Polyline3(iVQuantity,akVertex,true);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void ClodPolyline::OnTerminate ()
{
    delete m_pkPolyline;
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void ClodPolyline::OnDisplay ()
{
    ClearScreen();

    int iEQuantity = m_pkPolyline->GetEdgeQuantity();
    const int* aiEdge = m_pkPolyline->GetEdges();
    const Vector3f* akVertex = m_pkPolyline->GetVertices();

    Vector3f kV;
    int i;

    for (i = 0; i < m_pkPolyline->GetVertexQuantity(); i++)
    {
        kV = akVertex[i];
        int iX = int(0.25f*g_iSize*(kV.X()+2.0f));
        int iY = g_iSize - 1 - (int)(0.25f*g_iSize*(kV.Y()+2.0f));
        for (int iDY = -1; iDY <= 1; iDY++)
        {
            for (int iDX = -1; iDX <= 1; iDX++)
                SetPixel(iX+iDX,iY+iDY,Color(0,0,0));
        }
    }

    for (i = 0; i < iEQuantity; i++)
    {
        kV = akVertex[aiEdge[2*i]];
        int iX0 = int(0.25f*g_iSize*(kV.X()+2.0f));
        int iY0 = g_iSize - 1 - (int)(0.25f*g_iSize*(kV.Y()+2.0f));

        kV = akVertex[aiEdge[2*i+1]];
        int iX1 = int(0.25*g_iSize*(kV.X()+2.0));
        int iY1 = g_iSize - 1 - (int)(0.25f*g_iSize*(kV.Y()+2.0f));
        DrawLine(iX0,iY0,iX1,iY1,Color(0,0,0));
    }

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void ClodPolyline::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    int iLOD, iMaxLOD;

    switch ( ucKey )
    {
    case '+':  // increase level of detail
    case '=':
        iLOD = m_pkPolyline->GetLevelOfDetail();
        iMaxLOD = m_pkPolyline->GetMaxLevelOfDetail();
        if ( iLOD < iMaxLOD )
        {
            m_pkPolyline->SetLevelOfDetail(iLOD+1);
            OnDisplay();
        }
        break;
    case '-':  // decrease level of detail
    case '_':
        iLOD = m_pkPolyline->GetLevelOfDetail();
        if ( iLOD > 0 )
        {
            m_pkPolyline->SetLevelOfDetail(iLOD-1);
            OnDisplay();
        }
        break;
    }
}
//----------------------------------------------------------------------------
