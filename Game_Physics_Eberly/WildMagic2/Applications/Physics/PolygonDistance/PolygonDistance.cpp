// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "PolygonDistance.h"

const int g_iSize = 512;
PolygonDistance g_kTheApp;

//----------------------------------------------------------------------------
PolygonDistance::PolygonDistance ()
    :
    Application2("PolygonDistance",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
}
//----------------------------------------------------------------------------
PolygonDistance::~PolygonDistance ()
{
}
//----------------------------------------------------------------------------
bool PolygonDistance::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // warm up the random number generator
    srand((unsigned)time(NULL));
    int i, j = rand() % 100;
    for (i = 0; i < j; i++)
        rand();

    m_kPolygon = new Polygon[PD_NUM_POLYS];
    for (i = 0; i < PD_NUM_POLYS; i++)
    {
        m_kPolygon[i].m_iNumVertices = 5-i;
        m_kPolygon[i].m_akVertices =
            new Vector2f[m_kPolygon[i].m_iNumVertices];
        m_kPolygon[i].m_akPolar = new Vector2f[m_kPolygon[i].m_iNumVertices];
        m_kPolygon[i].m_akFaces = new Tuple2[m_kPolygon[i].m_iNumVertices];
    }

    InitialConfiguration();
    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void PolygonDistance::OnTerminate ()
{
    for (int i = 0; i < PD_NUM_POLYS; i++)
    {
        delete[] m_kPolygon[i].m_akVertices;
        delete[] m_kPolygon[i].m_akPolar;
        delete[] m_kPolygon[i].m_akFaces;
    }
    delete[] m_kPolygon;

    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void PolygonDistance::OnDisplay ()
{
    ClearScreen();

    const int iLineThick = 0;
    Color kLineColor(0,0,0);
    const int iSolutionThick = 2;
    const int iCentroidThick = 4;
    Color kCentroidColor(0,0,128);

    // draw polygons
    int i, j;
    for (i = 0; i < PD_NUM_POLYS; i++)
    {
        for (j = 0; j < m_kPolygon[i].m_iNumVertices; j++)
        {
            int k = (j+1) % m_kPolygon[i].m_iNumVertices;
            DrawLineSegment(iLineThick,kLineColor,
                m_kPolygon[i].m_akVertices[j],m_kPolygon[i].m_akVertices[k]);
        }
    }

    // draw line joining nearest points
    for (int k = 0; k < PD_NUM_POLYS; k++)
    {
        int k0 = (k+1) % PD_NUM_POLYS;
        Vector2f* akV00 = new Vector2f[m_kPolygon[k].m_iNumVertices];
        Vector2f* akV01 = new Vector2f[m_kPolygon[k0].m_iNumVertices];
        for (i = 0; i < m_kPolygon[k].m_iNumVertices; i++)
        {
            for (j = 0; j < 2; j++)
                akV00[i][j] = m_kPolygon[k].m_akVertices[i][j];
        }
        for (i = 0; i < m_kPolygon[k0].m_iNumVertices; i++)
        {
            for (j = 0; j < 2; j++)
                akV01[i][j] = m_kPolygon[k0].m_akVertices[i][j];
        }

        // A "function object" call LCPPolyDist2(m_kPolygon[k]...) works as
        // it should under MSVC6 and MSVC7, but it does not work with g++
        // version 3.2 (on Red Hat Linux 8 3.2-7).  So we need a named dummy
        // object.
        int iStatusCode;
        float fReturn;
        Vector2f akRes[2];
        LCPPolyDist2 kDummy(m_kPolygon[k].m_iNumVertices,akV00,
            m_kPolygon[k].m_iNumVertices,m_kPolygon[k].m_akFaces,
            m_kPolygon[k0].m_iNumVertices,akV01,m_kPolygon[k0].m_iNumVertices,
            m_kPolygon[k0].m_akFaces,iStatusCode,fReturn,akRes);
        
        // draw line joining solution points
        DrawLineSegment(iLineThick,kLineColor,akRes[0],akRes[1]);

        if ( m_bDoPerps && fReturn > 0.1f )
        {
            // compute perpendiculars to edges at solution points
            Vector2f akEndPoints[2];
            DoEdgeNorm(m_kPolygon[k].m_iNumVertices,
                m_kPolygon[k].m_akVertices,akRes[0],akEndPoints);
            DrawPerps(akEndPoints);
            DoEdgeNorm(m_kPolygon[k0].m_iNumVertices,
                m_kPolygon[k0].m_akVertices,akRes[1],akEndPoints);
            DrawPerps(akEndPoints);
        }

        // draw nearest points
        switch (iStatusCode)
        {
        case LCPPolyDist2::SC_FOUND_SOLUTION:
            {
            Color kSolutionFound(0,128,0);
            for (i = 0; i < 2; i++)
                DrawPoints(iSolutionThick,kSolutionFound,akRes[i]);
            break;
            }
        case LCPPolyDist2::SC_TEST_POINTS_TEST_FAILED:
            {
            Color kTestPointsFailed(0,0,128);
            for (i = 0; i < 2; i++)
                DrawPoints(iSolutionThick,kTestPointsFailed,akRes[i]);
            break;
            }
        case LCPPolyDist2::SC_VERIFY_FAILURE:
            {
            Color kVerifyFailed(128,0,0);
            for (i = 0; i < 2; i++)
                DrawPoints(iSolutionThick,kVerifyFailed,akRes[i]);
            break;
            }
        }

        // draw centroids
        DrawPoints(iCentroidThick,kCentroidColor,m_kPolygon[k].m_akCentroid);
        DrawPoints(iCentroidThick,kCentroidColor,m_kPolygon[k0].m_akCentroid);

        delete[] akV00;
        delete[] akV01;
    }

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void PolygonDistance::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'g':
        NextConfiguration();
        OnDisplay();
        break;
    case '0':
        InitialConfiguration();
        OnDisplay();
        break;
    case 'p':  // toggle for drawing the perpendiculars
        m_bDoPerps = !m_bDoPerps;
        OnDisplay();
        break;
    }
}
//----------------------------------------------------------------------------
void PolygonDistance::InitialConfiguration ()
{
    // first polygon
    m_kPolygon[0].m_akVertices[0] = Vector2f(50.0f,50.0f);
    m_kPolygon[0].m_akVertices[1] = Vector2f(60.0f,200.0f);
    m_kPolygon[0].m_akVertices[2] = Vector2f(90.0f,250.0f);
    m_kPolygon[0].m_akVertices[3] = Vector2f(150.0f,300.0f);
    m_kPolygon[0].m_akVertices[4] = Vector2f(200.0f,100.0f);

    PolarRepresentation(m_kPolygon[0].m_iNumVertices,
        m_kPolygon[0].m_akVertices,m_kPolygon[0].m_akCentroid,
        m_kPolygon[0].m_akPolar);
    
    m_kPolygon[0].m_akFaces[0][0] = 0, m_kPolygon[0].m_akFaces[0][1] = 1;
    m_kPolygon[0].m_akFaces[1][0] = 1, m_kPolygon[0].m_akFaces[1][1] = 2;
    m_kPolygon[0].m_akFaces[2][0] = 2, m_kPolygon[0].m_akFaces[2][1] = 3;
    m_kPolygon[0].m_akFaces[3][0] = 3, m_kPolygon[0].m_akFaces[3][1] = 4;
    m_kPolygon[0].m_akFaces[4][0] = 4, m_kPolygon[0].m_akFaces[4][1] = 0;

    // second polygon
    m_kPolygon[1].m_akVertices[0] = Vector2f(250.0f,250.0f);
    m_kPolygon[1].m_akVertices[1] = Vector2f(260.0f,400.0f);
    m_kPolygon[1].m_akVertices[2] = Vector2f(350.0f,450.0f);
    m_kPolygon[1].m_akVertices[3] = Vector2f(375.0f,300.0f);

    PolarRepresentation(m_kPolygon[1].m_iNumVertices,
        m_kPolygon[1].m_akVertices,m_kPolygon[1].m_akCentroid,
        m_kPolygon[1].m_akPolar);
    
    m_kPolygon[1].m_akFaces[0][0] = 0, m_kPolygon[1].m_akFaces[0][1] = 1;
    m_kPolygon[1].m_akFaces[1][0] = 1, m_kPolygon[1].m_akFaces[1][1] = 2;
    m_kPolygon[1].m_akFaces[2][0] = 2, m_kPolygon[1].m_akFaces[2][1] = 3;
    m_kPolygon[1].m_akFaces[3][0] = 3, m_kPolygon[1].m_akFaces[3][1] = 0;

    // third polygon
    m_kPolygon[2].m_akVertices[0] = Vector2f(200.0f,200.0f);
    m_kPolygon[2].m_akVertices[1] = Vector2f(400.0f,300.0f);
    m_kPolygon[2].m_akVertices[2] = Vector2f(350.0f,100.0f);

    PolarRepresentation(m_kPolygon[2].m_iNumVertices,
        m_kPolygon[2].m_akVertices,m_kPolygon[2].m_akCentroid,
        m_kPolygon[2].m_akPolar);
    
    m_kPolygon[2].m_akFaces[0][0] = 0, m_kPolygon[2].m_akFaces[0][1] = 1;
    m_kPolygon[2].m_akFaces[1][0] = 1, m_kPolygon[2].m_akFaces[1][1] = 2;
    m_kPolygon[2].m_akFaces[2][0] = 2, m_kPolygon[2].m_akFaces[2][1] = 0;

    for (int i = 0; i < PD_NUM_POLYS; i++)
    {
        // randomly select a direction to rotate
        m_kPolygon[i].m_iSign = (Mathf::SymmetricRandom() > 0.0f ? 1 : -1);
    }

    m_bDoPerps = false;
}
//----------------------------------------------------------------------------
void PolygonDistance::PolarRepresentation (int iNum,
    const Vector2f* akVertices, Vector2f& rkCentroid, Vector2f* akPolar)
{
    int i;
    rkCentroid = Vector2f(0.0f,0.0f);
    for (i = 0; i < iNum; i++)
        rkCentroid += akVertices[i];
    rkCentroid /= (float) iNum;
    for (i = 0; i < iNum; i++)
    {
        Vector2f kDiff = akVertices[i] - rkCentroid;
        float fTemp;
        if ( kDiff[0] == 0.0f )
            fTemp = Mathf::HALF_PI;
        else
            fTemp = Mathf::ATan(kDiff[1]/kDiff[0]);
        if ( kDiff[0] < 0.0f )
            fTemp = Mathf::PI+fTemp;
        akPolar[i][1] = fTemp;
        akPolar[i][0] = kDiff.Length();
    }
}
//----------------------------------------------------------------------------
void PolygonDistance::CartesianRepresentation (int iNum, 
    Vector2f* akVertices, const Vector2f& rkCentroid, const Vector2f* akPolar)
{
    for (int i = 0; i < iNum; i++)
    {
        akVertices[i][0] = akPolar[i][0]*Mathf::Cos(akPolar[i][1]);
        akVertices[i][1] = akPolar[i][0]*Mathf::Sin(akPolar[i][1]);
        akVertices[i] += rkCentroid;
    }
}
//----------------------------------------------------------------------------
void PolygonDistance::NextConfiguration ()
{
    for (int i = 0; i < PD_NUM_POLYS; i++)
    {
        PolyRotate(m_kPolygon[i].m_iNumVertices,m_kPolygon[i].m_iSign,
            m_kPolygon[i].m_akPolar);
        CartesianRepresentation(m_kPolygon[i].m_iNumVertices,
            m_kPolygon[i].m_akVertices,m_kPolygon[i].m_akCentroid,
            m_kPolygon[i].m_akPolar);
    }
}
//----------------------------------------------------------------------------
void PolygonDistance::PolyRotate (int iNum, int iSign, Vector2f* akPolar)
{
    // rotate figures by random amounts in randomly selected directions
    const float fRandomWidth = 0.08f;
    float fFactor = iSign*fRandomWidth;
    for (int i = 0; i < iNum; i++)
        akPolar[i][1] += fFactor;
}
//----------------------------------------------------------------------------
void PolygonDistance::DoEdgeNorm (int iNum, const Vector2f* akVertices,
    const Vector2f& rkClosest, Vector2f* akEnd)
{
    const float fNormLength = 40.0f;
    const float fClose = 0.1f;
    akEnd[0] = rkClosest;
    akEnd[1] = rkClosest;
    for (int i = 0; i < iNum; i++)
    {
        int j = (i+1) % iNum;
        float fD = akVertices[i][0] - akVertices[j][0];
        if ( fD == 0.0f )
        {
            // the edge is vertical
            if ( rkClosest[1] == akVertices[i][0] )
            {
                // the result lies on the edge
                Vector2f kDiff0 = rkClosest - akVertices[0];
                Vector2f kDiff1 = rkClosest - akVertices[1];
                if ( kDiff0.Length() > fClose && kDiff1.Length() > fClose )
                {
                    // the result is on the edge but not a vertex
                    akEnd[0] = rkClosest + fNormLength*Vector2f(1.0,0.0);
                    akEnd[1] = rkClosest - fNormLength*Vector2f(1.0,0.0);
                }
                return;
            }
            else
            {
                // the result is not on this edge, go to next edge
                continue;
            }
        }
        // the edge is not vertical
        float fN = rkClosest[0] - akVertices[j][0];
        float fT = fN/fD;
        if ( fT <= 0.0f || fT >= 1.0f )
        {
            // the result is not in this line segment
            continue;
        }
        float fTemp = Mathf::FAbs(fT*akVertices[i][1] +
            (1.0f-fT)*akVertices[j][1] - rkClosest[1]);
        if ( fTemp < fClose )
        {
            // the solution is on this edge
            Vector2f kDiff2 = rkClosest - akVertices[0];
            Vector2f kDiff3 = rkClosest - akVertices[1];
            if ( kDiff2.Length() > fClose && kDiff3.Length() > fClose )
            {
                // the result is on the edge but not a vertex
                Vector2f kNorm = Vector2f(akVertices[i][1]-akVertices[j][1],
                    -fD);
                kNorm.Normalize();
                akEnd[0] = rkClosest + fNormLength*kNorm;
                akEnd[1] = rkClosest - fNormLength*kNorm;
                return;
            }
        }
    }
}
//----------------------------------------------------------------------------
void PolygonDistance::DrawPerps (const Vector2f* akEP)
{
    DrawLineSegment(1,Color(0,128,0),akEP[0],akEP[1]);
}
//----------------------------------------------------------------------------
void PolygonDistance::DrawLineSegment (int iThick, Color kColor,
    const Vector2f& rkEnd1, const Vector2f& rkEnd2)
{
    const int iMax = 2048;
    for (int i = 0; i <= iMax; i++)
    {
        float fT = i/(float)iMax;
        Vector2f kPos = fT*rkEnd1+(1.0f-fT)*rkEnd2;
        int iX = int(kPos.X()+0.5f);
        if ( i > iMax/2 )
            i = i;
        int iY = g_iSize-1-int(kPos.Y()+0.5f);
        for (int iDY = -iThick; iDY <= iThick; iDY++)
        {
            for (int iDX = -iThick; iDX <= iThick; iDX++)
                SetPixel(iX+iDX,iY+iDY,kColor);
        }
    }
}
//----------------------------------------------------------------------------
void PolygonDistance::DrawPoints (int iThick, Color kColor,
    const Vector2f& rkPoint)
{
    int iX = int(rkPoint.X()+0.5f);
    int iY = g_iSize-1-int(rkPoint.Y()+0.5f);
    for (int iDY = -iThick; iDY <= iThick; iDY++)
    {
        for (int iDX = -iThick; iDX <= iThick; iDX++)
            SetPixel(iX+iDX,iY+iDY,kColor);
    }
}
//----------------------------------------------------------------------------
