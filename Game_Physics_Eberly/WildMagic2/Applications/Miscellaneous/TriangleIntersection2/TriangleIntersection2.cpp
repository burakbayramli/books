// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "TriangleIntersection2.h"
#include "WmlIntrTri2Tri2.h"

Application2::Color TriangleIntersection2::BLACK(0,0,0);
Application2::Color TriangleIntersection2::GRAY(128,128,128);
Application2::Color TriangleIntersection2::RED(255,0,0);
Application2::Color TriangleIntersection2::BLUE(0,0,255);
Application2::Color TriangleIntersection2::PURPLE(255,0,255);
Application2::Color TriangleIntersection2::LIGHT_RED(128,0,0);
Application2::Color TriangleIntersection2::LIGHT_BLUE(0,0,128);
Application2::Color TriangleIntersection2::LIGHT_PURPLE(128,0,128);

const int g_iSize = 512;
TriangleIntersection2 g_kTheApp;

//----------------------------------------------------------------------------
TriangleIntersection2::TriangleIntersection2 ()
    :
    Application2("TriangleIntersection2",0,0,g_iSize,g_iSize,
        ColorRGB(1.0f,1.0f,1.0f))
{
    m_bMouseDown = false;
    m_iTriangle = -1;
    m_iSelect = -1;
    m_iQuantity = 0;
    m_eType = TT_TEST;
}
//----------------------------------------------------------------------------
TriangleIntersection2::~TriangleIntersection2 ()
{
}
//----------------------------------------------------------------------------
bool TriangleIntersection2::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // create initial triangles
    int iW = GetWidth(), iH = GetHeight();
    m_aiX0[0] = iW/8;    m_aiY0[0] = iH/8;
    m_aiX0[1] = 3*iW/8;  m_aiY0[1] = iH/8;
    m_aiX0[2] = iW/8;    m_aiY0[2] = 3*iH/8;
    m_aiX1[0] = 7*iW/8;  m_aiY1[0] = 7*iH/8;
    m_aiX1[1] = 5*iW/8;  m_aiY1[1] = 7*iH/8;
    m_aiX1[2] = 7*iW/8;  m_aiY1[2] = 5*iH/8;

    m_uiAngle0 = 0;
    m_uiAngle1 = 0;
    m_uiSpeed0 = 0;
    m_uiSpeed1 = 0;

    GetIntersecting();
    return true;
}
//----------------------------------------------------------------------------
void TriangleIntersection2::OnTerminate ()
{
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void TriangleIntersection2::OnDisplay ()
{
    ClearScreen();

    switch ( m_eType )
    {
    case TT_TEST:
        strcpy(m_acHeader,"TEST: ");
        DrawTriangle(m_aiX0,m_aiY0,RED);
        DrawTriangle(m_aiX1,m_aiY1,BLUE);
        break;
    case TT_FIND:
        strcpy(m_acHeader,"FIND: ");
        DrawTriangle(m_aiX0,m_aiY0,RED);
        DrawTriangle(m_aiX1,m_aiY1,BLUE);
        DrawIntersection();
        break;
    case TT_TEST_VEL:
        strcpy(m_acHeader,"TEST VEL: ");
        DrawTriangle(m_aiX0,m_aiY0,RED);
        DrawTriangle(m_aiX1,m_aiY1,BLUE);
        DrawMovedTriangle(m_aiX0,m_aiY0,m_uiSpeed0,m_uiAngle0,LIGHT_RED);
        DrawMovedTriangle(m_aiX1,m_aiY1,m_uiSpeed1,m_uiAngle1,LIGHT_BLUE);
        break;
    case TT_FIND_VEL:
        strcpy(m_acHeader,"FIND VEL: ");
        DrawTriangle(m_aiX0,m_aiY0,RED);
        DrawTriangle(m_aiX1,m_aiY1,BLUE);
        DrawMovedTriangle(m_aiX0,m_aiY0,m_uiSpeed0,m_uiAngle0,LIGHT_RED);
        DrawMovedTriangle(m_aiX1,m_aiY1,m_uiSpeed1,m_uiAngle1,LIGHT_BLUE);
        DrawIntersection();
        break;
    default:  // TT_MAX
        break;
    }

    strcat(m_acHeader,"Intersecting = ");
    strcat(m_acHeader,(m_bIntersecting ? "YES" : "NO"));

    sprintf(m_acFooter,
        "tri = %d , spd0 = %u  ang0 = %u  spd1 = %u  ang1 = %u",
        m_iTriangle,m_uiSpeed0,m_uiAngle0,m_uiSpeed1,m_uiAngle1);

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void TriangleIntersection2::ScreenOverlay ()
{
    ms_spkRenderer->Draw(16,32,ColorRGB::BLACK,m_acHeader);
    ms_spkRenderer->Draw(16,g_iSize-16,ColorRGB::BLACK,m_acFooter);
}
//----------------------------------------------------------------------------
void TriangleIntersection2::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    // select next intersection type
    case 't':
        m_eType = Type((m_eType+1) % TT_MAX);
        GetIntersecting();
        break;

    // rotate velocity vector of active triangle
    case '+':
    case '=':
        if ( m_iTriangle == 0 )
            m_uiAngle0 = (m_uiAngle0 + 1) % MAX_ANGLE;
        else if ( m_iTriangle == 1 )
            m_uiAngle1 = (m_uiAngle1 + 1) % MAX_ANGLE;
        GetIntersecting();
        break;

    // rotate velocity vector of active triangle
    case '-':
    case '_':
        if ( m_iTriangle == 0 )
        {
            if ( m_uiAngle0 > 0 )
                m_uiAngle0--;
            else
                m_uiAngle0 = MAX_ANGLE - 1;
        }
        else if ( m_iTriangle == 1 )
        {
            if ( m_uiAngle1 > 0 )
                m_uiAngle1--;
            else
                m_uiAngle1 = MAX_ANGLE - 1;
        }
        GetIntersecting();
        break;

    // increase speed of active triangle
    case '>':
    case '.':
        if ( m_iTriangle == 0 )
        {
            if ( m_uiSpeed0 < MAX_SPEED )
                m_uiSpeed0++;
        }
        else if ( m_iTriangle == 1 )
        {
            if ( m_uiSpeed1 < MAX_SPEED )
                m_uiSpeed1++;
        }
        GetIntersecting();
        break;

    // decrease speed of active triangle
    case '<':
    case ',':
        if ( m_iTriangle == 0 )
        {
            if ( m_uiSpeed0 > 0 )
                m_uiSpeed0--;
        }
        else if ( m_iTriangle == 1 )
        {
            if ( m_uiSpeed1 > 0 )
                m_uiSpeed1--;
        }
        GetIntersecting();
        break;
    }
}
//----------------------------------------------------------------------------
void TriangleIntersection2::OnMouseClick (int iButton, int iState, int iX,
    int iY, unsigned int)
{
    if ( iButton != MOUSE_LEFT_BUTTON )
        return;

    if ( iState == MOUSE_DOWN )
    {
        m_bMouseDown = true;
        m_iTriangle = -1;
        m_iSelect = -1;

        if ( 0 <= iX && iX < GetWidth() && 0 <= iY && iY < GetHeight() )
        {
            int i;
            for (i = 0; i < 3; i++)
            {
                if ( MouseNearVertex(iX,iY,m_aiX0[i],m_aiY0[i]) )
                {
                    m_iTriangle = 0;
                    m_iSelect = i;
                    return;
                }
            }

            for (i = 0; i < 3; i++)
            {
                if ( MouseNearVertex(iX,iY,m_aiX1[i],m_aiY1[i]) )
                {
                    m_iTriangle = 1;
                    m_iSelect = i;
                    return;
                }
            }

            if ( MouseInTriangle(iX,iY,m_aiX0,m_aiY0) )
            {
                m_iTriangle = 0;
                m_iXMouseStart = iX;
                m_iYMouseStart = iY;
                return;
            }

            if ( MouseInTriangle(iX,iY,m_aiX1,m_aiY1) )
            {
                m_iTriangle = 1;
                m_iXMouseStart = iX;
                m_iYMouseStart = iY;
                return;
            }
        }
    }
    else if ( iState == MOUSE_UP )
    {
        m_bMouseDown = false;
        AdjustTriangle(iX,iY);
    }
}
//----------------------------------------------------------------------------
void TriangleIntersection2::OnMotion (int iX, int iY, unsigned int)
{
    if ( m_bMouseDown )
    {
        if ( 0 <= iX && iX < GetWidth() && 0 <= iY && iY < GetHeight() )
        {
            AdjustTriangle(iX,iY);
            m_iXMouseStart = iX;
            m_iYMouseStart = iY;
        }
    }
}
//----------------------------------------------------------------------------
Vector2f TriangleIntersection2::GetVelocity (unsigned int uiSpeed,
    unsigned int uiAngle)
{
    float fAngle = Mathf::TWO_PI*uiAngle/MAX_ANGLE;
    float fCos = Mathf::Cos(fAngle);
    float fSin = Mathf::Sin(fAngle);
    return Vector2f(uiSpeed*fCos,uiSpeed*fSin);
}
//----------------------------------------------------------------------------
void TriangleIntersection2::GetIntersecting ()
{
    Triangle2f kTri0;
    kTri0.Origin() = Vector2f((float)m_aiX0[0],(float)m_aiY0[0]);
    kTri0.Edge0() = Vector2f((float)m_aiX0[1],(float)m_aiY0[1]) -
        kTri0.Origin();
    kTri0.Edge1() = Vector2f((float)m_aiX0[2],(float)m_aiY0[2]) -
        kTri0.Origin();

    Triangle2f kTri1;
    kTri1.Origin() = Vector2f((float)m_aiX1[0],(float)m_aiY1[0]);
    kTri1.Edge0() = Vector2f((float)m_aiX1[1],(float)m_aiY1[1]) -
        kTri1.Origin();
    kTri1.Edge1() = Vector2f((float)m_aiX1[2],(float)m_aiY1[2]) -
        kTri1.Origin();

    Vector2f kW0, kW1;
    float fTLast, fTMax = 1.0f;
    m_iQuantity = 0;

    switch ( m_eType )
    {
    case TT_TEST:
        m_bIntersecting = TestIntersection(kTri0,kTri1);
        break;
    case TT_FIND:
        m_bIntersecting = FindIntersection(kTri0,kTri1,m_iQuantity,
            m_akVertex);
        break;
    case TT_TEST_VEL:
        kW0 = GetVelocity(m_uiSpeed0,m_uiAngle0);
        kW1 = GetVelocity(m_uiSpeed1,m_uiAngle1);
        m_bIntersecting = TestIntersection(fTMax,kTri0,kW0,kTri1,kW1,
            m_fTFirst,fTLast);
        break;
    case TT_FIND_VEL:
        kW0 = GetVelocity(m_uiSpeed0,m_uiAngle0);
        kW1 = GetVelocity(m_uiSpeed1,m_uiAngle1);
        m_bIntersecting = FindIntersection(fTMax,kTri0,kW0,kTri1,kW1,
            m_fTFirst,fTLast,m_iQuantity,m_akVertex);
        break;
    default:  // TT_MAX
        break;
    }

    OnDisplay();
}
//----------------------------------------------------------------------------
void TriangleIntersection2::DrawTriangle (int aiX[3], int aiY[3],
    const Color& rkColor)
{
    DrawLine(aiX[0],aiY[0],aiX[1],aiY[1],rkColor);
    DrawLine(aiX[1],aiY[1],aiX[2],aiY[2],rkColor);
    DrawLine(aiX[2],aiY[2],aiX[0],aiY[0],rkColor);
}
//----------------------------------------------------------------------------
void TriangleIntersection2::DrawMovedTriangle (int aiX[3], int aiY[3],
    unsigned int uiSpeed, unsigned int uiAngle, const Color& rkColor)
{
    Vector2f kW = GetVelocity(uiSpeed,uiAngle);
    if ( m_bIntersecting )
        kW *= m_fTFirst;

    int aiMoveX[3] =
    {
        (int)(aiX[0] + kW.X()),
        (int)(aiX[1] + kW.X()),
        (int)(aiX[2] + kW.X())
    };

    int aiMoveY[3] =
    {
        (int)(aiY[0] + kW.Y()),
        (int)(aiY[1] + kW.Y()),
        (int)(aiY[2] + kW.Y())
    };

    // draw triangle boundary
    DrawLine(aiMoveX[0],aiMoveY[0],aiMoveX[1],aiMoveY[1],rkColor);
    DrawLine(aiMoveX[1],aiMoveY[1],aiMoveX[2],aiMoveY[2],rkColor);
    DrawLine(aiMoveX[2],aiMoveY[2],aiMoveX[0],aiMoveY[0],rkColor);

    // connect start/final triangle vertices
    for (int i = 0; i < 3; i++)
        DrawLine(aiX[i],aiY[i],aiMoveX[i],aiMoveY[i],rkColor);
}
//----------------------------------------------------------------------------
void TriangleIntersection2::DrawIntersection ()
{
    if ( m_iQuantity == 0 )
        return;

    int i;
    for (i = 0; i < m_iQuantity-1; i++)
    {
        DrawLine((int)m_akVertex[i].X(),(int)m_akVertex[i].Y(),
            (int)m_akVertex[i+1].X(),(int)m_akVertex[i+1].Y(),PURPLE);
    }
    DrawLine((int)m_akVertex[i].X(),(int)m_akVertex[i].Y(),
        (int)m_akVertex[0].X(),(int)m_akVertex[0].Y(),PURPLE);
}
//----------------------------------------------------------------------------
bool TriangleIntersection2::MouseNearVertex (int iXPos, int iYPos, int iXVer,
    int iYVer)
{
    const int iThick = 3;
    return abs(iXPos - iXVer) <= iThick && abs(iYPos - iYVer) <= iThick;
}
//----------------------------------------------------------------------------
bool TriangleIntersection2::MouseInTriangle (int iXPos, int iYPos, int aiX[3],
    int aiY[3])
{
    int iE0x = aiX[1] - aiX[0], iE0y = aiY[1] - aiY[0];
    int iE1x = aiX[2] - aiX[0], iE1y = aiY[2] - aiY[0];
    int iDx = iXPos - aiX[0], iDy = iYPos - aiY[0];
    int iE00 = iE0x*iE0x + iE0y*iE0y;
    int iE01 = iE0x*iE1x + iE0y*iE1y;
    int iE11 = iE1x*iE1x + iE1y*iE1y;
    int iR0 = iE0x*iDx + iE0y*iDy;
    int iR1 = iE1x*iDx + iE1y*iDy;
    int iS0 = iE11*iR0 - iE01*iR1;
    int iS1 = iE00*iR1 - iE01*iR0;
    int iS2 = iE00*iE11 - iE01*iE01;
    return 0 <= iS0 && 0 <= iS1 && iS0+iS1 <= iS2;
}
//----------------------------------------------------------------------------
void TriangleIntersection2::AdjustTriangle (int iXPos, int iYPos)
{
    int i, iDx, iDy;

    if ( m_iTriangle == 0 )
    {
        if ( m_iSelect == -1 )
        {
            iDx = iXPos - m_iXMouseStart;
            iDy = iYPos - m_iYMouseStart;
            for (i = 0; i < 3; i++)
            {
                m_aiX0[i] += iDx;
                m_aiY0[i] += iDy;
            }
        }
        else
        {
            m_aiX0[m_iSelect] = iXPos;
            m_aiY0[m_iSelect] = iYPos;
        }
    }
    else if ( m_iTriangle == 1 )
    {
        if ( m_iSelect == -1 )
        {
            iDx = iXPos - m_iXMouseStart;
            iDy = iYPos - m_iYMouseStart;
            for (i = 0; i < 3; i++)
            {
                m_aiX1[i] += iDx;
                m_aiY1[i] += iDy;
            }
        }
        else
        {
            m_aiX1[m_iSelect] = iXPos;
            m_aiY1[m_iSelect] = iYPos;
        }
    }

    GetIntersecting();
}
//----------------------------------------------------------------------------
