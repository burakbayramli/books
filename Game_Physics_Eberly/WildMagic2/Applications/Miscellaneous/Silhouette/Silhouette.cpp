// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "Silhouette.h"

Silhouette g_kTheApp;

//----------------------------------------------------------------------------
Silhouette::Silhouette ()
    :
    Application2("Silhouette",0,0,256,256,ColorRGB(1.0f,1.0f,1.0f))
{
}
//----------------------------------------------------------------------------
Silhouette::~Silhouette ()
{
}
//----------------------------------------------------------------------------
bool Silhouette::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    m_kEye = Vector3f::UNIT_Z;
    m_kRot = Matrix3f::IDENTITY;
    m_kRotXP.FromAxisAngle(Vector3f::UNIT_X,+0.1f);
    m_kRotXM.FromAxisAngle(Vector3f::UNIT_X,-0.1f);
    m_kRotYP.FromAxisAngle(Vector3f::UNIT_Y,+0.1f);
    m_kRotYM.FromAxisAngle(Vector3f::UNIT_Y,-0.1f);

    ConstructCube();
    m_kPoly.ComputeTerminator(m_kEye,m_kTerminator);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void Silhouette::OnTerminate ()
{
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void Silhouette::OnDisplay ()
{
    ClearScreen();
    DrawPolyhedron();
    DrawTerminator();
    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void Silhouette::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }
}
//----------------------------------------------------------------------------
void Silhouette::OnSpecialKeyDown (int iKey, int, int)
{
    if ( iKey == KEY_LEFT_ARROW )
    {
        m_kRot = m_kRotYP*m_kRot;
        m_kEye = m_kRot.GetColumn(2);
        m_kTerminator.clear();
        m_kPoly.ComputeTerminator(m_kEye,m_kTerminator);
        OnDisplay();
    }
    else if ( iKey == KEY_RIGHT_ARROW )
    {
        m_kRot = m_kRotYM*m_kRot;
        m_kEye = m_kRot.GetColumn(2);
        m_kTerminator.clear();
        m_kPoly.ComputeTerminator(m_kEye,m_kTerminator);
        OnDisplay();
    }
    else if ( iKey == KEY_DOWN_ARROW )
    {
        m_kRot = m_kRotXP*m_kRot;
        m_kEye = m_kRot.GetColumn(2);
        m_kTerminator.clear();
        m_kPoly.ComputeTerminator(m_kEye,m_kTerminator);
        OnDisplay();
    }
    else if ( iKey == KEY_UP_ARROW )
    {
        m_kRot = m_kRotXM*m_kRot;
        m_kEye = m_kRot.GetColumn(2);
        m_kTerminator.clear();
        m_kPoly.ComputeTerminator(m_kEye,m_kTerminator);
        OnDisplay();
    }
}
//----------------------------------------------------------------------------
void Silhouette::ConstructCube ()
{
    vector<Vector3f> akVertex(8);
    akVertex[0] = Vector3f(-0.5f,-0.5f,-0.5f);
    akVertex[1] = Vector3f(+0.5f,-0.5f,-0.5f);
    akVertex[2] = Vector3f(+0.5f,+0.5f,-0.5f);
    akVertex[3] = Vector3f(-0.5f,+0.5f,-0.5f);
    akVertex[4] = Vector3f(-0.5f,-0.5f,+0.5f);
    akVertex[5] = Vector3f(+0.5f,-0.5f,+0.5f);
    akVertex[6] = Vector3f(+0.5f,+0.5f,+0.5f);
    akVertex[7] = Vector3f(-0.5f,+0.5f,+0.5f);

    vector<int> aiConnect(36);
    aiConnect[ 0] = 0;  aiConnect[ 1] = 3;  aiConnect[ 2] = 2;
    aiConnect[ 3] = 0;  aiConnect[ 4] = 2;  aiConnect[ 5] = 1;
    aiConnect[ 6] = 0;  aiConnect[ 7] = 1;  aiConnect[ 8] = 5;
    aiConnect[ 9] = 0;  aiConnect[10] = 5;  aiConnect[11] = 4;
    aiConnect[12] = 0;  aiConnect[13] = 4;  aiConnect[14] = 7;
    aiConnect[15] = 0;  aiConnect[16] = 7;  aiConnect[17] = 3;
    aiConnect[18] = 6;  aiConnect[19] = 5;  aiConnect[20] = 1;
    aiConnect[21] = 6;  aiConnect[22] = 1;  aiConnect[23] = 2;
    aiConnect[24] = 6;  aiConnect[25] = 2;  aiConnect[26] = 3;
    aiConnect[27] = 6;  aiConnect[28] = 3;  aiConnect[29] = 7;
    aiConnect[30] = 6;  aiConnect[31] = 7;  aiConnect[32] = 4;
    aiConnect[33] = 6;  aiConnect[34] = 4;  aiConnect[35] = 5;

    m_kPoly.Create(akVertex,aiConnect);
}
//----------------------------------------------------------------------------
void Silhouette::ProjectPoint (const Vector3f& rkPoint, int& riX, int& riY)
{
    float fDot = m_kEye.Dot(rkPoint);
    float fT = 2.0f/(1.0f - fDot);
    Vector3f kProj = m_kEye + fT*(rkPoint - m_kEye);
    Vector3f kQ = kProj + m_kEye;
    Vector3f kProd = kQ*m_kRot;

    int iSize = GetWidth();
    riX = iSize/4 + (int)((iSize/8)*(kProd.X() + 2.0f));
    riY = iSize/4 + (int)((iSize/8)*(kProd.Y() + 2.0f));
}
//----------------------------------------------------------------------------
void Silhouette::DrawPolyhedron ()
{
    const vector<Vector3f>& rakPoint = m_kPoly.GetPoints();
    int iEQuantity = m_kPoly.GetEQuantity();
    for (int i = 0; i < iEQuantity; i++)
    {
        const MTEdge& rkE = m_kPoly.GetEdge(i);
        int iX0, iY0;
        ProjectPoint(rakPoint[m_kPoly.GetVLabel(rkE.GetVertex(0))],iX0,iY0);

        int iX1, iY1;
        ProjectPoint(rakPoint[m_kPoly.GetVLabel(rkE.GetVertex(1))],iX1,iY1);

        DrawLine(iX0,iY0,iX1,iY1,Color(255,0,0));
    }
}
//----------------------------------------------------------------------------
void Silhouette::DrawTerminator ()
{
    int iX0, iY0, iX1, iY1;
    ProjectPoint(m_kTerminator[0],iX0,iY0);
    for (int i = 1; i < (int)m_kTerminator.size(); i++)
    {
        ProjectPoint(m_kTerminator[i],iX1,iY1);
        DrawLine(iX0,iY0,iX1,iY1,Color(0,0,255));
        iX0 = iX1;
        iY0 = iY1;
    }
}
//----------------------------------------------------------------------------
