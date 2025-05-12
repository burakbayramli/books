// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "KeyframeAnimation.h"

KeyframeAnimation g_kTheApp;

//----------------------------------------------------------------------------
KeyframeAnimation::KeyframeAnimation ()
    :
    Application2("KeyframeAnimation",0,0,256,256,ColorRGB(1.0f,1.0f,1.0f)),
    m_iNumKeys(7),
    m_fHKLength(70.0f),
    m_fKALength(70.0f),
    m_fAFLength(15.0f),
    m_fTMin(0.0f),
    m_fTMax((float)(m_iNumKeys-3)),
    m_fDt(0.01f*(m_fTMax-m_fTMin))
{
    m_pkPosSpline = NULL;
    m_pkHRotSpline = NULL;
    m_pkKRotSpline = NULL;
    m_pkARotSpline = NULL;
    m_fTime = m_fTMin;
    m_bDrawKeys = true;
}
//----------------------------------------------------------------------------
KeyframeAnimation::~KeyframeAnimation ()
{
}
//----------------------------------------------------------------------------
bool KeyframeAnimation::OnInitialize ()
{
    if ( !Application2::OnInitialize() )
        return false;

    // keyframe data
    Vector3f akPos[7] =
    {
        Vector3f(50.0f,34.0f,0.0f),
        Vector3f(50.0f,34.0f,0.0f),
        Vector3f(80.0f,33.0f,0.0f),
        Vector3f(110.0f,33.0f,0.0f),
        Vector3f(140.0f,34.0f,0.0f),
        Vector3f(207.0f,34.0f,0.0f),
        Vector3f(207.0f,34.0f,0.0f)
    };

    float afHAngle[7] =
    {
        0.0f, 0.0f, 0.392699f, 0.589049f, 0.490874f, 0.0f, 0.0f
    };

    float afKAngle[7] =
    {
        2.748894f, 2.748894f, 1.963495f, 2.356194f, 2.748894f, 2.748894f,
        2.748894f
    };

    float afAAngle[7] =
    {
        -1.178097f, -1.178097f, -1.178097f, -1.570796f, -1.963495f,
        -1.178097f, -1.178097f
    };

    PosKey akHPos[7];
    RotKey akHRot[7], akKRot[7], akARot[7];
    m_akLeg = new Leg[7];

    int i;
    for (i = 0; i < m_iNumKeys; i++)
    {
        float fTime = (float)(i-1);
        akHPos[i].Time() = fTime;
        akHPos[i].P() = akPos[i];
        akHRot[i].Time() = fTime;
        akHRot[i].Q().FromAxisAngle(Vector3f::UNIT_Z,afHAngle[i]);
        akKRot[i].Time() = fTime;
        akKRot[i].Q().FromAxisAngle(Vector3f::UNIT_Z,afKAngle[i]);
        akARot[i].Time() = fTime;
        akARot[i].Q().FromAxisAngle(Vector3f::UNIT_Z,afAAngle[i]);

        m_akLeg[i].m_fHKLength = m_fHKLength;
        m_akLeg[i].m_fKALength = m_fKALength;
        m_akLeg[i].m_fAFLength = m_fAFLength;
        m_akLeg[i].m_kH = akHPos[i].P();
        m_akLeg[i].m_fHAngle = afHAngle[i];
        m_akLeg[i].m_fKAngle = afKAngle[i];
        m_akLeg[i].m_fAAngle = afAAngle[i];
        InverseKinematics(m_akLeg[i]);
    }

    m_pkPosSpline = new PosSpline(m_iNumKeys,akHPos);
    m_pkHRotSpline = new RotSpline(m_iNumKeys,akHRot);
    m_pkKRotSpline = new RotSpline(m_iNumKeys,akKRot);
    m_pkARotSpline = new RotSpline(m_iNumKeys,akARot);

    OnDisplay();
    return true;
}
//----------------------------------------------------------------------------
void KeyframeAnimation::OnTerminate ()
{
    delete[] m_akLeg;
    delete m_pkPosSpline;
    delete m_pkHRotSpline;
    delete m_pkKRotSpline;
    delete m_pkARotSpline;
    Application2::OnTerminate();
}
//----------------------------------------------------------------------------
void KeyframeAnimation::OnDisplay ()
{
    ClearScreen();

    int iX0, iY0, iX1, iY1;

    if ( m_bDrawKeys )
    {
        // draw key frames
        for (int i = 0; i < m_iNumKeys; i++)
        {
            iX0 = (int)m_akLeg[i].m_kH.X();
            iY0 = (int)m_akLeg[i].m_kH.Y();
            iX1 = (int)m_akLeg[i].m_kK.X();
            iY1 = (int)m_akLeg[i].m_kK.Y();
            DrawLine(iX0,iY0,iX1,iY1,Color(0,0,0));

            iX0 = iX1;
            iY0 = iY1;
            iX1 = (int)m_akLeg[i].m_kA.X();
            iY1 = (int)m_akLeg[i].m_kA.Y();
            DrawLine(iX0,iY0,iX1,iY1,Color(0,0,0));

            iX0 = iX1;
            iY0 = iY1;
            iX1 = (int)m_akLeg[i].m_kF.X();
            iY1 = (int)m_akLeg[i].m_kF.Y();
            DrawLine(iX0,iY0,iX1,iY1,Color(0,0,0));
        }
    }

    // interpolate key frames
    Leg kInterp;
    float fAngle;
    Vector3f kAxis;

    kInterp.m_fHKLength = m_fHKLength;
    kInterp.m_fKALength = m_fKALength;
    kInterp.m_fAFLength = m_fAFLength;
    kInterp.m_kH =  m_pkPosSpline->Position(m_fTime);

    m_pkHRotSpline->Q(m_fTime).ToAxisAngle(kAxis,fAngle);
    kInterp.m_fHAngle = ( kAxis.Z() > 0.0f ? fAngle : -fAngle );

    m_pkKRotSpline->Q(m_fTime).ToAxisAngle(kAxis,fAngle);
    kInterp.m_fKAngle = ( kAxis.Z() > 0.0f ? fAngle : -fAngle );

    m_pkARotSpline->Q(m_fTime).ToAxisAngle(kAxis,fAngle);
    kInterp.m_fAAngle = ( kAxis.Z() > 0.0f ? fAngle : -fAngle );

    InverseKinematics(kInterp);
    
    // draw interpolated key frame
    iX0 = (int)kInterp.m_kH.X();
    iY0 = (int)kInterp.m_kH.Y();
    iX1 = (int)kInterp.m_kK.X();
    iY1 = (int)kInterp.m_kK.Y();
    DrawLine(iX0,iY0,iX1,iY1,Color(255,0,0));

    iX0 = iX1;
    iY0 = iY1;
    iX1 = (int)kInterp.m_kA.X();
    iY1 = (int)kInterp.m_kA.Y();
    DrawLine(iX0,iY0,iX1,iY1,Color(255,0,0));

    iX0 = iX1;
    iY0 = iY1;
    iX1 = (int)kInterp.m_kF.X();
    iY1 = (int)kInterp.m_kF.Y();
    DrawLine(iX0,iY0,iX1,iY1,Color(255,0,0));

    Application2::OnDisplay();
}
//----------------------------------------------------------------------------
void KeyframeAnimation::OnKeyDown (unsigned char ucKey, int, int)
{
    if ( ucKey == 'q' || ucKey == 'Q' || ucKey == KEY_ESCAPE )
    {
        RequestTermination();
        return;
    }

    switch ( ucKey )
    {
    case 'k':
    case 'K':
        // toggle drawing of key frames
        m_bDrawKeys = !m_bDrawKeys;
        OnDisplay();
        break;
    case 'b':
    case 'B':
        // single step backwards through animation
        m_fTime -= m_fDt;
        if ( m_fTime < m_fTMin )
            m_fTime = m_fTMax;
        OnDisplay();
        break;
    case 'f':
    case 'F':
        // single step forwards through animation
        m_fTime += m_fDt;
        if ( m_fTime > m_fTMax )
            m_fTime = m_fTMin;
        OnDisplay();
        break;
    }
}
//----------------------------------------------------------------------------
void KeyframeAnimation::InverseKinematics (Leg& rkLeg)
{
    rkLeg.m_kK.X() = rkLeg.m_kH.X() +
        rkLeg.m_fHKLength*Mathf::Sin(rkLeg.m_fHAngle);
    rkLeg.m_kK.Y() = rkLeg.m_kH.Y() +
        rkLeg.m_fHKLength*Mathf::Cos(rkLeg.m_fHAngle);
    rkLeg.m_kK.Z() = 0.0f;

    float fCos = Mathf::Cos(rkLeg.m_fKAngle);
    float fSin = Mathf::Sin(rkLeg.m_fKAngle);
    float fInvLength = 1.0f/rkLeg.m_fHKLength;
    float fDx = (rkLeg.m_kH.X()-rkLeg.m_kK.X())*fInvLength;
    float fDy = (rkLeg.m_kH.Y()-rkLeg.m_kK.Y())*fInvLength;
    rkLeg.m_kA.X() = rkLeg.m_kK.X()+rkLeg.m_fKALength*( fCos*fDx+fSin*fDy);
    rkLeg.m_kA.Y() = rkLeg.m_kK.Y()+rkLeg.m_fKALength*(-fSin*fDx+fCos*fDy);
    rkLeg.m_kA.Z() = 0;

    fCos = Mathf::Cos(rkLeg.m_fAAngle);
    fSin = Mathf::Sin(rkLeg.m_fAAngle);
    fInvLength = 1.0f/rkLeg.m_fKALength;
    fDx = (rkLeg.m_kK.X()-rkLeg.m_kA.X())*fInvLength;
    fDy = (rkLeg.m_kK.Y()-rkLeg.m_kA.Y())*fInvLength;
    rkLeg.m_kF.X() = rkLeg.m_kA.X()+rkLeg.m_fAFLength*( fCos*fDx+fSin*fDy);
    rkLeg.m_kF.Y() = rkLeg.m_kA.Y()+rkLeg.m_fAFLength*(-fSin*fDx+fCos*fDy);
    rkLeg.m_kF.Z() = 0.0f;
}
//---------------------------------------------------------------------------
