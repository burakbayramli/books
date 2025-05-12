// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef KEYFRAMEANIMATION_H
#define KEYFRAMEANIMATION_H

#include "WmlApplication2.h"
#include "PosSpline.h"
#include "RotSpline.h"

class KeyframeAnimation : public Application2
{
public:
    KeyframeAnimation ();
    virtual ~KeyframeAnimation ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnDisplay ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    class Leg
    {
    public:
        // specified
        Vector3f m_kH;
        float m_fHKLength, m_fKALength, m_fAFLength;
        float m_fHAngle, m_fKAngle, m_fAAngle;

        // derived
        Vector3f m_kK, m_kA, m_kF;
    };

    void InverseKinematics (Leg& rkLeg);

    const int m_iNumKeys;
    const float m_fHKLength, m_fKALength, m_fAFLength;
    const float m_fTMin, m_fTMax, m_fDt;

    Leg* m_akLeg;
    PosSpline* m_pkPosSpline;
    RotSpline* m_pkHRotSpline;
    RotSpline* m_pkKRotSpline;
    RotSpline* m_pkARotSpline;
    float m_fTime;
    bool m_bDrawKeys;
};

#endif
