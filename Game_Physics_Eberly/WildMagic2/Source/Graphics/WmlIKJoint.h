// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLIKJOINT_H
#define WMLIKJOINT_H

#include "WmlObject.h"
#include "WmlVector3.h"

namespace Wml
{

class IKGoal;
class Spatial;

class WML_ITEM IKJoint : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    IKJoint (Spatial* pkObject = 0);

    bool& AllowTranslation (int i);
    void SetTranslationRange (int i, float fMin, float fMax);
    float GetTranslationMin (int i) const;
    float GetTranslationMax (int i) const;
    float& TranslationSpringDamp (int i);

    bool& AllowRotation (int i);
    void SetRotationRange (int i, float fMin, float fMax);
    float GetRotationMin (int i) const;
    float GetRotationMax (int i) const;
    float& RotationSpringDamp (int i);

protected:
    // support for the IK update
    friend class IKController;

    // joint update
    Vector3f GetAxis (int i);
    void UpdateWorldSRT ();
    void UpdateWorldRT ();
    bool UpdateLocalT (int i, int iGoalQuantity, IKGoal** apkGoal,
        float& rfCurrentNorm);
    bool UpdateLocalR (int i, int iGoalQuantity, IKGoal** apkGoal,
        float& rfCurrentNorm);

    // indices:  X = 0, Y = 1, Z = 2
    Spatial* m_pkObject;
    bool m_abAllowTrn[3];
    float m_afMinTrn[3], m_afMaxTrn[3], m_afDampTrn[3];
    bool m_abAllowRot[3];
    float m_afMinRot[3], m_afMaxRot[3], m_afDampRot[3];

    static const float ms_fEpsilon;
};

WmlSmartPointer(IKJoint);
WmlRegisterStream(IKJoint);
#include "WmlIKJoint.inl"

}

#endif
