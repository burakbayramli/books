// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLKEYFRAMECONTROLLER_H
#define WMLKEYFRAMECONTROLLER_H

#include "WmlController.h"
#include "WmlQuaternion.h"

namespace Wml
{

class WML_ITEM KeyframeController : public Controller
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    KeyframeController ();
    virtual ~KeyframeController ();

    void SetTranslationQuantity (int iTQuantity);
    void SetTranslationTimes (float* afTTime);
    void SetTranslations (Vector3f* akTData);
    int GetTranslationQuantity () const;
    float* GetTranslationTimes () const;
    Vector3f* GetTranslations () const;

    void SetRotationQuantity (int iRQuantity);
    void SetRotationTimes (float* afRTime);
    void SetRotations (Quaternionf* akRData);
    int GetRotationQuantity () const;
    float* GetRotationTimes () const;
    Quaternionf* GetRotations () const;

    void SetScaleQuantity (int iSQuantity);
    void SetScaleTimes (float* afSTime);
    void SetScales (float* afSData);
    int GetScaleQuantity () const;
    float* GetScaleTimes () const;
    float* GetScales () const;

    // TO DO.  This is for sharing of the time values of the keys.  However,
    // the current implementation implicitly forces translation, rotation,
    // and scale keys to exist if sharing is enabled.  What should be the
    // case is that *if* translation keys exist, then the number of keys
    // should be the shared quantity, but it is okay if the number of
    // translation keys is zero.  Same argument for rotation keys or
    // scale keys.
    void SetSharedQuantity (int iQuantity);
    void SetSharedTimes (float* afTime);
    int GetSharedQuantity () const;
    float* GetSharedTimes () const;

    virtual bool Update (float fAppTime);

protected:
    static void GetKeyInfo (float fCtrlTime, int iQuantity, float* afTime,
        int& riLastIndex, float& rfNormTime, int& ri0, int& ri1);

    Vector3f GetTranslation (float fNormTime, int i0, int i1);
    Matrix3f GetRotation (float fNormTime, int i0, int i1);
    float GetScale (float fNormTime, int i0, int i1);

    int m_iTQuantity;
    float* m_afTTime;
    Vector3f* m_akTData;
    int m_iTLastIndex;

    int m_iRQuantity;
    float* m_afRTime;
    Quaternionf* m_akRData;
    int m_iRLastIndex;

    int m_iSQuantity;
    float* m_afSTime;
    float* m_afSData;
    int m_iSLastIndex;

    int m_iSharedQuantity;
    float* m_afSharedTime;
    int m_iSharedLastIndex;
};

WmlSmartPointer(KeyframeController);
WmlRegisterStream(KeyframeController);
#include "WmlKeyframeController.inl"

}

#endif
