// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSPOTLIGHT_H
#define WMLSPOTLIGHT_H

#include "WmlPointLight.h"

namespace Wml
{

class WML_ITEM SpotLight : public PointLight
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    SpotLight ();

    virtual Type GetType () const;

    Vector3f& Direction ();
    float& Exponent ();
    void SetAngle (float fAngle);
    float GetAngle () const;

    virtual void ComputeDiffuse (const Matrix3f& rkWorldRotate,
        const Vector3f& rkWorldTranslate, float fWorldScale,
        const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
        const bool* abVisible, ColorRGB* akDiffuse);

    virtual void ComputeSpecular (const Matrix3f& rkWorldRotate,
        const Vector3f& rkWorldTranslate, float fWorldScale,
        const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
        const bool* abVisible, const Vector3f& rkCameraModelLocation,
        ColorRGB* akSpecular);

protected:
    Vector3f m_kDirection;
    float m_fAngle, m_fCosSqr, m_fSinSqr;
    float m_fExponent;
};

WmlSmartPointer(SpotLight);
WmlRegisterStream(SpotLight);
#include "WmlSpotLight.inl"

}

#endif
