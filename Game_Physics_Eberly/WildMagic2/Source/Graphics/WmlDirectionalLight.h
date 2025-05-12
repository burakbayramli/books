// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLDIRECTIONALLIGHT_H
#define WMLDIRECTIONALLIGHT_H

#include "WmlLight.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM DirectionalLight : public Light
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    DirectionalLight ();

    virtual Type GetType () const;

    Vector3f& Direction ();

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
};

WmlSmartPointer(DirectionalLight);
WmlRegisterStream(DirectionalLight);
#include "WmlDirectionalLight.inl"

}

#endif
