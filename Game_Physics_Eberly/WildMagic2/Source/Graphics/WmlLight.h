// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLLIGHT_H
#define WMLLIGHT_H

#include "WmlColorRGB.h"
#include "WmlMatrix3.h"
#include "WmlObject.h"

namespace Wml
{

class WML_ITEM Light : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    virtual ~Light ();

    enum Type
    {
        LT_AMBIENT,
        LT_DIRECTIONAL,
        LT_POINT,
        LT_SPOT,
        LT_QUANTITY
    };

    virtual Type GetType () const = 0;

    ColorRGB& Ambient ();   // default: ColorRGB(0,0,0)
    ColorRGB& Diffuse ();   // default: ColorRGB(0,0,0)
    ColorRGB& Specular ();  // default: ColorRGB(0,0,0)
    bool& Attenuate ();     // default: false
    float& Constant ();     // default: 1
    float& Linear ();       // default: 0
    float& Quadratic ();    // default: 0
    bool& On ();            // default: true
    float& Intensity ();    // default: 1

    // Draw the specular highlight after texture so that it appears as the
    // true specular color rather than tinted by the texture color.
    bool& SpecularAfterTexture ();  // default = false;

    virtual void ComputeDiffuse (const Matrix3f& rkWorldRotate,
        const Vector3f& rkWorldTranslate, float fWorldScale,
        const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
        const bool* abVisible, ColorRGB* akDiffuse) = 0;

    virtual void ComputeSpecular (const Matrix3f& rkWorldRotate,
        const Vector3f& rkWorldTranslate, float fWorldScale,
        const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
        const bool* abVisible, const Vector3f& rkCameraModelLocation,
        ColorRGB* akSpecular) = 0;

protected:
    // abstract base class
    Light ();

    ColorRGB m_kAmbient;
    ColorRGB m_kDiffuse;
    ColorRGB m_kSpecular;
    float m_fConstant;
    float m_fLinear;
    float m_fQuadratic;
    float m_fIntensity;
    bool m_bAttenuate;
    bool m_bOn;
    bool m_bSpecularAfterTexture;
};

WmlSmartPointer(Light);
WmlRegisterStream(Light);
#include "WmlLight.inl"

}

#endif
