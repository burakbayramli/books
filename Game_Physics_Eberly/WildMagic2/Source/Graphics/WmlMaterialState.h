// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMATERIALSTATE_H
#define WMLMATERIALSTATE_H

#include "WmlColorRGB.h"
#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(MaterialState);

class WML_ITEM MaterialState : public RenderState
{
    WmlDeclareDefaultState(MaterialState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    MaterialState ();
    virtual Type GetType () const;

    ColorRGB& Emissive ();  // default:  ColorRGB(0,0,0)
    ColorRGB& Ambient ();   // default:  ColorRGB(0.2,0.2,0.2)
    ColorRGB& Diffuse ();   // default:  ColorRGB(0.8,0.8,0.8)
    ColorRGB& Specular ();  // default:  ColorRGB(0,0,0)
    float& Shininess ();    // default:  0
    float& Alpha ();        // default:  1

protected:
    ColorRGB m_kEmissive;
    ColorRGB m_kAmbient;
    ColorRGB m_kDiffuse;
    ColorRGB m_kSpecular;
    float m_fShininess;
    float m_fAlpha;
};

WmlRegisterStream(MaterialState);
#include "WmlMaterialState.inl"

}

#endif
