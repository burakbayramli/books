// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLFOGSTATE_H
#define WMLFOGSTATE_H

#include "WmlColorRGB.h"
#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(FogState);

class WML_ITEM FogState : public RenderState
{
    WmlDeclareDefaultState(FogState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    FogState ();
    virtual Type GetType () const;

    enum DensityFunction
    {
        DF_LINEAR,
        DF_EXP,
        DF_EXPSQR,
        DF_QUANTITY
    };

    DensityFunction& DFunction ();

    enum ApplyFunction
    {
        AF_PER_VERTEX,
        AF_PER_PIXEL,
        AF_QUANTITY
    };

    bool& Enabled ();             // default: false
    float& Start ();              // default: 0
    float& End ();                // default: 1
    float& Density ();            // default: 1
    ColorRGB& Color ();           // default: ColorRGB(0,0,0)
    ApplyFunction& AFunction ();  // default: AF_PER_VERTEX

protected:
    bool m_bEnabled;
    float m_fStart, m_fEnd, m_fDensity;
    ColorRGB m_kColor;
    DensityFunction m_eDFunction;
    ApplyFunction m_eAFunction;
};

WmlRegisterStream(FogState);
#include "WmlFogState.inl"

}

#endif
