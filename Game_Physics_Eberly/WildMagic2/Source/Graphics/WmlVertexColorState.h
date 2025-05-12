// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLVERTEXCOLORSTATE_H
#define WMLVERTEXCOLORSTATE_H

#include "WmlColorRGB.h"
#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(VertexColorState);

class WML_ITEM VertexColorState : public RenderState
{
    WmlDeclareDefaultState(VertexColorState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    VertexColorState ();
    virtual Type GetType () const;

    enum SourceMode
    {
        SM_IGNORE,
        SM_EMISSIVE,
        SM_DIFFUSE,  // includes ambient + diffuse
        SM_QUANTITY
    };

    enum LightingMode
    {
        LM_EMISSIVE,
        LM_DIFFUSE,  // includes emissive + ambient + diffuse
        LM_QUANTITY
    };

    SourceMode& Source ();      // default: SM_IGNORE
    LightingMode& Lighting ();  // default: LM_DIFFUSE

protected:
    SourceMode m_eSource;
    LightingMode m_eLighting;
};

WmlRegisterStream(VertexColorState);
#include "WmlVertexColorState.inl"

}

#endif
