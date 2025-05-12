// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLALPHASTATE_H
#define WMLALPHASTATE_H

#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(AlphaState);

class WML_ITEM AlphaState : public RenderState
{
    WmlDeclareDefaultState(AlphaState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    AlphaState ();
    virtual Type GetType () const;

    enum SrcBlendFunction
    {
        SBF_ZERO,
        SBF_ONE,
        SBF_DST_COLOR,
        SBF_ONE_MINUS_DST_COLOR,
        SBF_SRC_ALPHA,
        SBF_ONE_MINUS_SRC_ALPHA,
        SBF_DST_ALPHA,
        SBF_ONE_MINUS_DST_ALPHA,
        SBF_SRC_ALPHA_SATURATE,
        SBF_QUANTITY
    };

    enum DstBlendFunction
    {
        DBF_ZERO,
        DBF_ONE,
        DBF_SRC_COLOR,
        DBF_ONE_MINUS_SRC_COLOR,
        DBF_SRC_ALPHA,
        DBF_ONE_MINUS_SRC_ALPHA,
        DBF_DST_ALPHA,
        DBF_ONE_MINUS_DST_ALPHA,
        DBF_QUANTITY
    };

    enum TestFunction
    {
        TF_NEVER,
        TF_LESS,
        TF_EQUAL,
        TF_LEQUAL,
        TF_GREATER,
        TF_NOTEQUAL,
        TF_GEQUAL,
        TF_ALWAYS,
        TF_QUANTITY
    };

    bool& BlendEnabled ();          // default: false
    SrcBlendFunction& SrcBlend ();  // default: SBF_SRC_ALPHA
    DstBlendFunction& DstBlend ();  // default: DBF_ONE_MINUS_SRC_ALPHA
    bool& TestEnabled ();           // default: false;
    TestFunction& Test ();          // default: TF_ALWAYS
    float& Reference ();            // default: 0

protected:
    bool m_bBlendEnabled;
    SrcBlendFunction m_eSrcBlend;
    DstBlendFunction m_eDstBlend;

    bool m_bTestEnabled;
    TestFunction m_eTest;
    float m_fReference;  // in [0,1]
};

WmlRegisterStream(AlphaState);
#include "WmlAlphaState.inl"

}

#endif


