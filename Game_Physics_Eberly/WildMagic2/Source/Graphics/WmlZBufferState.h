// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLZBUFFERSTATE_H
#define WMLZBUFFERSTATE_H

#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(ZBufferState);

class WML_ITEM ZBufferState : public RenderState
{
    WmlDeclareDefaultState(ZBufferState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    ZBufferState ();
    virtual Type GetType () const;

    enum CompareFunction
    {
        CF_NEVER,
        CF_LESS,
        CF_EQUAL,
        CF_LEQUAL,
        CF_GREATER,
        CF_NOTEQUAL,
        CF_GEQUAL,
        CF_ALWAYS,
        CF_QUANTITY
    };

    bool& Enabled ();             // default: false
    bool& Writeable ();           // default: false
    CompareFunction& Compare ();  // default: CF_ALWAYS

protected:
    bool m_bEnabled, m_bWriteable;
    CompareFunction m_eCompare;
};

WmlRegisterStream(ZBufferState);
#include "WmlZBufferState.inl"

}

#endif
