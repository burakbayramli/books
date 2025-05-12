// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLWIREFRAMESTATE_H
#define WMLWIREFRAMESTATE_H

#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(WireframeState);

class WML_ITEM WireframeState : public RenderState
{
    WmlDeclareDefaultState(WireframeState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    WireframeState ();
    virtual Type GetType () const;

    bool& Enabled ();  // default: false

protected:
    bool m_bEnabled;
};

WmlRegisterStream(WireframeState);
#include "WmlWireframeState.inl"

}

#endif
