// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSHADESTATE_H
#define WMLSHADESTATE_H

#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(ShadeState);

class WML_ITEM ShadeState : public RenderState
{
    WmlDeclareDefaultState(ShadeState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    ShadeState ();
    virtual Type GetType () const;

    enum ShadeMode
    {
        SM_FLAT,
        SM_SMOOTH,
        SM_QUANTITY
    };

    ShadeMode& Shade ();  // default: SM_SMOOTH

protected:
    ShadeMode m_eShade;
};

WmlRegisterStream(ShadeState);
#include "WmlShadeState.inl"

}

#endif
