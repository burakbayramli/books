// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCULLSTATE_H
#define WMLCULLSTATE_H

#include "WmlRenderState.h"

namespace Wml
{

WmlSmartPointer(CullState);

class WML_ITEM CullState : public RenderState
{
    WmlDeclareDefaultState(CullState);
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    CullState ();
    virtual Type GetType () const;

    enum FrontType
    {
        FT_CCW,  // front faces are counter-clockwise ordered
        FT_CW,   // front faces clockwise ordered
        FT_QUANTITY
    };

    enum CullType
    {
        CT_FRONT,  // cull front-facing triangles
        CT_BACK,   // cull back-facing triangles
        CT_QUANTITY
    };

    bool& Enabled ();         // default: true
    FrontType& FrontFace ();  // default: FT_CCW
    CullType& CullFace ();    // default: CT_BACK

protected:
    bool m_bEnabled;
    FrontType m_eFrontFace;
    CullType m_eCullFace;
};

WmlRegisterStream(CullState);
#include "WmlCullState.inl"

}

#endif
