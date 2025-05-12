// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef CLODFACE_H
#define CLODFACE_H

#include "WmlApplication.h"
using namespace Wml;

class ClodFace : public Application
{
public:
    ClodFace ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void DrawTriangleQuantity (int iX, int iY, const ColorRGB& rkColor);

    // turret-based camera motion
    virtual void MoveForward ();
    virtual void MoveBackward ();

    // model is a face mesh
    NodePtr m_spkScene;
    ClodMeshPtr m_spkClod;
    WireframeStatePtr m_spkWireframeState;

    bool m_bInitialized;
};

#endif
