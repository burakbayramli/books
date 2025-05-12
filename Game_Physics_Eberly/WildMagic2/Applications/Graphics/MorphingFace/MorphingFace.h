// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef MORPHINGFACE_H
#define MORPHINGFACE_H

#include "WmlApplication.h"
using namespace Wml;

class MorphingFace : public Application
{
public:
    MorphingFace ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void DrawStatistics (int iX, int iY, const ColorRGB& rkColor);

    // morph scene graph
    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframeState;

    // animation time
    float m_fBaseTime;
    float m_fCurrTime;

    bool m_bInitialized;
};

#endif
