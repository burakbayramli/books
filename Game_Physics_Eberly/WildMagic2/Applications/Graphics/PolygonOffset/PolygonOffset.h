// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef POLYGONOFFSET_H
#define POLYGONOFFSET_H

#include "WmlApplication.h"
using namespace Wml;

class PolygonOffset : public Application
{
public:
    PolygonOffset ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void CreateScene ();

    NodePtr m_spkScene;
    ZBufferStatePtr m_spkZBuffer;
    WireframeStatePtr m_spkWireframe;
};

#endif
