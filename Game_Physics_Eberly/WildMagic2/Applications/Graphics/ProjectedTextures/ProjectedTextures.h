// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef PROJECTEDTEXTURES_H
#define PROJECTEDTEXTURES_H

#include "WmlApplication.h"
using namespace Wml;

class ProjectedTextures : public Application
{
public:
    ProjectedTextures ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    bool LoadFace ();
    bool Setup ();

    NodePtr m_spkScene;
    NodePtr m_spkTrnNode;
    NodePtr m_spkModel;
    ProjectedTexturePtr m_spkPTexture;

    bool m_bInitialized;
};

#endif
