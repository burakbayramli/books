// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef IRIDESCENCE_H
#define IRIDESCENCE_H

#include "WmlApplication.h"
#include "WmlVertexShader.h"
#include "WmlPixelShader.h"
using namespace Wml;

class Iridescence : public Application
{
public:
    Iridescence ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    bool Setup ();
    void DoUpdate ();

    NodePtr m_spkScene;
    NodePtr m_spkTrnNode;
    NodePtr m_spkModel;

    TriMeshPtr m_spkTriMesh;
    VertexShaderPtr m_spkVertShader;
    PixelShaderPtr m_spkPixShader;

    bool m_bVertexShader;
    bool m_bInitialized;

    float m_fInterpolateFactor;

    // tiled rendering
    float m_fN, m_fF, m_fL, m_fR, m_fT, m_fB;
};

#endif
