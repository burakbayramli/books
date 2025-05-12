// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef VERTEXNOISE_H
#define VERTEXNOISE_H

#include "WmlApplication.h"
using namespace Wml;

class VertexNoise : public Application
{
public:
    static const int TABLE_SIZE;

    VertexNoise ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void InitVertexNoiseConstants ();
    bool Setup ();
    void UpdateParameters ();

    NodePtr m_spkScene;
    NodePtr m_spkTrnNode;
    NodePtr m_spkModel;
    WireframeStatePtr m_spkWireframe;

    TriMeshPtr m_spkTriMesh;
    VertexShaderPtr m_spkVertShader;
    PixelShaderPtr m_spkPixShader;

    bool m_bVertexShader;
    bool m_bInitialized;

    float m_fDisplacement;
    Vector4f m_kNoiseTrans;
    float m_fNoiseScale;

    // tiled rendering
    float m_fN, m_fF, m_fL, m_fR, m_fT, m_fB;
};

#endif
