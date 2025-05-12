// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef RIPPLINGOCEAN_H
#define RIPPLINGOCEAN_H

#include "WmlApplication.h"
#include "WmlVertexShader.h"
#include "WmlPixelShader.h"
#include "WmlVector4.h"

using namespace Wml;

class RipplingOcean : public Application
{
public:
    RipplingOcean ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void CreateRectangleMesh (TriMesh*& rpkMesh,
        const Vector3f& rkCenter, const Vector3f& rkU, const Vector3f& rkV,
        const Vector3f& rkAxis, float fUExtent, float fVExtent,
        int iUSamples, int iVSamples, bool bWantNormals, bool bWantColors, 
        bool bWantUVs);

    TriMesh* CreateRect (float fSize, float fDepth);

    void HeightToNormalMap (Image*& pkImage);
    void SetupShaders ();
    bool Setup ();

    NodePtr m_spkScene;
    NodePtr m_spkTrnNode;
    NodePtr m_spkModel;

    TriMeshPtr m_spkTriMesh;
    VertexShaderPtr m_spkVertShader;
    PixelShaderPtr m_spkPixShader;

    bool m_bVertexShader;
    bool m_bInitialized;

    bool m_bStopped;
    float m_fStopTime;

    // Wave info
    float m_fAmbient;
    float m_fTexRepeat;
    float m_fWaveSpeedFactor;
    float m_fWaveHeightFactor;
    float m_fRippleSpeedFactor;

    // tiled rendering
    float m_fN, m_fF, m_fL, m_fR, m_fT, m_fB;
};

#endif
