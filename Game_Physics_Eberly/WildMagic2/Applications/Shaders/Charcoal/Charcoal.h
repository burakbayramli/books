// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef SHADERDEMO_H
#define SHADERDEMO_H

#include "WmlApplication.h"
#include "WmlVertexShader.h"
#include "WmlPixelShader.h"
#include "WmlDirectionalLight.h"

using namespace Wml;

class Charcoal : public Application
{
public:
    Charcoal ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:

    Image* ContrastImage (int iWidth, int iHeight, double dNoiseDensity, 
        double dContrastExponent);
    Image* RandomImage (int iWidth, int iHeight);
    void AttachShader(Node* pkNode);
    void UpdateConstants(Node* pkNode);
    TriMesh* CreateSquare (float fSize, float fDepth);

    bool Setup ();

    NodePtr m_spkScene;
    NodePtr m_spkTrnNode;
    NodePtr m_spkModel;

    TriMeshPtr m_spkTriMesh;
    VertexShaderPtr m_spkVertShader;
    PixelShaderPtr m_spkPixShader;

    float m_fCycle;
    double m_dContrastExponent;

    DirectionalLight* m_pkLight;
    float m_fStopTime;

    bool m_bVertexShader;
    bool m_bInitialized;
    bool m_bRunning;

    bool m_bDisplayLighting;
    bool m_bDisplayPaper;
    bool m_bSmudge;
};

#endif

