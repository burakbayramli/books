// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef SKINNING_H
#define SKINNING_H

#include "WmlApplication.h"
using namespace Wml;

class Skinning : public Application
{
public:
    Skinning ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    TriMesh* CreateCylinder (int iRadialSamples, const Vector3f& rkCenter, 
        const Vector3f& rkU, const Vector3f& rkV, const Vector3f& rkAxis, 
        float fRadius, float fHeight, bool bWantNormals, bool bWantColors,
        bool bOutsideView);
    bool Setup ();
    void DoUpdate ();

    NodePtr m_spkScene;
    NodePtr m_spkTrnNode;
    NodePtr m_spkModel;
    WireframeStatePtr m_spkWireframe;

    TriMeshPtr m_spkTriMesh;
    VertexShaderPtr m_spkVertShader;
    PixelShaderPtr m_spkPixShader;
    
    Matrix4f m_akSkinMat[4];
    float m_fTime;

    bool m_bShaderEnabled;
    bool m_bInitialized;

    // tiled rendering
    float m_fN, m_fF, m_fL, m_fR, m_fT, m_fB;
};

#endif
