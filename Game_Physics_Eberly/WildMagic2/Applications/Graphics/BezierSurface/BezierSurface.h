// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef BEZIERSURFACE_H
#define BEZIERSURFACE_H

#include "WmlApplication.h"
using namespace Wml;

class BezierSurface : public Application
{
public:
    BezierSurface ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);

protected:
    BezierMesh* CreateRectangleMesh (int iDegree, bool bUseNormals,
        bool bUseColors, bool bUseTextures);
    BezierMesh* CreateTriangleMesh (int iDegree, bool bUseNormals,
        bool bUseColors, bool bUseTextures);
    BezierMesh* CreateCylinderMesh (int iDegree, bool bUseNormals,
        bool bUseColors, bool bUseTextures);

    NodePtr m_spkScene;
    BezierMeshPtr m_spkMesh;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;
    TextureStatePtr m_spkTextureState;
    TexturePtr m_spkTexture;
};

#endif
