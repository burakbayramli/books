// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef CAMERAANDLIGHTNODES_H
#define CAMERAANDLIGHTNODES_H

#include "WmlApplication.h"
using namespace Wml;

class CameraAndLightNodes : public Application
{
public:
    CameraAndLightNodes ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    CameraNode* CreateCameraNode ();
    BezierMesh* CreateLightTarget (Light* pkLight);
    Node* CreateLightFixture (LightPtr& rspkAdjustableLight);
    TriMesh* CreateGround ();
    Node* CreateSceneNode ();
    void CreateScreenPolygon ();

    void GoForward ();
    void GoBackward ();
    void GoLeft ();
    void GoRight ();
    bool CameraMoved ();

    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframeState;
    CameraNodePtr m_spkCNode;
    ScreenPolygonPtr m_spkSky;
    LightPtr m_spkAdjustableLight0, m_spkAdjustableLight1;

    // mesh parameters
    float m_fMax, m_fHeight;

    Image* m_pkRedsky;
    Image* m_pkGround;
    bool m_bInitialized;
};

#endif
