// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef PLANARREFLECTIONS_H
#define PLANARREFLECTIONS_H

#include "WmlApplication.h"
using namespace Wml;

class PlanarReflections : public Application
{
public:
    PlanarReflections ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    void CreateScreenPolygon ();
    bool LoadBiped ();
    void CreatePlane ();
    void CreatePlanarReflectionNode ();

    NodePtr m_spkScene;
    NodePtr m_spkBiped;
    TriMeshPtr m_spkPlane;
    PlanarReflectionPtr m_spkReflectionNode;

    ZBufferStatePtr m_spkZBuffer;
    WireframeStatePtr m_spkWireframe;

    // for animating biped and texture coordinates
    float m_fTime, m_fDTime, m_fSkyDU, m_fPlaneDV;
    Vector2f* m_akSkyUV;
    Vector2f* m_akPlaneUV;

    // sky background
    ScreenPolygonPtr m_spkSky;

    Image* m_pkBluewater;
    Image* m_pkRedsky;
    bool m_bInitialized;
};

#endif
