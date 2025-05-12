// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef HELIXTUBESURFACE_H
#define HELIXTUBESURFACE_H

#include "WmlApplication.h"
#include "WmlNaturalSpline3.h"
using namespace Wml;

class HelixTubeSurface : public Application
{
public:
    HelixTubeSurface ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnSpecialKeyDown (int iKey, int iXPos, int iYPos);

protected:
    Image* CreateImageFromBMP (const char* acFilename);
    MultipleCurve3f* CreateCurve ();
    static float Radial (float fT);
    virtual bool MoveCamera ();

    // textures
    TexturePtr m_spkTexture;

    // render states
    TextureStatePtr m_spkTextureState;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;

    // test model
    NodePtr m_spkScene;
    MultipleCurve3f* m_pkCurve;
    float m_fMinCurveTime, m_fMaxCurveTime, m_fCurvePeriod;
    float m_fCurveTime, m_fDeltaTime;

    bool m_bInitialized;

    // tiled rendering
    float m_fN, m_fF, m_fL, m_fR, m_fT, m_fB;
};

#endif
