// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef TERRAIN_H
#define TERRAIN_H

#include "WmlApplication.h"
using namespace Wml;

class Terrain : public Application
{
public:
    Terrain ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    bool CreateTerrain ();
    void Simplify ();
    void DrawStatistics (int iX, int iY, const ColorRGB& rkColor);

    // turret-based camera motion
    virtual void MoveForward ();
    virtual void MoveBackward ();
    virtual void MoveUp ();
    virtual void MoveDown ();
    virtual void TurnLeft ();
    virtual void TurnRight ();
    virtual void LookUp ();
    virtual void LookDown ();

    // render states
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;

    // terrain model
    NodePtr m_spkScene;
    TerrainPagePtr m_spkPage;

    // terrain motion
    float m_fHeightAboveTerrain;

    bool m_bInitialized;
};

#endif
