// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef PORTALS_H
#define PORTALS_H

#include "WmlApplication.h"
using namespace Wml;

class Portals : public Application
{
public:
    Portals ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    static void CreateCenterCube (TextureStatePtr& rspkFloor,
        TextureStatePtr& rspkCeiling, TextureStatePtr& rspkWall,
        TextureStatePtr& rspkPicture0, TextureStatePtr& rspkPicture1,
        TextureStatePtr& rspkPicture2, TextureStatePtr& rspkPicture3,
        Node*& rpkCube, Portal**& rapkPortal);

    static void CreateAxisConnector (TextureStatePtr& rspkFloor,
        TextureStatePtr& rspkCeiling, TextureStatePtr& rspkWall,
        Node*& rpkCube, Portal**& rapkPortal);

    static void CreateEndCube (TextureStatePtr& rspkFloor,
        TextureStatePtr& rspkCeiling, TextureStatePtr& rspkWall,
        Node*& rpkCube, Portal**& rapkPortal);

    static void CreateDiagonalConnector (TextureStatePtr& rspkFloor,
        TextureStatePtr& rspkCeiling, TextureStatePtr& rspkWall,
        Node*& rpkCube, Portal**& rapkPortal);

    TriMesh* CreateOutside ();

    static Image* CreateImageFromBMP (const char* acFilename);
    bool InitializeTextures ();

    ConvexRegionManager* CreateBspTree ();

    NodePtr m_spkScene;
    ZBufferStatePtr m_spkZBuffer;
    WireframeStatePtr m_spkWireframe;
    TextureStatePtr m_spkTextureAgate;
    TextureStatePtr m_spkTextureBark;
    TextureStatePtr m_spkTextureBuff;
    TextureStatePtr m_spkTextureConnector;
    TextureStatePtr m_spkTextureDave;
    TextureStatePtr m_spkTextureMunch;
    TextureStatePtr m_spkTexturePebbles;
    TextureStatePtr m_spkTextureQuartz;
    TextureStatePtr m_spkTextureSand;
    TextureStatePtr m_spkTextureShelly;
    TextureStatePtr m_spkTextureSky;
    TextureStatePtr m_spkTextureWater;

    bool m_bInitialized;
};

#endif
