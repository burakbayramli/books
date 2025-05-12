// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef INTERSECTINGCYLINDERS_H
#define INTERSECTINGCYLINDERS_H

#include "WmlApplication.h"
using namespace Wml;

class IntersectingCylinders : public Application
{
public:
    IntersectingCylinders ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    // camera and scene graph construction
    TriMesh* CreateCylinder (float fRadius, float fHeight,
        int iRadialSamples, int iHeightSamples, bool bBlue);

    void CreateCamera ();
    void CreateScene ();

    NodePtr m_spkScene;
    TriMeshPtr m_spkCyln0, m_spkCyln1;
    WireframeStatePtr m_spkWireframe;

    // collision system
    CollisionGroup m_kGroup;
    bool Transform (unsigned char ucKey);
    void ResetColors ();
    static void Response (CollisionRecord& rkRecord0, int iT0,
        CollisionRecord& rkRecord1, int iT1, void*);

    static Vector2f ms_kBlueUV, ms_kRedUV, ms_kCyanUV, ms_kYellowUV;
};

#endif


