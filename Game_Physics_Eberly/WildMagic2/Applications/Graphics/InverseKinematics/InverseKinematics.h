// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef INVERSEKINEMATICS_H
#define INVERSEKINEMATICS_H

#include "WmlApplication.h"
using namespace Wml;

class InverseKinematics : public Application
{
public:
    InverseKinematics ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    TriMesh* CreateCube ();
    Polyline* CreateLine ();
    TriMesh* CreatePlane ();
    void UpdateLine ();
    bool Transform (unsigned char ucKey);

    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;
    NodePtr m_spkTarget;
    PolylinePtr m_spkLine;
    NodePtr m_spkRoot;
    NodePtr m_spkEffector;
    IKController* m_pkIKCtrl;
};

#endif
