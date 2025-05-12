// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef FREEFORMDEFORMATION_H
#define FREEFORMDEFORMATION_H

#include "WmlApplication.h"
#include "WmlBSplineVolume.h"
using namespace Wml;

class FreeFormDeformation : public Application
{
public:
    FreeFormDeformation ();
    virtual ~FreeFormDeformation ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);
    virtual void OnMouseClick (int iButton, int iState, int iX, int iY,
        unsigned int uiModifiers);
    virtual void OnMotion (int iX, int iY, unsigned int uiModifiers);

protected:
    void Setup ();
    void CreateBSplineVolume ();
    void CreatePolylines ();
    void CreateControlBoxes ();

    void UpdateMesh ();
    void UpdatePolylines ();
    void UpdateControlBoxes ();

    void DoRandomControlPoints ();
    void OnMouseDown (int iX, int iY);
    void OnMouseMove (int iX, int iY);

    // the scene graph
    NodePtr m_spkScene, m_spkTrnNode;
    WireframeStatePtr m_spkWireframeState;
    ZBufferStatePtr m_spkZBufferState;
    TriMeshPtr m_spkMesh;

    // control volume for deformation
    int m_iQuantity, m_iDegree;
    BSplineVolumef* m_pkVolume;
    float m_fXMin, m_fYMin, m_fZMin, m_fDX, m_fDY, m_fDZ;
    Vector3f* m_akParameter;  // (u,v,w) for mesh vertices

    // Q control points per dimension, 3*Q^2*(Q-1) polylines to connect them
    NodePtr m_spkPolylineRoot;

    // toggle between automated random motion and user-adjusted controls
    bool m_bDoRandom;

    // random motion parameters
    float m_fAmplitude, m_fRadius, m_fLastUpdateTime;

    // user-adjusted controls
    NodePtr m_spkControlRoot;
    TriMeshPtr m_spkSelected;
    MaterialStatePtr m_spkControlActive, m_spkControlInactive;
    LightStatePtr m_spkControlLight;
    bool m_bMouseDown;
};

#endif

