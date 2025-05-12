// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef POLYHEDRONDISTANCE_H
#define POLYHEDRONDISTANCE_H

#include "WmlApplication2.h"
#include "WmlLCPPolyDist.h"
using namespace Wml;

class PolyhedronDistance : public Application
{
public:
    PolyhedronDistance ();

    virtual bool OnInitialize ();
    virtual void OnTerminate ();
    virtual void OnIdle ();
    virtual void OnKeyDown (unsigned char ucKey, int iX, int iY);

protected:
    TriMesh* CreateTetra (float fSize, int iColor);
    Polyline* CreateLine ();
    TriMesh* CreatePlane ();
    void UpdateLine ();
    void InitialState ();
    bool Transform (unsigned char ucKey);

    // representation of bodies
    Tuple<3>* m_akFace;
    TriMesh* m_apkTetra[4];
    Vector3f* m_apkVertex[2];
    Vector3f m_akTV[2];
    Matrix3f m_akTM[2];

    // display variables
    float m_fSeparation;
    float m_fEdgeLength;
    float m_fSmall;

    // Offsets during calculation with LCPPolyDist to ensure that all of the
    // vertices are in the first octant.
    float m_fOffset;
 
    // the scene graph
    NodePtr m_spkScene;
    WireframeStatePtr m_spkWireframe;
    ZBufferStatePtr m_spkZBuffer;
    PolylinePtr m_aspkLine[2];
};

#endif
