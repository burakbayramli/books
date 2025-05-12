// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef RIGIDBALL_H
#define RIGIDBALL_H

#include "WmlRigidBody.h"
#include "WmlTriMesh.h"
using namespace Wml;

class RigidBall : public RigidBodyf
{
public:
    RigidBall (float fRadius = 1.0f);
    virtual ~RigidBall ();

    TriMeshPtr& Mesh ();
    float GetRadius () const;

private:
    TriMeshPtr m_spkMesh;
    float m_fRadius;
};

#endif
