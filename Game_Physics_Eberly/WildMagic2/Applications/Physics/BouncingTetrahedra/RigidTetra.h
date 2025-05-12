// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#ifndef RIGIDTETRA_H
#define RIGIDTETRA_H

#include "WmlRigidBody.h"
#include "WmlTriMesh.h"
using namespace Wml;

class RigidTetra : public RigidBodyf
{
public:
    RigidTetra (float fSize, float fMass, const Vector3f& rkPos,
        const Vector3f& rkLinMom, const Vector3f& rkAngMom);
    virtual ~RigidTetra ();

    TriMeshPtr Mesh () const;
    float GetRadius () const;
    void GetVertices (Vector3f* akVertex) const;

private:
    TriMeshPtr m_spkMesh;
    float m_fRadius;
};

#endif
