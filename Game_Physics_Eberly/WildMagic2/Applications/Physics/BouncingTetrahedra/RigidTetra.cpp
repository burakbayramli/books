// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "RigidTetra.h"

//----------------------------------------------------------------------------
RigidTetra::RigidTetra (float fSize, float fMass, const Vector3f& rkPos,
    const Vector3f& rkLinMom, const Vector3f& rkAngMom)
{
    // geometry
    Vector3f* akVertex = new Vector3f[4];
    akVertex[0] = -(fSize/3.0f)*Vector3f(1.0f,1.0f,1.0f);
    akVertex[1] = Vector3f(+fSize,0.0f,0.0f);
    akVertex[2] = Vector3f(0.0f,+fSize,0.0f);
    akVertex[3] = Vector3f(0.0f,0.0f,+fSize);

    ColorRGB* akColor = new ColorRGB[4];
    akColor[0] = ColorRGB(1.0f,1.0f,1.0f);
    akColor[1] = ColorRGB(1.0f,0.0f,0.0f);
    akColor[2] = ColorRGB(0.0f,1.0f,0.0f);
    akColor[3] = ColorRGB(0.0f,0.0f,1.0f);

    int* aiConnect = new int[12];
    aiConnect[ 0] = 0; aiConnect[ 1] = 2; aiConnect[ 2] = 1;
    aiConnect[ 3] = 0; aiConnect[ 4] = 3; aiConnect[ 5] = 2;
    aiConnect[ 6] = 0; aiConnect[ 7] = 1; aiConnect[ 8] = 3;
    aiConnect[ 9] = 1; aiConnect[10] = 2; aiConnect[11] = 3;

    m_spkMesh = new TriMesh(4,akVertex,NULL,akColor,NULL,4,aiConnect);

    // inertia tensor
    Matrix3f kInertia;
    int i, j;
    for (i = 0; i < 3; i++)
    {
        kInertia[i][i] = 0.0f;
        for (j = 0; j < 3; j++)
        {
            if ( i != j )
            {
                kInertia[i][j] = 0.0f;
                for (int k = 0; k < 4; k++)
                {
                    kInertia[i][i] += 0.25f*fMass * akVertex[k][j] *
                        akVertex[k][j];
                    kInertia[i][j] -= 0.25f*fMass * akVertex[k][i] *
                        akVertex[k][j];
                }
            }
        }
    }

    // compute the radius of a sphere bounding the tetrahedron
    Vector3f kCentroid = (fSize/6.0f)*Vector3f(1.0f,1.0f,1.0f);
    m_fRadius = 0.0f;
    for (j = 0; j < 4; j++)
    {
        akVertex[j] -= kCentroid;
        float fTemp = (akVertex[j]-kCentroid).Length();
        if ( fTemp > m_fRadius )
            m_fRadius = fTemp;
    }

    SetMass(fMass);
    SetBodyInertia(kInertia);
    SetPosition(rkPos);
    SetQOrientation(Quaternionf::IDENTITY);
    SetLinearMomentum(rkLinMom);
    SetAngularMomentum(rkAngMom);
}
//----------------------------------------------------------------------------
RigidTetra::~RigidTetra ()
{
    m_spkMesh = NULL;
}
//----------------------------------------------------------------------------
TriMeshPtr RigidTetra::Mesh () const
{
    return m_spkMesh;
}
//----------------------------------------------------------------------------
float RigidTetra::GetRadius () const
{
    return m_fRadius;
}
//----------------------------------------------------------------------------
void RigidTetra::GetVertices (Vector3f* akVertex) const
{
    // Do not move the boundaries.  The hard-coded numbers here depend on
    // those of the floor/walls in the application.
    Vector3f kPos;
    if ( 0.0f < m_kPos.X() && m_kPos.X() < 20.0f )
        kPos = m_kPos;
    else
        kPos = Vector3f::ZERO;

    // move the tetra vertices
    Vector3f* akMVertex = m_spkMesh->Vertices();
    for (int i = 0; i < 4; i++)
        akVertex[i] = m_kROrient*akMVertex[i] + kPos;
}
//----------------------------------------------------------------------------
