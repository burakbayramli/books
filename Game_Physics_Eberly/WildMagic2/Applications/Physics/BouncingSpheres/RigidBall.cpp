// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "RigidBall.h"
#include "WmlStandardMesh.h"
#include "WmlTextureState.h"

//----------------------------------------------------------------------------
RigidBall::RigidBall (float fRadius)
{
    m_fRadius = fRadius;

    TriMesh* pkMesh = NULL;
    CreateSphereMesh(pkMesh,16,16,Vector3f::ZERO,m_fRadius,Vector3f::UNIT_X,
        Vector3f::UNIT_Y,Vector3f::UNIT_Z,false,false,true,true);
    m_spkMesh = pkMesh;

    Texture* pkTexture = new Texture;
    pkTexture->SetImage(Image::Load("BallTexture.mif"));
    pkTexture->Filter() = Texture::FM_LINEAR;
    pkTexture->Mipmap() = Texture::MM_LINEAR;
    TextureState* pkTS = new TextureState;
    pkTS->Set(0,pkTexture);
    m_spkMesh->SetRenderState(pkTS);
}
//----------------------------------------------------------------------------
RigidBall::~RigidBall ()
{
    m_spkMesh = NULL;
}
//----------------------------------------------------------------------------
float RigidBall::GetRadius () const
{ 
    return m_fRadius; 
}
//----------------------------------------------------------------------------
TriMeshPtr& RigidBall::Mesh ()
{
    return m_spkMesh;
}
//----------------------------------------------------------------------------
