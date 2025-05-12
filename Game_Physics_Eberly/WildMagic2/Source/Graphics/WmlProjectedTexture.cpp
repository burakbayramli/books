// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRenderer.h"
#include "WmlProjectedTexture.h"
using namespace Wml;

WmlImplementRTTI(ProjectedTexture,Node);
WmlImplementStream(ProjectedTexture);

//----------------------------------------------------------------------------
ProjectedTexture::ProjectedTexture (Node* pkObjects, Camera* pkCamera,
    Texture* pkTexture)
    :
    m_spkObjects(pkObjects),
    m_spkCamera(pkCamera),
    m_spkTexture(pkTexture)
{
    assert( pkObjects );

    // Save the cull state of the objects.  These objects will be force-culled
    // by 'this' to make sure they are not drawn by the regular traversal.
    m_bForceCullObjects = pkObjects->ForceCull();
    pkObjects->ForceCull() = true;

    // modify the texture so it will not undo the texture uv generation
    pkTexture->Envmap() = Texture::EM_IGNORE;

    m_spkTextureState = new TextureState;
}
//----------------------------------------------------------------------------
ProjectedTexture::ProjectedTexture ()
{
    m_bForceCullObjects = false;
}
//----------------------------------------------------------------------------
ProjectedTexture::~ProjectedTexture()
{
    // restore the cull state
    m_spkObjects->ForceCull() = m_bForceCullObjects;

    // release the render effect objects
    m_spkObjects = NULL;
    m_spkCamera = NULL;
    m_spkTexture = NULL;
    m_spkTextureState = NULL;
}
//----------------------------------------------------------------------------
void ProjectedTexture::UpdateWorldBound()
{
    m_kWorldBound = m_spkObjects->WorldBound();
}
//----------------------------------------------------------------------------
void ProjectedTexture::Draw (Renderer& rkRenderer)
{
    // restore the original cull state 
    m_spkObjects->ForceCull() = m_bForceCullObjects;

    rkRenderer.Draw(*this);

    // stop the regular traversal
    m_spkObjects->ForceCull() = true;
}
//----------------------------------------------------------------------------
Object* ProjectedTexture::GetObjectByName (const char* acName)
{
    Object* pkFound = Node::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    // The subtree m_spkObjects is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkCamera )
    {
        pkFound = m_spkCamera->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkTexture )
    {
        pkFound = m_spkTexture->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkTextureState )
    {
        pkFound = m_spkTextureState->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void ProjectedTexture::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Node::GetAllObjectsByName(acName,rkObjects);

    // The subtree m_spkObjects is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkCamera )
        m_spkCamera->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkTexture )
        m_spkTexture->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkTextureState )
        m_spkTextureState->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ProjectedTexture::Factory (Stream& rkStream)
{
    ProjectedTexture* pkObject = new ProjectedTexture;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ProjectedTexture::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // link data (objects, camera, texture, texture state, in that order)
    Object* pkObject;
    StreamRead(rkStream,pkObject);
    pkLink->Add(pkObject);
    StreamRead(rkStream,pkObject);
    pkLink->Add(pkObject);
    StreamRead(rkStream,pkObject);
    pkLink->Add(pkObject);
    StreamRead(rkStream,pkObject);
    pkLink->Add(pkObject);
}
//----------------------------------------------------------------------------
void ProjectedTexture::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkObjects = (Node*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkCamera = (Camera*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkTexture = (Texture*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkTextureState = (TextureState*)rkStream.GetFromMap(pkLinkID);

    // initialize the cull status for correct toggling during rendering
    m_bForceCullObjects = m_spkObjects->ForceCull();
    m_spkObjects->ForceCull() = true;
}
//----------------------------------------------------------------------------
bool ProjectedTexture::Register (Stream& rkStream)
{
    if ( !Node::Register(rkStream) )
        return false;

    if ( m_spkObjects )
        m_spkObjects->Register(rkStream);

    if ( m_spkCamera )
        m_spkCamera->Register(rkStream);

    if ( m_spkTexture )
        m_spkTexture->Register(rkStream);

    if ( m_spkTextureState )
        m_spkTextureState->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void ProjectedTexture::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // link data
    StreamWrite(rkStream,m_spkObjects);
    StreamWrite(rkStream,m_spkCamera);
    StreamWrite(rkStream,m_spkTexture);
    StreamWrite(rkStream,m_spkTextureState);
}
//----------------------------------------------------------------------------
StringTree* ProjectedTexture::SaveStrings ()
{
    int iCQuantity = 1;
    if ( m_spkObjects )
        iCQuantity++;
    if ( m_spkCamera )
        iCQuantity++;
    if ( m_spkTexture )
        iCQuantity++;
    if ( m_spkTextureState )
        iCQuantity++;

    StringTree* pkTree = new StringTree(1,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,Node::SaveStrings());
    int iSlot = 1;
    if ( m_spkObjects )
        pkTree->SetChild(iSlot++,m_spkObjects->SaveStrings());
    if ( m_spkCamera )
        pkTree->SetChild(iSlot++,m_spkCamera->SaveStrings());
    if ( m_spkTexture )
        pkTree->SetChild(iSlot++,m_spkTexture->SaveStrings());
    if ( m_spkTextureState )
        pkTree->SetChild(iSlot++,m_spkTextureState->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int ProjectedTexture::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ProjectedTexture) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ProjectedTexture::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_spkObjects) +
        sizeof(m_spkCamera) +
        sizeof(m_spkTexture) +
        sizeof(m_spkTextureState);
}
//----------------------------------------------------------------------------
