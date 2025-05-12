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
#include "WmlGlossMap.h"
using namespace Wml;

WmlImplementRTTI(GlossMap,Node);
WmlImplementStream(GlossMap);

//----------------------------------------------------------------------------
GlossMap::GlossMap (Node* pkObjects, Texture* pkTexture, int iTexUnit)
    :
    m_spkObjects(pkObjects)
{
    assert( pkObjects );
    assert( iTexUnit < TextureState::MAX_TEXTURES );
    m_iTexUnit = iTexUnit;

    // Save the cull state of the objects.  These objects will be force-culled
    // by 'this' to make sure they are not drawn by the regular traversal.
    m_bForceCullObjects = pkObjects->ForceCull();
    pkObjects->ForceCull() = true;
    
    // create texture state
    m_spkTextureState = new TextureState;
    m_spkTextureState->Set(iTexUnit,pkTexture);

    // create alpha blending state
    m_spkAlphaState = new AlphaState;
    m_spkAlphaState->BlendEnabled() = true;
    m_spkAlphaState->TestEnabled() = false;
    m_spkAlphaState->SrcBlend() = AlphaState::SBF_ONE;
    m_spkAlphaState->DstBlend() = AlphaState::DBF_SRC_ALPHA;
}
//----------------------------------------------------------------------------
GlossMap::GlossMap ()
{
    m_iTexUnit = 0;
    m_bForceCullObjects = false;
}
//----------------------------------------------------------------------------
GlossMap::~GlossMap()
{
    // restore the cull state
    m_spkObjects->ForceCull() = m_bForceCullObjects;

    // release the render effect objects
    m_spkObjects = NULL;
    m_spkTextureState = NULL;
    m_spkAlphaState = NULL;
}
//----------------------------------------------------------------------------
void GlossMap::UpdateWorldBound()
{
    m_kWorldBound = m_spkObjects->WorldBound();
}
//----------------------------------------------------------------------------
void GlossMap::Draw (Renderer& rkRenderer)
{
    m_spkObjects->ForceCull() = m_bForceCullObjects;
    rkRenderer.Draw(*this);
    m_spkObjects->ForceCull() = true;
}
//----------------------------------------------------------------------------
Object* GlossMap::GetObjectByName (const char* acName)
{
    Object* pkFound = Node::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    // The subtree m_spkObjects is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkTextureState )
    {
        pkFound = m_spkTextureState->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    if ( m_spkAlphaState )
    {
        pkFound = m_spkAlphaState->GetObjectByName(acName);
        if ( pkFound )
            return pkFound;
    }

    return NULL;
}
//----------------------------------------------------------------------------
void GlossMap::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    Node::GetAllObjectsByName(acName,rkObjects);

    // The subtree m_spkObjects is not searched to avoid the possibility of
    // infinite recursion.

    if ( m_spkTextureState )
        m_spkTextureState->GetAllObjectsByName(acName,rkObjects);

    if ( m_spkAlphaState )
        m_spkAlphaState->GetAllObjectsByName(acName,rkObjects);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* GlossMap::Factory (Stream& rkStream)
{
    GlossMap* pkObject = new GlossMap;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void GlossMap::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iTexUnit);

    // link data (objects, texture state, alpha state, in that order)
    Spatial* pkChild;
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
    StreamRead(rkStream,pkChild);
    pkLink->Add(pkChild);
}
//----------------------------------------------------------------------------
void GlossMap::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Node::Link(rkStream,pkLink);

    Object* pkLinkID = pkLink->GetLinkID();
    m_spkObjects = (Node*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkTextureState = (TextureState*)rkStream.GetFromMap(pkLinkID);

    pkLinkID = pkLink->GetLinkID();
    m_spkAlphaState = (AlphaState*)rkStream.GetFromMap(pkLinkID);

    // initialize the cull status for correct toggling during rendering
    m_bForceCullObjects = m_spkObjects->ForceCull();
    m_spkObjects->ForceCull() = true;
}
//----------------------------------------------------------------------------
bool GlossMap::Register (Stream& rkStream)
{
    if ( !Node::Register(rkStream) )
        return false;

    if ( m_spkObjects )
        m_spkObjects->Register(rkStream);

    if ( m_spkTextureState )
        m_spkTextureState->Register(rkStream);

    if ( m_spkAlphaState )
        m_spkAlphaState->Register(rkStream);

    return true;
}
//----------------------------------------------------------------------------
void GlossMap::Save (Stream& rkStream)
{
    Node::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iTexUnit);

    // link data
    StreamWrite(rkStream,m_spkObjects);
    StreamWrite(rkStream,m_spkTextureState);
    StreamWrite(rkStream,m_spkAlphaState);
}
//----------------------------------------------------------------------------
StringTree* GlossMap::SaveStrings ()
{
    int iCQuantity = 1;
    if ( m_spkObjects )
        iCQuantity++;
    if ( m_spkTextureState )
        iCQuantity++;
    if ( m_spkAlphaState )
        iCQuantity++;

    StringTree* pkTree = new StringTree(2,0,iCQuantity,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("texture unit =",m_iTexUnit));

    // children
    pkTree->SetChild(0,Node::SaveStrings());
    int iSlot = 1;
    if ( m_spkObjects )
        pkTree->SetChild(iSlot++,m_spkObjects->SaveStrings());
    if ( m_spkTextureState )
        pkTree->SetChild(iSlot++,m_spkTextureState->SaveStrings());
    if ( m_spkAlphaState )
        pkTree->SetChild(iSlot++,m_spkAlphaState->SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int GlossMap::GetMemoryUsed () const
{
    int iBaseSize = sizeof(GlossMap) - sizeof(Node);
    int iTotalSize = iBaseSize + Node::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int GlossMap::GetDiskUsed () const
{
    return Node::GetDiskUsed() +
        sizeof(m_iTexUnit) +
        sizeof(m_spkObjects) +
        sizeof(m_spkTextureState) +
        sizeof(m_spkAlphaState);
}
//----------------------------------------------------------------------------
