// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTextureState.h"
using namespace Wml;

WmlImplementRTTI(TextureState,RenderState);
WmlImplementStream(TextureState);
WmlImplementDefaultState(TextureState);

//----------------------------------------------------------------------------
RenderState::Type TextureState::GetType () const
{
    return RS_TEXTURE;
}
//----------------------------------------------------------------------------
TextureState::TextureState ()
{
}
//----------------------------------------------------------------------------
TextureState::~TextureState ()
{
    RemoveAll();
}
//----------------------------------------------------------------------------
void TextureState::Set (int i, Texture* pkTexture)
{
    assert( i < MAX_TEXTURES );
    if ( i >= MAX_TEXTURES )
        return;

    m_aspkTexture[i] = pkTexture;
}
//----------------------------------------------------------------------------
Texture* TextureState::Get (int i)
{
    assert( i < MAX_TEXTURES );
    if ( i >= MAX_TEXTURES )
        return NULL;

    return m_aspkTexture[i];
}
//----------------------------------------------------------------------------
TexturePtr TextureState::Remove (int i)
{
    assert( i < MAX_TEXTURES );
    if ( i >= MAX_TEXTURES )
        return 0;

    TexturePtr spSave = m_aspkTexture[i];
    m_aspkTexture[i] = NULL;
    return spSave;
}
//----------------------------------------------------------------------------
void TextureState::RemoveAll ()
{
    for (int i = 0; i < MAX_TEXTURES; i++)
        m_aspkTexture[i] = NULL;
}
//----------------------------------------------------------------------------
int TextureState::GetQuantity () const
{
    int iQuantity = 0;

    for (int i = 0; i < MAX_TEXTURES; i++)
    {
        if ( m_aspkTexture[i] )
            iQuantity++;
    }

    return iQuantity;
}
//----------------------------------------------------------------------------
Object* TextureState::GetObjectByName (const char* acName)
{
    Object* pkFound = RenderState::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    for (int i = 0; i < MAX_TEXTURES; i++)
    {
        if ( m_aspkTexture[i] )
        {
            pkFound = m_aspkTexture[i]->GetObjectByName(acName);
            if ( pkFound )
                return pkFound;
        }
    }

    return NULL;
}
//----------------------------------------------------------------------------
void TextureState::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    RenderState::GetAllObjectsByName(acName,rkObjects);

    for (int i = 0; i < MAX_TEXTURES; i++)
    {
        if ( m_aspkTexture[i] )
            m_aspkTexture[i]->GetAllObjectsByName(acName,rkObjects);
    }
}
//----------------------------------------------------------------------------
RenderState* TextureState::Extract (int iLastIndex, RenderState* apkState[])
{
    // Accumulate the textures in the stack into a single TextureState object.
    // Once the most recent pushed texture in a channel is found and copied
    // to the new texture state object, that channel's processing is complete.
    // That is, the last pushed texture in a channel is the one used for the
    // leaf texture state.
    TextureState* pkNewTState = new TextureState;
    for (int iIndex = iLastIndex; iIndex >= 0; iIndex--)
    {
        TextureState* pkTState = (TextureState*)apkState[iIndex];
        for (int i = 0; i < MAX_TEXTURES; i++)
        {
            if ( !pkNewTState->m_aspkTexture[i] )
                pkNewTState->m_aspkTexture[i] = pkTState->m_aspkTexture[i];
        }
    }
    return pkNewTState;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* TextureState::Factory (Stream& rkStream)
{
    TextureState* pkObject = new TextureState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void TextureState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // link data
    for (int i = 0; i < MAX_TEXTURES; i++)
    {
        Texture* pkTexture;
        StreamRead(rkStream,pkTexture);
        pkLink->Add(pkTexture);
    }
}
//----------------------------------------------------------------------------
void TextureState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);

    for (int i = 0; i < MAX_TEXTURES; i++)
    {
        Object* pkLinkID = pkLink->GetLinkID();
        m_aspkTexture[i] = (Texture*)rkStream.GetFromMap(pkLinkID);
    }
}
//----------------------------------------------------------------------------
bool TextureState::Register (Stream& rkStream)
{
    if ( !RenderState::Register(rkStream) )
        return false;

    for (int i = 0; i < MAX_TEXTURES; i++)
    {
        if ( m_aspkTexture[i] )
            m_aspkTexture[i]->Register(rkStream);
    }

    return true;
}
//----------------------------------------------------------------------------
void TextureState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // link data
    for (int i = 0; i < MAX_TEXTURES; i++)
        StreamWrite(rkStream,m_aspkTexture[i]);
}
//----------------------------------------------------------------------------
StringTree* TextureState::SaveStrings ()
{
    int i, iTQuantity = 0;
    for (i = 0; i < MAX_TEXTURES; i++)
    {
        if ( m_aspkTexture[i] )
            iTQuantity++;
    }

    StringTree* pkTree = new StringTree(1,0,iTQuantity+1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    int iSlot = 1;
    for (i = 0; i < MAX_TEXTURES; i++)
    {
        Texture* pkTexture = m_aspkTexture[i];
        if ( pkTexture )
            pkTree->SetChild(iSlot++,pkTexture->SaveStrings());
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int TextureState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(TextureState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int TextureState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        MAX_TEXTURES*sizeof(m_aspkTexture[0]);
}
//----------------------------------------------------------------------------
