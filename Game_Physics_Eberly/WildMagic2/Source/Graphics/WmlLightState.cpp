// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLightState.h"
using namespace Wml;

WmlImplementRTTI(LightState,RenderState);
WmlImplementStream(LightState);
WmlImplementDefaultState(LightState);

//----------------------------------------------------------------------------
RenderState::Type LightState::GetType () const
{
    return RS_LIGHT;
}
//----------------------------------------------------------------------------
LightState::LightState ()
{
}
//----------------------------------------------------------------------------
LightState::~LightState ()
{
    DetachAll();
}
//----------------------------------------------------------------------------
int LightState::Attach (Light* pkLight)
{
    assert( pkLight );
    if ( !pkLight )
        return MAX_LIGHTS;

    // A light should not be attached twice.
    int i;
    for (i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] && m_aspkLight[i]->GetID() == pkLight->GetID() )
        {
            assert( false );
            return MAX_LIGHTS;
        }
    }

    for (i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] == NULL )
        {
            m_aspkLight[i] = pkLight;
            return i;
        }
    }

    return MAX_LIGHTS;
}
//----------------------------------------------------------------------------
int LightState::Detach (Light* pkLight)
{
    assert( pkLight );
    if ( !pkLight )
        return MAX_LIGHTS;

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] && m_aspkLight[i]->GetID() == pkLight->GetID() )
        {
            m_aspkLight[i] = NULL;
            return i;
        }
    }

    return MAX_LIGHTS;
}
//----------------------------------------------------------------------------
LightPtr LightState::Detach (int i)
{
    assert( i < MAX_LIGHTS );
    if ( i >= MAX_LIGHTS )
        return 0;

    LightPtr spSave = m_aspkLight[i];
    m_aspkLight[i] = NULL;
    return spSave;
}
//----------------------------------------------------------------------------
void LightState::DetachAll ()
{
    for (int i = 0; i < MAX_LIGHTS; i++)
        m_aspkLight[i] = NULL;
}
//----------------------------------------------------------------------------
int LightState::GetQuantity () const
{
    int iQuantity = 0;

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] )
            iQuantity++;
    }

    return iQuantity;
}
//----------------------------------------------------------------------------
Light* LightState::Get (int i)
{
    assert( i < MAX_LIGHTS );
    if ( i >= MAX_LIGHTS )
        return NULL;

    return m_aspkLight[i];
}
//----------------------------------------------------------------------------
Object* LightState::GetObjectByName (const char* acName)
{
    Object* pkFound = RenderState::GetObjectByName(acName);
    if ( pkFound )
        return pkFound;

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] )
        {
            pkFound = m_aspkLight[i]->GetObjectByName(acName);
            if ( pkFound )
                return pkFound;
        }
    }

    return NULL;
}
//----------------------------------------------------------------------------
void LightState::GetAllObjectsByName (const char* acName,
    std::vector<Object*>& rkObjects)
{
    RenderState::GetAllObjectsByName(acName,rkObjects);

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] )
            m_aspkLight[i]->GetAllObjectsByName(acName,rkObjects);
    }
}
//----------------------------------------------------------------------------
RenderState* LightState::Extract (int iLastIndex, RenderState* apkState[])
{
    // accumulate the lights in the stack into a single LightState object
    LightState* pkNewLState = new LightState;
    for (int iIndex = 0; iIndex <= iLastIndex; iIndex++)
    {
        LightState* pkLState = (LightState*)apkState[iIndex];
        for (int i = 0; i < MAX_LIGHTS; i++)
        {
            Light* pkLight = pkLState->m_aspkLight[i];
            if ( pkLight )
            {
                int iPos = pkNewLState->Attach(pkLight);
                assert( iPos < MAX_LIGHTS );
            }
        }
    }
    return pkNewLState;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* LightState::Factory (Stream& rkStream)
{
    LightState* pkObject = new LightState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void LightState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // link data
    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        Light* pkLight;
        StreamRead(rkStream,pkLight);
        pkLink->Add(pkLight);
    }
}
//----------------------------------------------------------------------------
void LightState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        Object* pkLinkID = pkLink->GetLinkID();
        m_aspkLight[i] = (Light*)rkStream.GetFromMap(pkLinkID);
    }
}
//----------------------------------------------------------------------------
bool LightState::Register (Stream& rkStream)
{
    if ( !RenderState::Register(rkStream) )
        return false;

    for (int i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] )
            m_aspkLight[i]->Register(rkStream);
    }

    return true;
}
//----------------------------------------------------------------------------
void LightState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // link data
    for (int i = 0; i < MAX_LIGHTS; i++)
        StreamWrite(rkStream,m_aspkLight[i]);
}
//----------------------------------------------------------------------------
StringTree* LightState::SaveStrings ()
{
    int iLQuantity = 0;
    int i;
    for (i = 0; i < MAX_LIGHTS; i++)
    {
        if ( m_aspkLight[i] )
            iLQuantity++;
    }

    StringTree* pkTree = new StringTree(1,0,iLQuantity+1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    int iSlot = 1;
    for (i = 0; i < MAX_LIGHTS; i++)
    {
        Light* pkLight = m_aspkLight[i];
        if ( pkLight )
            pkTree->SetChild(iSlot++,pkLight->SaveStrings());
    }

    return pkTree;
}
//----------------------------------------------------------------------------
int LightState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(LightState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int LightState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        MAX_LIGHTS*sizeof(m_aspkLight[0]);
}
//----------------------------------------------------------------------------
