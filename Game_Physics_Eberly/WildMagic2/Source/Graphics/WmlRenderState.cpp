// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRenderState.h"
using namespace Wml;

WmlImplementRTTI(RenderState,Object);
WmlImplementStream(RenderState);

RenderState* RenderState::ms_apkDefault[RS_MAX_STATE];

//----------------------------------------------------------------------------
RenderState::RenderState ()
{
}
//----------------------------------------------------------------------------
RenderState::~RenderState ()
{
}
//----------------------------------------------------------------------------
RenderState* RenderState::Extract (int iLastIndex, RenderState* apkState[])
{
    // The default behavior is to return the top of the stack, the last item
    // pushed during the recursive traveral.
    return apkState[iLastIndex];
}
//----------------------------------------------------------------------------
RenderState::Stack::Stack ()
{
    // The render state initially on top of the stack is the default render
    // state.
    for (int i = 0; i < RS_MAX_STATE; i++)
    {
        m_aapkState[i][0] = RenderState::GetDefaultStates()[i];
        m_aiTop[i] = 0;
    }
}
//----------------------------------------------------------------------------
void RenderState::Stack::Push (RenderState* pkState)
{
    assert( pkState );
    Type eType = pkState->GetType();

    int iTop = ++m_aiTop[eType];
    if ( iTop < RS_MAX_STACK )
    {
        m_aapkState[eType][iTop] = pkState;
    }
    else
    {
        // stack full, should not happen
        assert( false );
        m_aiTop[eType]--;
    }
}
//----------------------------------------------------------------------------
void RenderState::Stack::Pop (RenderState* pkState)
{
    assert( pkState );
    Type eType = pkState->GetType();

    int iTop = m_aiTop[eType];
    if ( iTop > 0 )
    {
        m_aapkState[eType][iTop] = NULL;
        m_aiTop[eType]--;
    }
    else
    {
        // stack empty, should not happen
        assert( false );
        m_aiTop[eType] = 0;
    }
}
//----------------------------------------------------------------------------
void RenderState::Stack::CopyTo (RenderStatePtr aspkState[])
{
    for (int i = 0; i < RS_MAX_STATE; i++)
    {
        RenderState* pkDefault = GetDefaultStates()[i];
        int iLastIndex = m_aiTop[i];
        if ( iLastIndex > 0 )
            aspkState[i] = pkDefault->Extract(iLastIndex,m_aapkState[i]);
        else
            aspkState[i] = pkDefault;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* RenderState::Factory (Stream&)
{
    // RenderState is abstract, Factory never called
    return 0;
}
//----------------------------------------------------------------------------
void RenderState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void RenderState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool RenderState::Register (Stream& rkStream)
{
    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void RenderState::Save (Stream& rkStream)
{
    Object::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* RenderState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int RenderState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(RenderState) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int RenderState::GetDiskUsed () const
{
    return Object::GetDiskUsed();
}
//----------------------------------------------------------------------------
