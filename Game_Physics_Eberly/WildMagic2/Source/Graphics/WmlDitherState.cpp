// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDitherState.h"
using namespace Wml;

WmlImplementRTTI(DitherState,RenderState);
WmlImplementStream(DitherState);
WmlImplementDefaultState(DitherState);

//----------------------------------------------------------------------------
RenderState::Type DitherState::GetType () const
{
    return RS_DITHER;
}
//----------------------------------------------------------------------------
DitherState::DitherState ()
{
    m_bEnabled = false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* DitherState::Factory (Stream& rkStream)
{
    DitherState* pkObject = new DitherState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void DitherState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bEnabled);
}
//----------------------------------------------------------------------------
void DitherState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool DitherState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void DitherState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bEnabled);
}
//----------------------------------------------------------------------------
StringTree* DitherState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("enabled =",m_bEnabled));

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int DitherState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(DitherState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int DitherState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bEnabled);
}
//----------------------------------------------------------------------------
