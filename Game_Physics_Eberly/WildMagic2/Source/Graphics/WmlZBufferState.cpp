// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlZBufferState.h"
using namespace Wml;

WmlImplementRTTI(ZBufferState,RenderState);
WmlImplementStream(ZBufferState);
WmlImplementDefaultState(ZBufferState);

//----------------------------------------------------------------------------
RenderState::Type ZBufferState::GetType () const
{
    return RS_ZBUFFER;
}
//----------------------------------------------------------------------------
ZBufferState::ZBufferState ()
{
    m_bEnabled = false;
    m_bWriteable = false;
    m_eCompare = CF_ALWAYS;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ZBufferState::Factory (Stream& rkStream)
{
    ZBufferState* pkObject = new ZBufferState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ZBufferState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bEnabled);
    StreamReadBool(rkStream,m_bWriteable);
    StreamReadEnum(rkStream,m_eCompare);
}
//----------------------------------------------------------------------------
void ZBufferState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ZBufferState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void ZBufferState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bEnabled);
    StreamWriteBool(rkStream,m_bWriteable);
    StreamWriteEnum(rkStream,m_eCompare);
}
//----------------------------------------------------------------------------
StringTree* ZBufferState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("enabled =",m_bEnabled));
    pkTree->SetString(2,MakeString("write =",m_bWriteable));

    switch ( m_eCompare )
    {
    case CF_NEVER:
        pkTree->SetString(3,MakeString("compare func = NEVER"));
        break;
    case CF_LESS:
        pkTree->SetString(3,MakeString("compare func = LESS"));
        break;
    case CF_EQUAL:
        pkTree->SetString(3,MakeString("compare func = EQUAL"));
        break;
    case CF_LEQUAL:
        pkTree->SetString(3,MakeString("compare func = LEQUAL"));
        break;
    case CF_GREATER:
        pkTree->SetString(3,MakeString("compare func = GREATER"));
        break;
    case CF_NOTEQUAL:
        pkTree->SetString(3,MakeString("compare func = NOTEQUAL"));
        break;
    case CF_GEQUAL:
        pkTree->SetString(3,MakeString("compare func = GEQUAL"));
        break;
    case CF_ALWAYS:
        pkTree->SetString(3,MakeString("compare func = ALWAYS"));
        break;
    default:  // CF_QUANTITY
        break;
    }

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int ZBufferState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ZBufferState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ZBufferState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bEnabled) +
        StreamBytesBool(m_bWriteable) +
        StreamBytesEnum(m_eCompare);
}
//----------------------------------------------------------------------------
