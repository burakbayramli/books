// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlWireframeState.h"
using namespace Wml;

WmlImplementRTTI(WireframeState,RenderState);
WmlImplementStream(WireframeState);
WmlImplementDefaultState(WireframeState);

//----------------------------------------------------------------------------
RenderState::Type WireframeState::GetType () const
{
    return RS_WIREFRAME;
}
//----------------------------------------------------------------------------
WireframeState::WireframeState ()
{
    m_bEnabled = false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* WireframeState::Factory (Stream& rkStream)
{
    WireframeState* pkObject = new WireframeState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void WireframeState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bEnabled);
}
//----------------------------------------------------------------------------
void WireframeState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool WireframeState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void WireframeState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bEnabled);
}
//----------------------------------------------------------------------------
StringTree* WireframeState::SaveStrings ()
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
int WireframeState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(WireframeState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int WireframeState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bEnabled);
}
//----------------------------------------------------------------------------
