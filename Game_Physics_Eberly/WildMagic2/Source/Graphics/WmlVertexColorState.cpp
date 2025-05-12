// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlVertexColorState.h"
using namespace Wml;

WmlImplementRTTI(VertexColorState,RenderState);
WmlImplementStream(VertexColorState);
WmlImplementDefaultState(VertexColorState);

//----------------------------------------------------------------------------
RenderState::Type VertexColorState::GetType () const
{
    return RS_VERTEXCOLOR;
}
//----------------------------------------------------------------------------
VertexColorState::VertexColorState ()
{
    m_eSource = SM_IGNORE;
    m_eLighting = LM_DIFFUSE;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* VertexColorState::Factory (Stream& rkStream)
{
    VertexColorState* pkObject = new VertexColorState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void VertexColorState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadEnum(rkStream,m_eSource);
    StreamReadEnum(rkStream,m_eLighting);
}
//----------------------------------------------------------------------------
void VertexColorState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool VertexColorState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void VertexColorState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteEnum(rkStream,m_eSource);
    StreamWriteEnum(rkStream,m_eLighting);
}
//----------------------------------------------------------------------------
StringTree* VertexColorState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(3,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    switch ( m_eSource )
    {
    case SM_IGNORE:
        pkTree->SetString(1,MakeString("source = IGNORE"));
        break;
    case SM_EMISSIVE:
        pkTree->SetString(1,MakeString("source = EMISSIVE"));
        break;
    case SM_DIFFUSE:
        pkTree->SetString(1,MakeString("source = DIFFUSE"));
        break;
    default:  // SM_QUANTITY
        break;
    };

    switch ( m_eLighting )
    {
    case LM_EMISSIVE:
        pkTree->SetString(2,MakeString("lighting = EMISSIVE"));
        break;
    case LM_DIFFUSE:
        pkTree->SetString(2,MakeString("lighting = DIFFUSE"));
        break;
    default:  // LM_QUANTITY
        break;
    }

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int VertexColorState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(VertexColorState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int VertexColorState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesEnum(m_eSource) +
        StreamBytesEnum(m_eLighting);
}
//----------------------------------------------------------------------------
