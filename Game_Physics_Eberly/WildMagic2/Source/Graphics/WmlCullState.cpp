// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCullState.h"
using namespace Wml;

WmlImplementRTTI(CullState,RenderState);
WmlImplementStream(CullState);
WmlImplementDefaultState(CullState);

//----------------------------------------------------------------------------
RenderState::Type CullState::GetType () const
{
    return RS_CULL;
}
//----------------------------------------------------------------------------
CullState::CullState ()
{
    m_bEnabled = true;
    m_eFrontFace = FT_CCW;
    m_eCullFace = CT_BACK;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* CullState::Factory (Stream& rkStream)
{
    CullState* pkObject = new CullState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void CullState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bEnabled);
    StreamReadEnum(rkStream,m_eFrontFace);
    StreamReadEnum(rkStream,m_eCullFace);
}
//----------------------------------------------------------------------------
void CullState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool CullState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void CullState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bEnabled);
    StreamWriteEnum(rkStream,m_eFrontFace);
    StreamWriteEnum(rkStream,m_eCullFace);
}
//----------------------------------------------------------------------------
StringTree* CullState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("enabled =",m_bEnabled));

    switch ( m_eFrontFace )
    {
    case FT_CCW:
        pkTree->SetString(2,MakeString("front face = CCW"));
        break;
    case FT_CW:
        pkTree->SetString(2,MakeString("front face = CW"));
        break;
    default:  // FT_QUANTITY
        break;
    }

    switch ( m_eCullFace )
    {
    case CT_FRONT:
        pkTree->SetString(3,MakeString("cull face = FRONT"));
        break;
    case CT_BACK:
        pkTree->SetString(3,MakeString("cull face = BACK"));
        break;
    default:  // CT_QUANTITY
        break;
    }

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int CullState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(CullState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int CullState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bEnabled) +
        StreamBytesEnum(m_eFrontFace) +
        StreamBytesEnum(m_eCullFace);
}
//----------------------------------------------------------------------------
