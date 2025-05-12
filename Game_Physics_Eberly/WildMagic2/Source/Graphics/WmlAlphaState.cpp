// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlAlphaState.h"
using namespace Wml;

WmlImplementRTTI(AlphaState,RenderState);
WmlImplementStream(AlphaState);
WmlImplementDefaultState(AlphaState);

//----------------------------------------------------------------------------
RenderState::Type AlphaState::GetType () const
{
    return RS_ALPHA;
}
//----------------------------------------------------------------------------
AlphaState::AlphaState ()
{
    m_bBlendEnabled = false;
    m_eSrcBlend = SBF_SRC_ALPHA;
    m_eDstBlend = DBF_ONE_MINUS_SRC_ALPHA;
    m_bTestEnabled = false;
    m_eTest = TF_ALWAYS;
    m_fReference = 0.0f;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* AlphaState::Factory (Stream& rkStream)
{
    AlphaState* pkObject = new AlphaState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void AlphaState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bBlendEnabled);
    StreamReadEnum(rkStream,m_eSrcBlend);
    StreamReadEnum(rkStream,m_eDstBlend);
    StreamReadBool(rkStream,m_bTestEnabled);
    StreamReadEnum(rkStream,m_eTest);
    StreamRead(rkStream,m_fReference);
}
//----------------------------------------------------------------------------
void AlphaState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool AlphaState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void AlphaState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bBlendEnabled);
    StreamWriteEnum(rkStream,m_eSrcBlend);
    StreamWriteEnum(rkStream,m_eDstBlend);
    StreamWriteBool(rkStream,m_bTestEnabled);
    StreamWriteEnum(rkStream,m_eTest);
    StreamWrite(rkStream,m_fReference);
}
//----------------------------------------------------------------------------
StringTree* AlphaState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(6,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("blend =",m_bBlendEnabled));

    switch ( m_eSrcBlend )
    {
    case SBF_ZERO:
        pkTree->SetString(2,MakeString("src blend = ZERO"));
        break;
    case SBF_ONE:
        pkTree->SetString(2,MakeString("src blend = ONE"));
        break;
    case SBF_DST_COLOR:
        pkTree->SetString(2,MakeString("src blend = DST_COLOR"));
        break;
    case SBF_ONE_MINUS_DST_COLOR:
        pkTree->SetString(2,MakeString("src blend = ONE_MINUS_DST_COLOR"));
        break;
    case SBF_SRC_ALPHA:
        pkTree->SetString(2,MakeString("src blend = SRC_ALPHA"));
        break;
    case SBF_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(2,MakeString("src blend = ONE_MINUS_SRC_ALPHA"));
        break;
    case SBF_DST_ALPHA:
        pkTree->SetString(2,MakeString("src blend = DST_ALPHA"));
        break;
    case SBF_ONE_MINUS_DST_ALPHA:
        pkTree->SetString(2,MakeString("src blend = ONE_MINUS_DST_ALPHA"));
        break;
    case SBF_SRC_ALPHA_SATURATE:
        pkTree->SetString(2,MakeString("src blend = SRC_ALPHA_SATURATE"));
        break;
    default:  // SBF_QUANTITY
        break;
    }

    switch ( m_eDstBlend )
    {
    case DBF_ZERO:
        pkTree->SetString(3,MakeString("dst blend = ZERO"));
        break;
    case DBF_ONE:
        pkTree->SetString(3,MakeString("dst blend = ONE"));
        break;
    case DBF_SRC_COLOR:
        pkTree->SetString(3,MakeString("dst blend = DST_COLOR"));
        break;
    case DBF_ONE_MINUS_SRC_COLOR:
        pkTree->SetString(3,MakeString("dst blend = ONE_MINUS_DST_COLOR"));
        break;
    case DBF_SRC_ALPHA:
        pkTree->SetString(3,MakeString("dst blend = SRC_ALPHA"));
        break;
    case DBF_ONE_MINUS_SRC_ALPHA:
        pkTree->SetString(3,MakeString("dst blend = ONE_MINUS_SRC_ALPHA"));
        break;
    case DBF_DST_ALPHA:
        pkTree->SetString(3,MakeString("dst blend = DST_ALPHA"));
        break;
    case DBF_ONE_MINUS_DST_ALPHA:
        pkTree->SetString(3,MakeString("dst blend = ONE_MINUS_DST_ALPHA"));
        break;
    default:  // DBF_QUANTITY
        break;
    }

    pkTree->SetString(1,MakeString("test =",m_bTestEnabled));

    switch ( m_eTest )
    {
    case TF_NEVER:
        pkTree->SetString(4,MakeString("test func = NEVER"));
        break;
    case TF_LESS:
        pkTree->SetString(4,MakeString("test func = LESS"));
        break;
    case TF_EQUAL:
        pkTree->SetString(4,MakeString("test func = EQUAL"));
        break;
    case TF_LEQUAL:
        pkTree->SetString(4,MakeString("test func = LEQUAL"));
        break;
    case TF_GREATER:
        pkTree->SetString(4,MakeString("test func = GREATER"));
        break;
    case TF_NOTEQUAL:
        pkTree->SetString(4,MakeString("test func = NOTEQUAL"));
        break;
    case TF_GEQUAL:
        pkTree->SetString(4,MakeString("test func = GEQUAL"));
        break;
    case TF_ALWAYS:
        pkTree->SetString(4,MakeString("test func = ALWAYS"));
        break;
    default:  // TF_QUANTITY
        break;
    }

    pkTree->SetString(5,MakeString("test ref =",m_fReference));

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int AlphaState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(AlphaState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int AlphaState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bBlendEnabled) +
        StreamBytesEnum(m_eSrcBlend) +
        StreamBytesEnum(m_eDstBlend) +
        StreamBytesBool(m_bTestEnabled) +
        StreamBytesEnum(m_eTest) +
        sizeof(m_fReference);
}
//----------------------------------------------------------------------------
