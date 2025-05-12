// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlFogState.h"
using namespace Wml;

WmlImplementRTTI(FogState,RenderState);
WmlImplementStream(FogState);
WmlImplementDefaultState(FogState);

//----------------------------------------------------------------------------
RenderState::Type FogState::GetType () const
{
    return RS_FOG;
}
//----------------------------------------------------------------------------
FogState::FogState ()
    :
    m_kColor(ColorRGB::BLACK)
{
    m_bEnabled = false;
    m_fStart = 0.0f;
    m_fEnd = 1.0f;
    m_fDensity = 1.0f;
    m_eDFunction = DF_LINEAR;
    m_eAFunction = AF_PER_VERTEX;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* FogState::Factory (Stream& rkStream)
{
    FogState* pkObject = new FogState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void FogState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bEnabled);
    StreamRead(rkStream,m_fStart);
    StreamRead(rkStream,m_fEnd);
    StreamRead(rkStream,m_fDensity);
    StreamRead(rkStream,m_kColor);
    StreamReadEnum(rkStream,m_eDFunction);
    StreamReadEnum(rkStream,m_eAFunction);
}
//----------------------------------------------------------------------------
void FogState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool FogState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void FogState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bEnabled);
    StreamWrite(rkStream,m_fStart);
    StreamWrite(rkStream,m_fEnd);
    StreamWrite(rkStream,m_fDensity);
    StreamWrite(rkStream,m_kColor);
    StreamWriteEnum(rkStream,m_eDFunction);
    StreamWriteEnum(rkStream,m_eAFunction);
}
//----------------------------------------------------------------------------
StringTree* FogState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(8,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("enabled =",m_bEnabled));
    pkTree->SetString(2,MakeString("start =",m_fStart));
    pkTree->SetString(3,MakeString("end =",m_fEnd));
    pkTree->SetString(4,MakeString("density =",m_fDensity));
    pkTree->SetString(5,MakeString("color =",m_kColor));

    switch ( m_eDFunction )
    {
    case DF_LINEAR:
        pkTree->SetString(6,MakeString("density func = LINEAR"));
        break;
    case DF_EXP:
        pkTree->SetString(6,MakeString("density func = EXP"));
        break;
    case DF_EXPSQR:
        pkTree->SetString(6,MakeString("density func = EXPSQR"));
        break;
    default:  // DF_QUANTITY
        break;
    };

    switch ( m_eAFunction )
    {
    case AF_PER_VERTEX:
        pkTree->SetString(7,MakeString("apply func = VERTEX"));
        break;
    case AF_PER_PIXEL:
        pkTree->SetString(7,MakeString("apply func = PIXEL"));
        break;
    default:  // AF_QUANTITY
        break;
    };

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int FogState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(FogState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int FogState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bEnabled) +
        sizeof(m_fStart) +
        sizeof(m_fEnd) +
        sizeof(m_fDensity) +
        sizeof(m_kColor) +
        StreamBytesEnum(m_eDFunction) +
        StreamBytesEnum(m_eAFunction);
}
//----------------------------------------------------------------------------
