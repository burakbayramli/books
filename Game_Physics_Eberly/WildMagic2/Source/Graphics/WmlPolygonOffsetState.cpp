// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPolygonOffsetState.h"
using namespace Wml;

WmlImplementRTTI(PolygonOffsetState,RenderState);
WmlImplementStream(PolygonOffsetState);
WmlImplementDefaultState(PolygonOffsetState);

//----------------------------------------------------------------------------
RenderState::Type PolygonOffsetState::GetType () const
{
    return RS_POLYGONOFFSET;
}
//----------------------------------------------------------------------------
PolygonOffsetState::PolygonOffsetState ()
{
    m_bFillEnabled = false;
    m_bLineEnabled = false;
    m_bPointEnabled = false;
    m_fScale = -1.0f;
    m_fBias = -2.0f;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* PolygonOffsetState::Factory (Stream& rkStream)
{
    PolygonOffsetState* pkObject = new PolygonOffsetState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void PolygonOffsetState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadBool(rkStream,m_bFillEnabled);
    StreamReadBool(rkStream,m_bLineEnabled);
    StreamReadBool(rkStream,m_bPointEnabled);
    StreamRead(rkStream,m_fScale);
    StreamRead(rkStream,m_fBias);
}
//----------------------------------------------------------------------------
void PolygonOffsetState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool PolygonOffsetState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void PolygonOffsetState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteBool(rkStream,m_bFillEnabled);
    StreamWriteBool(rkStream,m_bLineEnabled);
    StreamWriteBool(rkStream,m_bPointEnabled);
    StreamWrite(rkStream,m_fScale);
    StreamWrite(rkStream,m_fBias);
}
//----------------------------------------------------------------------------
StringTree* PolygonOffsetState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(6,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("fill enabled =",m_bFillEnabled));
    pkTree->SetString(2,MakeString("line enabled =",m_bLineEnabled));
    pkTree->SetString(3,MakeString("point enabled =",m_bPointEnabled));
    pkTree->SetString(4,MakeString("scale =",m_fScale));
    pkTree->SetString(5,MakeString("bias =",m_fBias));

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int PolygonOffsetState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(PolygonOffsetState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int PolygonOffsetState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesBool(m_bFillEnabled) +
        StreamBytesBool(m_bLineEnabled) +
        StreamBytesBool(m_bPointEnabled) +
        sizeof(m_fScale) +
        sizeof(m_fBias);
}
//----------------------------------------------------------------------------
