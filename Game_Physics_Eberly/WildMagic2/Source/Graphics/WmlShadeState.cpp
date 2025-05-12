// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlShadeState.h"
using namespace Wml;

WmlImplementRTTI(ShadeState,RenderState);
WmlImplementStream(ShadeState);
WmlImplementDefaultState(ShadeState);

//----------------------------------------------------------------------------
RenderState::Type ShadeState::GetType () const
{
    return RS_SHADE;
}
//----------------------------------------------------------------------------
ShadeState::ShadeState ()
{
    m_eShade = SM_SMOOTH;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ShadeState::Factory (Stream& rkStream)
{
    ShadeState* pkObject = new ShadeState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ShadeState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamReadEnum(rkStream,m_eShade);
}
//----------------------------------------------------------------------------
void ShadeState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ShadeState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void ShadeState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWriteEnum(rkStream,m_eShade);
}
//----------------------------------------------------------------------------
StringTree* ShadeState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    switch ( m_eShade )
    {
    case SM_FLAT:
        pkTree->SetString(1,MakeString("mode = FLAT"));
        break;
    case SM_SMOOTH:
        pkTree->SetString(1,MakeString("mode = SMOOTH"));
        break;
    default:  // SM_QUANTITY
        break;
    }

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int ShadeState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ShadeState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ShadeState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        StreamBytesEnum(m_eShade);
}
//----------------------------------------------------------------------------
