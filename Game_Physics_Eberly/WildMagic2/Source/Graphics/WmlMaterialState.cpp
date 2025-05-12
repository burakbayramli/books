// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMaterialState.h"
using namespace Wml;

WmlImplementRTTI(MaterialState,RenderState);
WmlImplementStream(MaterialState);
WmlImplementDefaultState(MaterialState);

//----------------------------------------------------------------------------
RenderState::Type MaterialState::GetType () const
{
    return RS_MATERIAL;
}
//----------------------------------------------------------------------------
MaterialState::MaterialState ()
    :
    m_kEmissive(ColorRGB::BLACK),
    m_kAmbient(0.2f,0.2f,0.2f),
    m_kDiffuse(0.8f,0.8f,0.8f),
    m_kSpecular(ColorRGB::BLACK)
{
    m_fShininess = 1.0f;
    m_fAlpha = 1.0f;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* MaterialState::Factory (Stream& rkStream)
{
    MaterialState* pkObject = new MaterialState;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void MaterialState::Load (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kEmissive);
    StreamRead(rkStream,m_kAmbient);
    StreamRead(rkStream,m_kDiffuse);
    StreamRead(rkStream,m_kSpecular);
    StreamRead(rkStream,m_fShininess);
    StreamRead(rkStream,m_fAlpha);
}
//----------------------------------------------------------------------------
void MaterialState::Link (Stream& rkStream, Stream::Link* pkLink)
{
    RenderState::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool MaterialState::Register (Stream& rkStream)
{
    return RenderState::Register(rkStream);
}
//----------------------------------------------------------------------------
void MaterialState::Save (Stream& rkStream)
{
    RenderState::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kEmissive);
    StreamWrite(rkStream,m_kAmbient);
    StreamWrite(rkStream,m_kDiffuse);
    StreamWrite(rkStream,m_kSpecular);
    StreamWrite(rkStream,m_fShininess);
    StreamWrite(rkStream,m_fAlpha);
}
//----------------------------------------------------------------------------
StringTree* MaterialState::SaveStrings ()
{
    StringTree* pkTree = new StringTree(7,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("emissive =",m_kEmissive));
    pkTree->SetString(2,MakeString("ambient =",m_kAmbient));
    pkTree->SetString(3,MakeString("diffuse =",m_kDiffuse));
    pkTree->SetString(4,MakeString("specular =",m_kSpecular));
    pkTree->SetString(5,MakeString("shininess =",m_fShininess));
    pkTree->SetString(6,MakeString("alpha =",m_fAlpha));

    // children
    pkTree->SetChild(0,RenderState::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int MaterialState::GetMemoryUsed () const
{
    int iBaseSize = sizeof(MaterialState) - sizeof(RenderState);
    int iTotalSize = iBaseSize + RenderState::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int MaterialState::GetDiskUsed () const
{
    return RenderState::GetDiskUsed() +
        sizeof(m_kEmissive) +
        sizeof(m_kAmbient) +
        sizeof(m_kDiffuse) +
        sizeof(m_kSpecular) +
        sizeof(m_fShininess) +
        sizeof(m_fAlpha);
}
//----------------------------------------------------------------------------
