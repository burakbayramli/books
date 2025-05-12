// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlAmbientLight.h"
using namespace Wml;

WmlImplementRTTI(AmbientLight,Light);
WmlImplementStream(AmbientLight);

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* AmbientLight::Factory (Stream& rkStream)
{
    AmbientLight* pkObject = new AmbientLight;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void AmbientLight::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Light::Load(rkStream,pkLink);
}
//----------------------------------------------------------------------------
void AmbientLight::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Light::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool AmbientLight::Register (Stream& rkStream)
{
    return Light::Register(rkStream);
}
//----------------------------------------------------------------------------
void AmbientLight::Save (Stream& rkStream)
{
    Light::Save(rkStream);
}
//----------------------------------------------------------------------------
StringTree* AmbientLight::SaveStrings ()
{
    StringTree* pkTree = new StringTree(1,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));

    // children
    pkTree->SetChild(0,Light::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int AmbientLight::GetMemoryUsed () const
{
    int iBaseSize = sizeof(AmbientLight) - sizeof(Light);
    int iTotalSize = iBaseSize + Light::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int AmbientLight::GetDiskUsed () const
{
    return Light::GetDiskUsed();
}
//----------------------------------------------------------------------------
