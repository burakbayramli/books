// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlLight.h"
using namespace Wml;

WmlImplementRTTI(Light,Object);
WmlImplementStream(Light);

//----------------------------------------------------------------------------
Light::Light ()
    :
    m_kAmbient(ColorRGB::BLACK),
    m_kDiffuse(ColorRGB::BLACK),
    m_kSpecular(ColorRGB::BLACK)
{
    m_fConstant = 1.0f;
    m_fLinear = 0.0f;
    m_fQuadratic = 0.0f;
    m_fIntensity = 1.0f;
    m_bAttenuate = false;
    m_bOn = true;
    m_bSpecularAfterTexture = false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Light::Factory (Stream&)
{
    // Light is abstract, Factory never called
    return NULL;
}
//----------------------------------------------------------------------------
void Light::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kAmbient);
    StreamRead(rkStream,m_kDiffuse);
    StreamRead(rkStream,m_kSpecular);
    StreamRead(rkStream,m_fConstant);
    StreamRead(rkStream,m_fLinear);
    StreamRead(rkStream,m_fQuadratic);
    StreamRead(rkStream,m_fIntensity);
    StreamReadBool(rkStream,m_bAttenuate);
    StreamReadBool(rkStream,m_bOn);

    if ( rkStream.GetVersion() >= Version(1,3) )
        StreamReadBool(rkStream,m_bSpecularAfterTexture);
}
//----------------------------------------------------------------------------
void Light::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Light::Register (Stream& rkStream)
{
    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void Light::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kAmbient);
    StreamWrite(rkStream,m_kDiffuse);
    StreamWrite(rkStream,m_kSpecular);
    StreamWrite(rkStream,m_fConstant);
    StreamWrite(rkStream,m_fLinear);
    StreamWrite(rkStream,m_fQuadratic);
    StreamWrite(rkStream,m_fIntensity);
    StreamWriteBool(rkStream,m_bAttenuate);
    StreamWriteBool(rkStream,m_bOn);
    StreamWriteBool(rkStream,m_bSpecularAfterTexture);
}
//----------------------------------------------------------------------------
StringTree* Light::SaveStrings ()
{
    StringTree* pkTree = new StringTree(9,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("ambient =",m_kAmbient));
    pkTree->SetString(2,MakeString("diffuse =",m_kDiffuse));
    pkTree->SetString(3,MakeString("specular =",m_kSpecular));

    char acDummy[128];
    sprintf(acDummy,"constant: %f, linear: %f, quadratic: %f",
        m_fConstant,m_fLinear,m_fQuadratic);
    pkTree->SetString(4,MakeString("attenuation =",acDummy));

    pkTree->SetString(5,MakeString("intensity =",m_fIntensity));
    pkTree->SetString(6,MakeString("attenuate enabled =",m_bAttenuate));
    pkTree->SetString(7,MakeString("light on =",m_bOn));
    pkTree->SetString(8,MakeString("specular after texture =",
        m_bSpecularAfterTexture));

    // children
    pkTree->SetChild(0,Object::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int Light::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Light) - sizeof(Object);
    int iTotalSize = iBaseSize + Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Light::GetDiskUsed () const
{
    return Object::GetDiskUsed() +
        sizeof(m_kAmbient) +
        sizeof(m_kDiffuse) +
        sizeof(m_kSpecular) +
        sizeof(m_fConstant) +
        sizeof(m_fLinear) +
        sizeof(m_fQuadratic) +
        sizeof(m_fIntensity) +
        StreamBytesBool(m_bAttenuate) +
        StreamBytesBool(m_bOn) +
        StreamBytesBool(m_bSpecularAfterTexture);
}
//----------------------------------------------------------------------------
