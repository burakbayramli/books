// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlDirectionalLight.h"
#include "WmlMatrix3.h"
using namespace Wml;

WmlImplementRTTI(DirectionalLight,Light);
WmlImplementStream(DirectionalLight);

//----------------------------------------------------------------------------
DirectionalLight::DirectionalLight ()
    :
    m_kDirection(Vector3f::UNIT_Z)
{
}
//----------------------------------------------------------------------------
void DirectionalLight::ComputeDiffuse (const Matrix3f& rkWorldRotate,
    const Vector3f&, float, const Vector3f*, const Vector3f* akNormal,
    int iQuantity, const bool* abVisible, ColorRGB* akDiffuse)
{
    // transform light direction to model space of old mesh
    Vector3f kModelDir = m_kDirection*rkWorldRotate;

    // adjust diffuse color by light intensity
    ColorRGB kAdjDiffuse = m_fIntensity*m_kDiffuse;

    for (int i = 0; i < iQuantity; i++)
    {
        if ( abVisible[i] )
        {
            float fDot = kModelDir.Dot(akNormal[i]);
            if ( fDot < 0.0f )
                akDiffuse[i] -= fDot*kAdjDiffuse;
        }
    }
}
//----------------------------------------------------------------------------
void DirectionalLight::ComputeSpecular (const Matrix3f& rkWorldRotate,
    const Vector3f&, float, const Vector3f* akVertex,
    const Vector3f* akNormal, int iQuantity, const bool* abVisible,
    const Vector3f& rkCameraModelLocation, ColorRGB* akSpecular)
{
    // transform light direction to model space of old mesh
    Vector3f kModelDir = m_kDirection*rkWorldRotate;

    // adjust specular color by light intensity
    ColorRGB kAdjSpecular = m_fIntensity*m_kSpecular;

    for (int i = 0; i < iQuantity; i++)
    {
        if ( abVisible[i] )
        {
            float fDot = kModelDir.Dot(akNormal[i]);
            Vector3f kReflect = kModelDir - (2.0f*fDot)*akNormal[i];
            Vector3f kViewDir = rkCameraModelLocation - akVertex[i];
            fDot = kViewDir.Dot(kReflect);
            if ( fDot > 0.0f )
            {
                float fCoeff = fDot*fDot/kViewDir.Dot(kViewDir);
                akSpecular[i] += fCoeff*kAdjSpecular;
            }
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* DirectionalLight::Factory (Stream& rkStream)
{
    DirectionalLight* pkObject = new DirectionalLight;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void DirectionalLight::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Light::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kDirection);
}
//----------------------------------------------------------------------------
void DirectionalLight::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Light::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool DirectionalLight::Register (Stream& rkStream)
{
    return Light::Register(rkStream);
}
//----------------------------------------------------------------------------
void DirectionalLight::Save (Stream& rkStream)
{
    Light::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kDirection);
}
//----------------------------------------------------------------------------
StringTree* DirectionalLight::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("direction =",m_kDirection));

    // children
    pkTree->SetChild(0,Light::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int DirectionalLight::GetMemoryUsed () const
{
    int iBaseSize = sizeof(DirectionalLight) - sizeof(Light);
    int iTotalSize = iBaseSize + Light::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int DirectionalLight::GetDiskUsed () const
{
    return Light::GetDiskUsed() +
        sizeof(m_kDirection);
}
//----------------------------------------------------------------------------
