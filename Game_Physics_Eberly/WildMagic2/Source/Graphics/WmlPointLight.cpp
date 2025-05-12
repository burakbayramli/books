// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMatrix3.h"
#include "WmlPointLight.h"
using namespace Wml;

WmlImplementRTTI(PointLight,Light);
WmlImplementStream(PointLight);

//----------------------------------------------------------------------------
PointLight::PointLight ()
    :
    m_kLocation(Vector3f::ZERO)
{
}
//----------------------------------------------------------------------------
void PointLight::ComputeDiffuse (const Matrix3f& rkWorldRotate,
    const Vector3f& rkWorldTranslate, float fWorldScale,
    const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
    const bool* abVisible, ColorRGB* akDiffuse)
{
    // transform light position to model space of old mesh
    float fInvWScale = 1.0f/fWorldScale;
    Vector3f kDiff = m_kLocation - rkWorldTranslate;
    Vector3f kModelPos = (kDiff*rkWorldRotate)*fInvWScale;

    // adjust diffuse color by light intensity
    ColorRGB kAdjDiffuse = m_fIntensity*m_kDiffuse;

    for (int i = 0; i < iQuantity; i++)
    {
        if ( abVisible[i] )
        {
            kDiff = kModelPos - akVertex[i];
            float fDot = kDiff.Dot(akNormal[i]);
            if ( fDot > 0.0f )
            {
                fDot /= kDiff.Length();
                akDiffuse[i] += fDot*kAdjDiffuse;
            }
        }
    }
}
//----------------------------------------------------------------------------
void PointLight::ComputeSpecular (const Matrix3f& rkWorldRotate,
    const Vector3f& rkWorldTranslate, float fWorldScale,
    const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
    const bool* abVisible, const Vector3f& rkCameraModelLocation,
    ColorRGB* akSpecular)
{
    // transform light position to model space of old mesh
    float fInvWScale = 1.0f/fWorldScale;
    Vector3f kDiff = m_kLocation - rkWorldTranslate;
    Vector3f kModelPos = (kDiff*rkWorldRotate)*fInvWScale;

    // adjust specular color by light intensity
    ColorRGB kAdjSpecular = m_fIntensity*m_kSpecular;

    for (int i = 0; i < iQuantity; i++)
    {
        if ( abVisible[i] )
        {
            kDiff = kModelPos - akVertex[i];
            float fDot = kDiff.Dot(akNormal[i]);
            Vector3f kReflect = (2.0f*fDot)*akNormal[i] - kDiff;
            Vector3f kViewDir = rkCameraModelLocation - akVertex[i];
            fDot = kViewDir.Dot(kReflect);
            if ( fDot > 0.0f )
            {
                float fVSL = kViewDir.Dot(kViewDir);
                float fRSL = kReflect.Dot(kReflect);
                float fDenom = Mathf::Sqrt(fVSL*fRSL);
                akSpecular[i] += (fDot/fDenom)*kAdjSpecular;
            }
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* PointLight::Factory (Stream& rkStream)
{
    PointLight* pkObject = new PointLight;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void PointLight::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Light::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kLocation);
}
//----------------------------------------------------------------------------
void PointLight::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Light::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool PointLight::Register (Stream& rkStream)
{
    return Light::Register(rkStream);
}
//----------------------------------------------------------------------------
void PointLight::Save (Stream& rkStream)
{
    Light::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kLocation);
}
//----------------------------------------------------------------------------
StringTree* PointLight::SaveStrings ()
{
    StringTree* pkTree = new StringTree(2,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("location =",m_kLocation));

    // children
    pkTree->SetChild(0,Light::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int PointLight::GetMemoryUsed () const
{
    int iBaseSize = sizeof(PointLight) - sizeof(Light);
    int iTotalSize = iBaseSize + Light::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int PointLight::GetDiskUsed () const
{
    return Light::GetDiskUsed() +
        sizeof(m_kLocation);
}
//----------------------------------------------------------------------------
