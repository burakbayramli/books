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
#include "WmlSpotLight.h"
using namespace Wml;

WmlImplementRTTI(SpotLight,PointLight);
WmlImplementStream(SpotLight);

//----------------------------------------------------------------------------
SpotLight::SpotLight ()
    :
    m_kDirection(-Vector3f::UNIT_Z)
{
    SetAngle(Mathf::PI);
    m_fExponent = 0.0f;
}
//----------------------------------------------------------------------------
void SpotLight::SetAngle (float fAngle)
{
    m_fAngle = fAngle;
    m_fCosSqr = Mathf::Cos(m_fAngle);
    m_fCosSqr *= m_fCosSqr;
    m_fSinSqr = Mathf::Sin(m_fAngle);
    m_fSinSqr *= m_fSinSqr;
}
//----------------------------------------------------------------------------
void SpotLight::ComputeDiffuse (const Matrix3f& rkWorldRotate,
    const Vector3f& rkWorldTranslate, float fWorldScale,
    const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
    const bool* abVisible, ColorRGB* akDiffuse)
{
    // transform light position to model space of old mesh
    float fInvWScale = 1.0f/fWorldScale;
    Vector3f kDiff = m_kLocation - rkWorldTranslate;
    Vector3f kModelPos = (kDiff*rkWorldRotate)*fInvWScale;
    Vector3f kModelDir = m_kDirection*rkWorldRotate;

    // adjust diffuse color by light intensity
    ColorRGB kAdjDiffuse = m_fIntensity*m_kDiffuse;

    for (int i = 0; i < iQuantity; i++)
    {
        if ( abVisible[i] )
        {
            kDiff = kModelPos - akVertex[i];
            float fDiffDotDir = kDiff.Dot(kModelDir);
            if ( fDiffDotDir >= 0.0f )
                continue;

            float fLenSqr = kDiff.Dot(kDiff);
            float fDiffDotDirSqr = fDiffDotDir*fDiffDotDir;
            float fAngleAttenuate = fDiffDotDirSqr - fLenSqr*m_fCosSqr;
            if ( fAngleAttenuate <= 0.0f )
                continue;

            float fDot = kDiff.Dot(akNormal[i]);
            if ( fDot > 0.0f )
            {
                float fNumer = fDot*fAngleAttenuate;
                float fDenom = fDiffDotDirSqr*m_fSinSqr*Mathf::Sqrt(fLenSqr);
                akDiffuse[i] += (fNumer/fDenom)*kAdjDiffuse;
            }
        }
    }
}
//----------------------------------------------------------------------------
void SpotLight::ComputeSpecular (const Matrix3f& rkWorldRotate,
    const Vector3f& rkWorldTranslate, float fWorldScale,
    const Vector3f* akVertex, const Vector3f* akNormal, int iQuantity,
    const bool* abVisible, const Vector3f& rkCameraModelLocation,
    ColorRGB* akSpecular)
{
    // transform light position to model space of old mesh
    float fInvWScale = 1.0f/fWorldScale;
    Vector3f kDiff = m_kLocation - rkWorldTranslate;
    Vector3f kModelPos = (kDiff*rkWorldRotate)*fInvWScale;
    Vector3f kModelDir = m_kDirection*rkWorldRotate;

    // adjust diffuse color by light intensity
    ColorRGB kAdjSpecular = m_fIntensity*m_kSpecular;

    for (int i = 0; i < iQuantity; i++)
    {
        if ( abVisible[i] )
        {
            kDiff = kModelPos - akVertex[i];
            float fDiffDotDir = kDiff.Dot(kModelDir);
            if ( fDiffDotDir >= 0.0f )
                continue;

            float fLenSqr = kDiff.Dot(kDiff);
            float fDiffDotDirSqr = fDiffDotDir*fDiffDotDir;
            float fAngleAttenuate = fDiffDotDirSqr - fLenSqr*m_fCosSqr;
            if ( fAngleAttenuate <= 0.0f )
                continue;

            float fDot = kDiff.Dot(akNormal[i]);
            Vector3f kReflect = (2.0f*fDot)*akNormal[i] - kDiff;
            Vector3f kViewDir = rkCameraModelLocation - akVertex[i];
            fDot = kViewDir.Dot(kReflect);
            if ( fDot > 0.0f )
            {
                float fNumer = fDot*fDot*fAngleAttenuate;
                float fDenom = fDiffDotDirSqr*m_fSinSqr*Mathf::Sqrt(fLenSqr)*
                    kViewDir.Dot(kViewDir);
                akSpecular[i] += (fNumer/fDenom)*kAdjSpecular;
            }
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* SpotLight::Factory (Stream& rkStream)
{
    SpotLight* pkObject = new SpotLight;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void SpotLight::Load (Stream& rkStream, Stream::Link* pkLink)
{
    PointLight::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_kDirection);
    StreamRead(rkStream,m_fAngle);
    StreamRead(rkStream,m_fExponent);
}
//----------------------------------------------------------------------------
void SpotLight::Link (Stream& rkStream, Stream::Link* pkLink)
{
    PointLight::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool SpotLight::Register (Stream& rkStream)
{
    return PointLight::Register(rkStream);
}
//----------------------------------------------------------------------------
void SpotLight::Save (Stream& rkStream)
{
    PointLight::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_kDirection);
    StreamWrite(rkStream,m_fAngle);
    StreamWrite(rkStream,m_fExponent);
}
//----------------------------------------------------------------------------
StringTree* SpotLight::SaveStrings ()
{
    StringTree* pkTree = new StringTree(4,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("direction =",m_kDirection));
    pkTree->SetString(2,MakeString("angle =",m_fAngle));
    pkTree->SetString(3,MakeString("exponent =",m_fExponent));

    // children
    pkTree->SetChild(0,PointLight::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int SpotLight::GetMemoryUsed () const
{
    int iBaseSize = sizeof(SpotLight) - sizeof(PointLight);
    int iTotalSize = iBaseSize + PointLight::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int SpotLight::GetDiskUsed () const
{
    return PointLight::GetDiskUsed() +
        sizeof(m_kDirection) +
        sizeof(m_fAngle) +
        sizeof(m_fExponent);
}
//----------------------------------------------------------------------------
