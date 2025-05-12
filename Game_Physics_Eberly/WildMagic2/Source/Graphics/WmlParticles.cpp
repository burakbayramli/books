// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlRenderer.h"
#include "WmlParticles.h"
#include "WmlTriMesh.h"
using namespace Wml;

WmlImplementRTTI(Particles,Geometry);
WmlImplementStream(Particles);

//----------------------------------------------------------------------------
Particles::Particles (int iPQuantity, Vector3f* akPCenter,
    Vector3f* akPNormal, ColorRGB* akPColor, float* afSize, bool bUseTexture0,
    bool bUseTexture1, bool bUseTexture2, bool bUseTexture3,
    bool bUseTextureBump)
    :
    Geometry(iPQuantity,akPCenter,akPNormal,akPColor,NULL,NULL,NULL,NULL,NULL)
{
    assert( afSize );

    m_iActiveQuantity = iPQuantity;
    m_afSize = afSize;
    m_fSizeAdjust = 1.0f;
    m_bUseTexture0 = bUseTexture0;
    m_bUseTexture1 = bUseTexture1;
    m_bUseTexture2 = bUseTexture2;
    m_bUseTexture3 = bUseTexture3;
    m_bUseTextureBump = bUseTextureBump;

    CreateMesh();
}
//----------------------------------------------------------------------------
Particles::Particles ()
{
    m_iActiveQuantity = 0;
    m_afSize = NULL;
    m_fSizeAdjust = 0.0f;
    m_spkMesh = NULL;
    m_bUseTexture0 = false;
    m_bUseTexture1 = false;
    m_bUseTexture2 = false;
    m_bUseTexture3 = false;
    m_bUseTextureBump = false;
}
//----------------------------------------------------------------------------
Particles::~Particles ()
{
    delete[] m_afSize;
}
//----------------------------------------------------------------------------
void Particles::CreateMesh ()
{
    int i, j;

    // create storage for model space quad vertices
    int iQuantity = 4*m_iVertexQuantity;
    Vector3f* akVertex = new Vector3f[iQuantity];

    // generate normals
    Vector3f* akNormal = NULL;
    if ( m_akNormal )
    {
        akNormal = new Vector3f[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            Vector3f& rkNormal = m_akNormal[i];
            akNormal[j++] = rkNormal;
            akNormal[j++] = rkNormal;
            akNormal[j++] = rkNormal;
            akNormal[j++] = rkNormal;
        }
    }

    // generate colors
    ColorRGB* akColor = NULL;
    if ( m_akColor )
    {
        akColor = new ColorRGB[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            ColorRGB& rkColor = m_akColor[i];
            akColor[j++] = rkColor;
            akColor[j++] = rkColor;
            akColor[j++] = rkColor;
            akColor[j++] = rkColor;
        }
    }

    // generate textures
    Vector2f* akTexture0 = NULL;
    if ( m_bUseTexture0 )
    {
        akTexture0 = new Vector2f[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            akTexture0[j++] = Vector2f(0.0f,0.0f);
            akTexture0[j++] = Vector2f(0.0f,1.0f);
            akTexture0[j++] = Vector2f(1.0f,1.0f);
            akTexture0[j++] = Vector2f(1.0f,0.0f);
        }
    }

    Vector2f* akTexture1 = NULL;
    if ( m_bUseTexture1 )
    {
        akTexture1 = new Vector2f[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            akTexture1[j++] = Vector2f(0.0f,0.0f);
            akTexture1[j++] = Vector2f(0.0f,1.0f);
            akTexture1[j++] = Vector2f(1.0f,1.0f);
            akTexture1[j++] = Vector2f(1.0f,0.0f);
        }
    }

    Vector2f* akTexture2 = NULL;
    if ( m_bUseTexture2 )
    {
        akTexture2 = new Vector2f[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            akTexture2[j++] = Vector2f(0.0f,0.0f);
            akTexture2[j++] = Vector2f(0.0f,1.0f);
            akTexture2[j++] = Vector2f(1.0f,1.0f);
            akTexture2[j++] = Vector2f(1.0f,0.0f);
        }
    }

    Vector2f* akTexture3 = NULL;
    if ( m_bUseTexture3 )
    {
        akTexture3 = new Vector2f[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            akTexture3[j++] = Vector2f(0.0f,0.0f);
            akTexture3[j++] = Vector2f(0.0f,1.0f);
            akTexture3[j++] = Vector2f(1.0f,1.0f);
            akTexture3[j++] = Vector2f(1.0f,0.0f);
        }
    }

    Vector2f* akTextureBump = NULL;
    if ( m_bUseTextureBump )
    {
        akTexture2 = new Vector2f[iQuantity];
        for (i = 0, j = 0; i < m_iVertexQuantity; i++)
        {
            akTextureBump[j++] = Vector2f(0.0f,0.0f);
            akTextureBump[j++] = Vector2f(0.0f,1.0f);
            akTextureBump[j++] = Vector2f(1.0f,1.0f);
            akTextureBump[j++] = Vector2f(1.0f,0.0f);
        }
    }

    // generate connectivity
    int* aiConnect = new int[6*m_iVertexQuantity];
    for (i = 0, j = 0; i < m_iVertexQuantity; i++)
    {
        int iFI = 4*i, iFIp1 = iFI+1, iFIp2 = iFI+2, iFIp3 = iFI+3;
        aiConnect[j++] = iFI;
        aiConnect[j++] = iFIp1;
        aiConnect[j++] = iFIp2;
        aiConnect[j++] = iFI;
        aiConnect[j++] = iFIp2;
        aiConnect[j++] = iFIp3;
    }

    m_spkMesh = new TriMesh(iQuantity,akVertex,akNormal,akColor,akTexture0,
        2*m_iVertexQuantity,aiConnect,akTexture1,akTexture2,akTexture3,
        akTextureBump);
}
//----------------------------------------------------------------------------
void Particles::GenerateParticles (const Camera* pkCamera)
{
    // get up/left in model space
    Vector3f kUpL = (pkCamera->GetUp() + pkCamera->GetLeft())*m_kWorldRotate;
    Vector3f kUmL = (pkCamera->GetUp() - pkCamera->GetLeft())*m_kWorldRotate;

    // generate particle quadrilaterals as pairs of triangles
    Vector3f* akVertex = m_spkMesh->Vertices();
    for (int i = 0, j = 0; i < m_iActiveQuantity; i++)
    {
        Vector3f& rkCenter = m_akVertex[i];

        float fTrueSize = m_fSizeAdjust*m_afSize[i];
        Vector3f kScaledUpL = fTrueSize*kUpL;
        Vector3f kScaledUmL = fTrueSize*kUmL;

        akVertex[j++] = rkCenter - kScaledUpL;
        akVertex[j++] = rkCenter + kScaledUmL;
        akVertex[j++] = rkCenter + kScaledUpL;
        akVertex[j++] = rkCenter - kScaledUmL;
    }
}
//----------------------------------------------------------------------------
void Particles::Draw (Renderer& rkRenderer)
{
    GenerateParticles(rkRenderer.GetCamera());
    Geometry::Draw(rkRenderer);
    rkRenderer.Draw(*this);
}
//----------------------------------------------------------------------------
void Particles::ColorsHaveChanged ()
{
    ColorRGB* akColor = m_spkMesh->Colors();
    if ( !akColor )
        return;

    for (int i = 0, j = 0; i < m_iActiveQuantity; i++)
    {
        // get the "particle" color
        ColorRGB& rkColor = m_akColor[i];

        // assign it as the "quad" color, applied to all four vertices
        akColor[j++] = rkColor;
        akColor[j++] = rkColor;
        akColor[j++] = rkColor;
        akColor[j++] = rkColor;
    }
}
//----------------------------------------------------------------------------
void Particles::NormalsHaveChanged ()
{
    Vector3f* akNormal = m_spkMesh->Normals();
    if ( !akNormal )
        return;

    for (int i = 0, j = 0; i < m_iActiveQuantity; i++)
    {
        // get the "particle" normal
        Vector3f& rkNormal = m_akNormal[i];

        // assign it as the "quad" normal, applied to all four vertices
        akNormal[j++] = rkNormal;
        akNormal[j++] = rkNormal;
        akNormal[j++] = rkNormal;
        akNormal[j++] = rkNormal;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Particles::Factory (Stream& rkStream)
{
    Particles* pkObject = new Particles;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void Particles::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Load(rkStream,pkLink);

    // NOTE:  Particles used to be derived from Polypoint.  Polypoint has one
    // streamed member, m_iActiveQuantity.  Geometry::Load gets everything
    // that Polypoint::Load had gotten, except m_iActiveQuantity.  However,
    // the new Particles::Save writes m_iActiveQuantity in the same location
    // that Polypoint::Save did, so no stream version checking has to occur
    // here to handle m_iActiveQuantity.
    StreamRead(rkStream,m_iActiveQuantity);
    StreamRead(rkStream,m_fSizeAdjust);
    StreamReadBool(rkStream,m_bUseTexture0);
    m_afSize = new float[m_iVertexQuantity];
    StreamRead(rkStream,m_afSize,m_iVertexQuantity);

    if ( rkStream.GetVersion() >= Version(1,4) )
    {
        StreamReadBool(rkStream,m_bUseTexture1);
        StreamReadBool(rkStream,m_bUseTexture2);
        StreamReadBool(rkStream,m_bUseTexture3);
        StreamReadBool(rkStream,m_bUseTextureBump);
    }
    else
    {
        m_bUseTexture1 = false;
        m_bUseTexture2 = false;
        m_bUseTexture3 = false;
        m_bUseTextureBump = false;
    }

    CreateMesh();
}
//----------------------------------------------------------------------------
void Particles::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Geometry::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Particles::Register (Stream& rkStream)
{
    return Geometry::Register(rkStream);
}
//----------------------------------------------------------------------------
void Particles::Save (Stream& rkStream)
{
    Geometry::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iActiveQuantity);
    StreamWrite(rkStream,m_fSizeAdjust);
    StreamWriteBool(rkStream,m_bUseTexture0);
    StreamWrite(rkStream,m_afSize,m_iVertexQuantity);
    StreamWriteBool(rkStream,m_bUseTexture1);
    StreamWriteBool(rkStream,m_bUseTexture2);
    StreamWriteBool(rkStream,m_bUseTexture3);
    StreamWriteBool(rkStream,m_bUseTextureBump);
}
//----------------------------------------------------------------------------
StringTree* Particles::SaveStrings ()
{
    StringTree* pkTree = new StringTree(8,0,2,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("active quantity =",m_iActiveQuantity));
    pkTree->SetString(2,MakeString("size adjust =",m_fSizeAdjust));
    pkTree->SetString(3,MakeString("use texture0 =",m_bUseTexture0));
    pkTree->SetString(4,MakeString("use texture1 =",m_bUseTexture1));
    pkTree->SetString(5,MakeString("use texture2 =",m_bUseTexture2));
    pkTree->SetString(6,MakeString("use texture3 =",m_bUseTexture3));
    pkTree->SetString(7,MakeString("use textureB =",m_bUseTextureBump));

    // children
    pkTree->SetChild(0,Geometry::SaveStrings());

    StringTree* pkSizeTree = new StringTree(m_iVertexQuantity+1,0,0,0);
    pkSizeTree->SetString(0,MakeString("sizes"));
    char acDummy[32];
    for (int i = 0; i < m_iVertexQuantity; i++)
    {
        sprintf(acDummy,"%f",m_afSize[i]);
        pkSizeTree->SetString(i+1,MakeString(acDummy));
    }

    pkTree->SetChild(1,pkSizeTree);

    return pkTree;
}
//----------------------------------------------------------------------------
int Particles::GetMemoryUsed () const
{
    int iBaseSize = sizeof(Particles) - sizeof(Geometry);
    int iDynaSize =
        m_iVertexQuantity*sizeof(m_afSize[0]) +
        m_spkMesh->GetMemoryUsed();

    int iTotalSize = iBaseSize + iDynaSize + Geometry::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Particles::GetDiskUsed () const
{
    return Geometry::GetDiskUsed() +
        sizeof(m_iActiveQuantity) +
        sizeof(m_fSizeAdjust) +
        m_iVertexQuantity*sizeof(m_afSize[0]) +
        StreamBytesBool(m_bUseTexture0) +
        StreamBytesBool(m_bUseTexture1) +
        StreamBytesBool(m_bUseTexture2) +
        StreamBytesBool(m_bUseTexture3) +
        StreamBytesBool(m_bUseTextureBump);
}
//----------------------------------------------------------------------------
