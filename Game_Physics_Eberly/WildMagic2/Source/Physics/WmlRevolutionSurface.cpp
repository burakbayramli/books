// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlRevolutionSurface.h"
#include "WmlStandardMesh.h"
using namespace Wml;

WmlImplementRTTI(RevolutionSurface,TriMesh);
WmlImplementStream(RevolutionSurface);

//----------------------------------------------------------------------------
RevolutionSurface::RevolutionSurface (Curve2f* pkCurve, float fXCenter,
    Topology eTopology, int iCurveSamples, int iRadialSamples,
    bool bWantNormals, bool bWantColors, bool bWantUVs,
    bool bSampleByArcLength, bool bOutsideView)
{
    assert( pkCurve );
    m_pkCurve = pkCurve;
    m_fXCenter = fXCenter;
    m_eTopology = eTopology;
    m_iCurveSamples = iCurveSamples;
    m_iRadialSamples = iRadialSamples;
    m_bSampleByArcLength = bSampleByArcLength;

    ComputeSampleData();

    // The topology of the meshes is all that matters.  The vertices will be
    // modified later based on the curve of revolution.
    TriMesh* pkMesh = (TriMesh*)this;
    switch ( m_eTopology )
    {
    case REV_DISK_TOPOLOGY:
        CreateDiskMesh(pkMesh,m_iCurveSamples,m_iRadialSamples,
            Vector3f::ZERO,1.0f,Vector3f::UNIT_X,Vector3f::UNIT_Y,
            Vector3f::UNIT_Z,bWantNormals,bWantColors,bWantUVs);
        break;
    case REV_CYLINDER_TOPOLOGY:
        CreateCylinderMesh(pkMesh,m_iCurveSamples,m_iRadialSamples,
            Vector3f::ZERO,Vector3f::UNIT_X,Vector3f::UNIT_Y,Vector3f::UNIT_Z,
            1.0f,1.0f,bWantNormals,bWantColors,bWantUVs,bOutsideView);
        break;
    case REV_SPHERE_TOPOLOGY:
        CreateSphereMesh(pkMesh,m_iCurveSamples,m_iRadialSamples,
            Vector3f::ZERO,1.0f,Vector3f::UNIT_X,Vector3f::UNIT_Y,
            Vector3f::UNIT_Z,bWantNormals,bWantColors,bWantUVs,bOutsideView);
        break;
    case REV_TORUS_TOPOLOGY:
        CreateTorusMesh(pkMesh,m_iCurveSamples,m_iRadialSamples,
            Vector3f::ZERO,Vector3f::UNIT_X,Vector3f::UNIT_Y,Vector3f::UNIT_Z,
            1.0f,0.25f,bWantNormals,bWantColors,bWantUVs,bOutsideView);
        break;
    }

    // generate the actual surface by replacing the vertex values
    UpdateSurface();
}
//----------------------------------------------------------------------------
RevolutionSurface::RevolutionSurface ()
{
    m_pkCurve = NULL;
    m_afSin = NULL;
    m_afCos = NULL;
    m_akSample = NULL;
}
//----------------------------------------------------------------------------
RevolutionSurface::~RevolutionSurface ()
{
    delete[] m_afSin;
    delete[] m_afCos;
    delete[] m_akSample;
}
//----------------------------------------------------------------------------
void RevolutionSurface::ComputeSampleData ()
{
    // Compute slice vertex coefficients.  The first and last coefficients
    // are duplicated to allow a closed cross section that has two different
    // pairs of texture coordinates at the shared vertex.
    m_afSin = new float[m_iRadialSamples+1];
    m_afCos = new float[m_iRadialSamples+1];
    float fInvRS = 1.0f/(float)m_iRadialSamples;
    for (int i = 0; i < m_iRadialSamples; i++)
    {
        float fAngle = Mathf::TWO_PI*fInvRS*i;
        m_afCos[i] = Mathf::Cos(fAngle);
        m_afSin[i] = Mathf::Sin(fAngle);
    }
    m_afSin[m_iRadialSamples] = m_afSin[0];
    m_afCos[m_iRadialSamples] = m_afCos[0];

    // allocate storage for curve samples
    m_akSample = new Vector3f[m_iCurveSamples];
}
//----------------------------------------------------------------------------
void RevolutionSurface::UpdateSurface ()
{
    // parameters for evaluating curve
    float fTMin = m_pkCurve->GetMinTime();
    float fTRange = m_pkCurve->GetMaxTime() - fTMin;

    // sampling by arc length requires the total length of the curve
    float fTotalLength;
    if ( m_bSampleByArcLength )
        fTotalLength = m_pkCurve->GetTotalLength();
    else
        fTotalLength = 0.0f;

    // sample the curve of revolution
    float fInvCSm1 = 1.0f/(float)(m_iCurveSamples - 1);
    for (int i = 0; i < m_iCurveSamples; i++)
    {
        float fT;
        if ( m_bSampleByArcLength )
            fT = m_pkCurve->GetTime(i*fTotalLength*fInvCSm1);
        else
            fT = fTMin + i*fTRange*fInvCSm1;

        Vector2f kPos = m_pkCurve->GetPosition(fT);
        m_akSample[i].X() = kPos.X();
        m_akSample[i].Y() = 0.0f;
        m_akSample[i].Z() = kPos.Y();
    }

    // Store the samples and their rotated equivalents.  The storage layout
    // is dependent on the topology of the mesh.
    switch ( m_eTopology )
    {
    case REV_DISK_TOPOLOGY:
        UpdateDisk();
        break;
    case REV_CYLINDER_TOPOLOGY:
        UpdateCylinder();
        break;
    case REV_SPHERE_TOPOLOGY:
        UpdateSphere();
        break;
    case REV_TORUS_TOPOLOGY:
        UpdateTorus();
        break;
    }

    if ( m_akNormal )
        UpdateModelNormals();
}
//----------------------------------------------------------------------------
void RevolutionSurface::UpdateDisk ()
{
    // initial ray
    int iC;
    for (iC = 0; iC < m_iCurveSamples; iC++)
        m_akVertex[iC] = m_akSample[iC];

    // remaining rays obtained by revolution
    int iCSm1 = m_iCurveSamples - 1;
    for (int iR = 1; iR < m_iRadialSamples; iR++)
    {
        for (iC = 1; iC < m_iCurveSamples; iC++)
        {
            float fRadius = m_akSample[iC].X() - m_fXCenter;
            if ( fRadius < 0.0f )
                fRadius = 0.0f;
            int i = iC+iCSm1*iR;
            m_akVertex[i].X() = m_fXCenter + fRadius*m_afCos[iR];
            m_akVertex[i].Y() = fRadius*m_afSin[iR];
            m_akVertex[i].Z() = m_akSample[iC].Z();
        }
    }
}
//----------------------------------------------------------------------------
void RevolutionSurface::UpdateSphere ()
{
    // south pole
    m_akVertex[m_iVertexQuantity-2] = m_akSample[0];

    // north pole
    m_akVertex[m_iVertexQuantity-1] = m_akSample[m_iCurveSamples-1];

    // initial and final ray
    int iC, i;
    for (iC = 1; iC <= m_iCurveSamples-2; iC++)
    {
        i = (iC-1)*(m_iRadialSamples+1);
        m_akVertex[i] = m_akSample[iC];

        i += m_iRadialSamples;
        m_akVertex[i] = m_akSample[iC];
    }

    // remaining rays obtained by revolution
    for (int iR = 1; iR < m_iRadialSamples; iR++)
    {
        for (iC = 1; iC <= m_iCurveSamples-2; iC++)
        {
            float fRadius = m_akSample[iC].X() - m_fXCenter;
            if ( fRadius < 0.0f )
                fRadius = 0.0f;
            i = (iC-1)*(m_iRadialSamples+1) + iR;
            m_akVertex[i].X() = m_fXCenter + fRadius*m_afCos[iR];
            m_akVertex[i].Y() = fRadius*m_afSin[iR];
            m_akVertex[i].Z() = m_akSample[iC].Z();
        }
    }
}
//----------------------------------------------------------------------------
void RevolutionSurface::UpdateCylinder ()
{
    // initial and final ray
    int iC, i;
    for (iC = 0; iC < m_iCurveSamples; iC++)
    {
        i = iC*(m_iRadialSamples+1);
        m_akVertex[i] = m_akSample[iC];

        i += m_iRadialSamples;
        m_akVertex[i] = m_akSample[iC];
    }

    // remaining rays obtained by revolution
    for (int iR = 1; iR < m_iRadialSamples; iR++)
    {
        for (iC = 0; iC < m_iCurveSamples; iC++)
        {
            float fRadius = m_akSample[iC].X() - m_fXCenter;
            if ( fRadius < 0.0f )
                fRadius = 0.0f;
            i = iC*(m_iRadialSamples+1) + iR;
            m_akVertex[i].X() = m_fXCenter + fRadius*m_afCos[iR];
            m_akVertex[i].Y() = fRadius*m_afSin[iR];
            m_akVertex[i].Z() = m_akSample[iC].Z();
        }
    }
}
//----------------------------------------------------------------------------
void RevolutionSurface::UpdateTorus ()
{
    // initial and final ray
    int iC, i;
    for (iC = 0; iC < m_iCurveSamples; iC++)
    {
        i = iC*(m_iRadialSamples+1);
        m_akVertex[i] = m_akSample[iC];

        i += m_iRadialSamples;
        m_akVertex[i] = m_akSample[iC];
    }

    // remaining rays obtained by revolution
    int iR;
    for (iR = 1; iR < m_iRadialSamples; iR++)
    {
        for (iC = 0; iC < m_iCurveSamples; iC++)
        {
            float fRadius = m_akSample[iC].X() - m_fXCenter;
            if ( fRadius < 0.0f )
                fRadius = 0.0f;
            i = iC*(m_iRadialSamples+1) + iR;
            m_akVertex[i].X() = m_fXCenter + fRadius*m_afCos[iR];
            m_akVertex[i].Y() = fRadius*m_afSin[iR];
            m_akVertex[i].Z() = m_akSample[iC].Z();
        }
    }

    i = m_iVertexQuantity - (m_iRadialSamples+1);
    for (iR = 0; iR <= m_iRadialSamples; iR++, i++)
        m_akVertex[i] = m_akVertex[iR];
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* RevolutionSurface::Factory (Stream& rkStream)
{
    RevolutionSurface* pkObject = new RevolutionSurface;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void RevolutionSurface::Load (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_fXCenter);
    StreamReadEnum(rkStream,m_eTopology);
    StreamRead(rkStream,m_iCurveSamples);
    StreamRead(rkStream,m_iRadialSamples);
    StreamRead(rkStream,m_afSin,m_iRadialSamples+1);
    StreamRead(rkStream,m_afCos,m_iRadialSamples+1);
    StreamReadBool(rkStream,m_bSampleByArcLength);

    // TO DO.  See note in RevolutionSurface::Save.
    m_pkCurve = NULL;
}
//----------------------------------------------------------------------------
void RevolutionSurface::Link (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool RevolutionSurface::Register (Stream& rkStream)
{
    return TriMesh::Register(rkStream);
}
//----------------------------------------------------------------------------
void RevolutionSurface::Save (Stream& rkStream)
{
    TriMesh::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_fXCenter);
    StreamWriteEnum(rkStream,m_eTopology);
    StreamWrite(rkStream,m_iCurveSamples);
    StreamWrite(rkStream,m_iRadialSamples);
    StreamWrite(rkStream,m_afSin,m_iRadialSamples+1);
    StreamWrite(rkStream,m_afCos,m_iRadialSamples+1);
    StreamWriteBool(rkStream,m_bSampleByArcLength);

    // TO DO.  The class Curve2 is abstract and does not know about the data
    // representation for the derived-class object that is passed to the
    // RevolutionSurface constructor.  Because of this, any loaded
    // RevolutionSurface object will require the application to manually set
    // the curve via the Curve() member.
    //
    // Streaming support should probably be added to the curve classes and
    // a mechanism for saving function pointers should probably be created.
}
//----------------------------------------------------------------------------
StringTree* RevolutionSurface::SaveStrings ()
{
    StringTree* pkTree = new StringTree(6,0,1,0);

    // strings
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetString(1,MakeString("x center = ",m_fXCenter));
    switch ( m_eTopology )
    {
    case REV_CYLINDER_TOPOLOGY:
        pkTree->SetString(2,MakeString("topology = CYLINDER"));
        break;
    case REV_DISK_TOPOLOGY:
        pkTree->SetString(2,MakeString("topology = DISK"));
        break;
    case REV_SPHERE_TOPOLOGY:
        pkTree->SetString(2,MakeString("topology = SPHERE"));
        break;
    case REV_TORUS_TOPOLOGY:
        pkTree->SetString(2,MakeString("topology = TORUS"));
        break;
    }
    pkTree->SetString(3,MakeString("curve samples = ",m_iCurveSamples));
    pkTree->SetString(4,MakeString("radial samples = ",m_iRadialSamples));
    pkTree->SetString(5,MakeString("sample by arc length = ",
        m_bSampleByArcLength));

    // children
    pkTree->SetChild(0,TriMesh::SaveStrings());

    return pkTree;
}
//----------------------------------------------------------------------------
int RevolutionSurface::GetMemoryUsed () const
{
    // TO DO.  Does not count the size of the Curve2 object.  The curve class
    // is virtual, so that system would need to provide a GetBytesUsed member
    // function.
    int iBaseSize = sizeof(RevolutionSurface) - sizeof(TriMesh);
    int iDynaSize = 2*(m_iRadialSamples+1)*sizeof(float);  // m_afSin, m_afCos
    int iTotalSize = iBaseSize + iDynaSize + TriMesh::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int RevolutionSurface::GetDiskUsed () const
{
    return TriMesh::GetDiskUsed() +
        StreamBytesEnum(m_eTopology) +
        sizeof(m_iCurveSamples) +
        sizeof(m_iRadialSamples) +
        2*(m_iRadialSamples+1)*sizeof(float) +  // m_afSin, m_afCos
        StreamBytesBool(m_bSampleByArcLength);
}
//----------------------------------------------------------------------------
