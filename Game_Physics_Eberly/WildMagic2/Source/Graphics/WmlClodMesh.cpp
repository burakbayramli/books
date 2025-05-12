// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlClodMesh.h"
#include "WmlCreateClodMesh.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(ClodMesh,TriMesh);
WmlImplementStream(ClodMesh);

//----------------------------------------------------------------------------
ClodMesh::ClodMesh (int iVertexQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
    int iTriangleQuantity, int* aiConnect, int iRecordQuantity,
    CollapseRecord* akRecord)
    :
    TriMesh(iVertexQuantity,akVertex,akNormal,akColor,akTexture,
        iTriangleQuantity,aiConnect)
{
    m_iTargetRecord = 0;
    m_iCurrentRecord = 0;

    if ( iRecordQuantity > 0 )
    {
        m_iRecordQuantity = iRecordQuantity;
        m_akRecord = akRecord;
    }
    else
    {
        CreateClodMesh(iVertexQuantity,akVertex,akNormal,akColor,akTexture,
            iTriangleQuantity,aiConnect,m_iRecordQuantity,m_akRecord);
    }

    UpdateModelBound();
}
//----------------------------------------------------------------------------
ClodMesh::ClodMesh ()
{
    m_iTargetRecord = 0;
    m_iCurrentRecord = 0;
    m_iRecordQuantity = 0;
    m_akRecord = NULL;
}
//----------------------------------------------------------------------------
ClodMesh::~ClodMesh ()
{
    delete[] m_akRecord;
}
//----------------------------------------------------------------------------
void ClodMesh::SelectLevelOfDetail ()
{
    // Get target record.  The virtual function may be overridden by a derived
    // class to obtain a desired automated change in the target.
    int iTargetRecord = GetAutomatedTargetRecord();

    // collapse mesh (if necessary)
    int i, iC;
    while ( m_iCurrentRecord < iTargetRecord )
    {
        m_iCurrentRecord++;

        // replace indices in connectivity array
        CollapseRecord& rkRecord = m_akRecord[m_iCurrentRecord];
        for (i = 0; i < rkRecord.m_iIQuantity; i++)
        {
            iC = rkRecord.m_aiIndex[i];
            assert( m_aiConnect[iC] == rkRecord.m_iVThrow );
            m_aiConnect[iC] = rkRecord.m_iVKeep;
        }

        // reduce vertex count (vertices are properly ordered)
        m_iVertexQuantity = rkRecord.m_iVQuantity;

        // reduce triangle count (triangles are properly ordered)
        m_iTriangleQuantity = rkRecord.m_iTQuantity;
    }

    // expand mesh (if necessary)
    while ( m_iCurrentRecord > iTargetRecord )
    {
        // restore indices in connectivity array
        CollapseRecord& rkRecord = m_akRecord[m_iCurrentRecord];
        for (i = 0; i < rkRecord.m_iIQuantity; i++)
        {
            iC = rkRecord.m_aiIndex[i];
            assert( m_aiConnect[iC] == rkRecord.m_iVKeep );
            m_aiConnect[iC] = rkRecord.m_iVThrow;
        }

        m_iCurrentRecord--;
        CollapseRecord& rkPrevRecord = m_akRecord[m_iCurrentRecord];

        // increase vertex count (vertices are properly ordered)
        m_iVertexQuantity = rkPrevRecord.m_iVQuantity;

        // increase triangle count (triangles are properly ordered)
        m_iTriangleQuantity = rkPrevRecord.m_iTQuantity;
    }
}
//----------------------------------------------------------------------------
void ClodMesh::Draw (Renderer& rkRenderer)
{
    SelectLevelOfDetail();
    TriMesh::Draw(rkRenderer);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ClodMesh::Factory (Stream& rkStream)
{
    ClodMesh* pkObject = new ClodMesh;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void ClodMesh::Load (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Load(rkStream,pkLink);

    // native data
    StreamRead(rkStream,m_iCurrentRecord);
    StreamRead(rkStream,m_iTargetRecord);
    StreamRead(rkStream,m_iRecordQuantity);
    if ( m_iRecordQuantity )
    {
        m_akRecord = new CollapseRecord[m_iRecordQuantity];
        for (int i = 0; i < m_iRecordQuantity; i++)
            m_akRecord[i].Read(rkStream);
    }
    else
    {
        m_akRecord = NULL;
    }
}
//----------------------------------------------------------------------------
void ClodMesh::Link (Stream& rkStream, Stream::Link* pkLink)
{
    TriMesh::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ClodMesh::Register (Stream& rkStream)
{
    return TriMesh::Register(rkStream);
}
//----------------------------------------------------------------------------
void ClodMesh::Save (Stream& rkStream)
{
    TriMesh::Save(rkStream);

    // native data
    StreamWrite(rkStream,m_iCurrentRecord);
    StreamWrite(rkStream,m_iTargetRecord);
    StreamWrite(rkStream,m_iRecordQuantity);
    for (int i = 0; i < m_iRecordQuantity; i++)
        m_akRecord[i].Write(rkStream);
}
//----------------------------------------------------------------------------
StringTree* ClodMesh::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,TriMesh::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int ClodMesh::GetMemoryUsed () const
{
    int iBaseSize = sizeof(ClodMesh) - sizeof(TriMesh);

    int iDynaSize = 0;
    for (int i = 0; i < m_iRecordQuantity; i++)
        iDynaSize += m_akRecord[i].GetMemoryUsed();

    int iTotalSize = iBaseSize + iDynaSize + TriMesh::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ClodMesh::GetDiskUsed () const
{
    int iSize = TriMesh::GetDiskUsed() +
        sizeof(m_iCurrentRecord) +
        sizeof(m_iTargetRecord) +
        sizeof(m_iRecordQuantity);

    for (int i = 0; i < m_iRecordQuantity; i++)
        iSize += m_akRecord[i].GetDiskUsed();

    return iSize;
}
//----------------------------------------------------------------------------
