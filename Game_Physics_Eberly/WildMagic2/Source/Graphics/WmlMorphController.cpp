// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlGeometry.h"
#include "WmlMorphController.h"
using namespace Wml;

WmlImplementRTTI(MorphController,Controller);
WmlImplementStream(MorphController);

//----------------------------------------------------------------------------
MorphController::MorphController ()
{
    m_iTargetQuantity = 0;
    m_aakVertex = NULL;
    m_bUpdateNormals = false;
    m_iKeyQuantity = 0;
    m_afTime = NULL;
    m_aafWeight = NULL;
    m_iLastIndex = 0;
}
//----------------------------------------------------------------------------
MorphController::~MorphController ()
{
    DeleteTargets();
    DeleteTimes();
    DeleteWeights();
}
//----------------------------------------------------------------------------
void MorphController::DeleteTargets ()
{
    if ( m_aakVertex )
    {
        for (int i = 0; i < m_iTargetQuantity; i++)
            delete[] m_aakVertex[i];
        delete[] m_aakVertex;
        m_aakVertex = NULL;
    }
}
//----------------------------------------------------------------------------
void MorphController::DeleteTimes ()
{
    delete[] m_afTime;
    m_afTime = NULL;
}
//----------------------------------------------------------------------------
void MorphController::DeleteWeights ()
{
    if ( m_aafWeight )
    {
        for (int i = 0; i < m_iKeyQuantity; i++)
            delete[] m_aafWeight[i];
        delete[] m_aafWeight;
        m_aafWeight = NULL;
    }
}
//----------------------------------------------------------------------------
void MorphController::SetTargetQuantity (int iTargetQuantity)
{
    assert( iTargetQuantity > 0 );

    DeleteTargets();
    m_iTargetQuantity = iTargetQuantity;
    m_aakVertex = new Vector3f*[m_iTargetQuantity];
    memset(m_aakVertex,0,m_iTargetQuantity*sizeof(Vector3f*));
}
//----------------------------------------------------------------------------
void MorphController::SetKeyQuantity (int iKeyQuantity)
{
    assert( iKeyQuantity > 0 );

    DeleteTimes();
    DeleteWeights();
    m_iKeyQuantity = iKeyQuantity;
    m_aafWeight = new float*[m_iKeyQuantity];
    memset(m_aafWeight,0,m_iKeyQuantity*sizeof(float*));
}
//----------------------------------------------------------------------------
void MorphController::GetKeyInfo (float fCtrlTime, float& rfTime,
    float& rfOmTime, int& ri0, int& ri1)
{
    if ( fCtrlTime <= m_afTime[0] )
    {
        rfTime = 0.0f;
        rfOmTime = 1.0f;
        m_iLastIndex = 0;
        ri0 = 0;
        ri1 = 0;
        return;
    }

    if ( fCtrlTime >= m_afTime[m_iKeyQuantity-1] )
    {
        rfTime = 0.0f;
        rfOmTime = 1.0f;
        m_iLastIndex = m_iKeyQuantity - 1;
        ri0 = m_iLastIndex;
        ri1 = m_iLastIndex;
        return;
    }

    int iNextIndex;
    if ( fCtrlTime > m_afTime[m_iLastIndex] )
    {
        iNextIndex = m_iLastIndex + 1;
        while ( fCtrlTime >= m_afTime[iNextIndex] )
        {
            m_iLastIndex = iNextIndex;
            iNextIndex++;
        }

        ri0 = m_iLastIndex;
        ri1 = iNextIndex;
        rfTime = (fCtrlTime-m_afTime[ri0])/(m_afTime[ri1]-m_afTime[ri0]);
    }
    else if ( fCtrlTime < m_afTime[m_iLastIndex] )
    {
        iNextIndex = m_iLastIndex - 1;
        while ( fCtrlTime <= m_afTime[iNextIndex] )
        {
            m_iLastIndex = iNextIndex;
            iNextIndex--;
        }

        ri0 = iNextIndex;
        ri1 = m_iLastIndex;
        rfTime = (fCtrlTime-m_afTime[ri0])/(m_afTime[ri1]-m_afTime[ri0]);
    }
    else
    {
        rfTime = 0.0f;
        ri0 = m_iLastIndex;
        ri1 = m_iLastIndex;
    }

    rfOmTime = 1.0f - rfTime;
}
//----------------------------------------------------------------------------
bool MorphController::Update (float fAppTime)
{
    // The key interpolation uses linear interpolation.  To get higher-order
    // interpolation, you need to provide a more sophisticated key (Bezier
    // cubic or TCB spline, for example).

    if ( !Active() )
    {
        // controller does not compute world transform
        return false;
    }

    // set vertices to target[0]
    Geometry* pkGeom = (Geometry*) m_pkObject;
    int iVertexQuantity = pkGeom->GetVertexQuantity();
    Vector3f* akVertex = pkGeom->Vertices();
    memcpy(akVertex,m_aakVertex[0],iVertexQuantity*sizeof(Vector3f));

    // lookup the bounding keys
    float fCtrlTime = GetControlTime(fAppTime);
    float fNT, fOmNT;
    int i0, i1;
    GetKeyInfo(fCtrlTime,fNT,fOmNT,i0,i1);

    // add the remaining components in the convex composition
    for (int i = 1; i < m_iTargetQuantity; i++)
    {
        float fCoeff = fOmNT*m_aafWeight[i0][i-1] + fNT*m_aafWeight[i1][i-1];
        for (int j = 0; j < iVertexQuantity; j++)
            akVertex[j] += fCoeff*m_aakVertex[i][j];
    }

    // controller does not compute world transform
    return false;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* MorphController::Factory (Stream& rkStream)
{
    MorphController* pkObject = new MorphController;
    Stream::Link* pkLink = new Stream::Link(pkObject);
    pkObject->Load(rkStream,pkLink);
    return pkObject;
}
//----------------------------------------------------------------------------
void MorphController::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Load(rkStream,pkLink);

    StreamRead(rkStream,m_iTargetQuantity);
    SetTargetQuantity(m_iTargetQuantity);

    int iVertexQuantity;
    StreamRead(rkStream,iVertexQuantity);
    int i;
    for (i = 0; i < m_iTargetQuantity; i++)
    {
        Vector3f* akVertex = new Vector3f[iVertexQuantity];
        StreamRead(rkStream,akVertex,iVertexQuantity);
        SetVertices(i,akVertex);
    }

    StreamReadBool(rkStream,m_bUpdateNormals);

    StreamRead(rkStream,m_iKeyQuantity);
    SetKeyQuantity(m_iKeyQuantity);

    float* afTime = new float[m_iKeyQuantity];
    StreamRead(rkStream,afTime,m_iKeyQuantity);
    SetTimes(afTime);

    if ( rkStream.GetVersion() == Version(1,0) )
    {
        // The weight array is [keyQuantity][targetQuantity-1] as indicated
        // in the header file.  This code just happened to work for the
        // MorphingFace sample because keyQuantity > targetQuantity in
        // Face.mgc.
        for (i = 0; i < m_iKeyQuantity; i++)
        {
            float* afWeight = new float[m_iKeyQuantity-1];
            StreamRead(rkStream,afWeight,m_iKeyQuantity-1);
            SetWeights(i,afWeight);
        }
    }
    else
    {
        // This is the correct streaming for the weights.
        for (i = 0; i < m_iKeyQuantity; i++)
        {
            float* afWeight = new float[m_iTargetQuantity-1];
            StreamRead(rkStream,afWeight,m_iTargetQuantity-1);
            SetWeights(i,afWeight);
        }
    }
}
//----------------------------------------------------------------------------
void MorphController::Link (Stream& rkStream, Stream::Link* pkLink)
{
    Controller::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool MorphController::Register (Stream& rkStream)
{
    return Controller::Register(rkStream);
}
//----------------------------------------------------------------------------
void MorphController::Save (Stream& rkStream)
{
    Controller::Save(rkStream);

    StreamWrite(rkStream,m_iTargetQuantity);

    Geometry* pkGeom = (Geometry*) m_pkObject;
    int iVertexQuantity = pkGeom->GetVertexQuantity();
    StreamWrite(rkStream,iVertexQuantity);
    int i;
    for (i = 0; i < m_iTargetQuantity; i++)
        StreamWrite(rkStream,m_aakVertex[i],iVertexQuantity);

    StreamWriteBool(rkStream,m_bUpdateNormals);

    StreamWrite(rkStream,m_iKeyQuantity);
    StreamWrite(rkStream,m_afTime,m_iKeyQuantity);
    for (i = 0; i < m_iKeyQuantity; i++)
        StreamWrite(rkStream,m_aafWeight[i],m_iTargetQuantity-1);
}
//----------------------------------------------------------------------------
StringTree* MorphController::SaveStrings ()
{
    // TO DO.  Finish implementation.
    StringTree* pkTree = new StringTree(1,0,1,0);
    pkTree->SetString(0,MakeString(&ms_kRTTI,GetName()));
    pkTree->SetChild(0,Controller::SaveStrings());
    return pkTree;
}
//----------------------------------------------------------------------------
int MorphController::GetMemoryUsed () const
{
    int iBaseSize = sizeof(MorphController) - sizeof(Controller);

    Geometry* pkGeom = (Geometry*) m_pkObject;
    int iVertexQuantity = pkGeom->GetVertexQuantity();
    int iDynaSize =
        m_iTargetQuantity*iVertexQuantity*sizeof(m_aakVertex[0][0]) +
        m_iKeyQuantity*sizeof(m_afTime[0]) +
        m_iKeyQuantity*(m_iTargetQuantity-1)*sizeof(m_aafWeight[0][0]);

    int iTotalSize = iBaseSize + iDynaSize + Controller::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int MorphController::GetDiskUsed () const
{
    int iSize = Controller::GetDiskUsed() +
        sizeof(m_iTargetQuantity);

    Geometry* pkGeom = (Geometry*) m_pkObject;
    int iVertexQuantity = pkGeom->GetVertexQuantity();
    iSize += sizeof(iVertexQuantity) +
        m_iTargetQuantity*iVertexQuantity*sizeof(m_aakVertex[0][0]);

    iSize += StreamBytesBool(m_bUpdateNormals);

    iSize += sizeof(m_iKeyQuantity) +
        m_iKeyQuantity*sizeof(m_afTime[0]) +
        m_iKeyQuantity*(m_iTargetQuantity-1)*sizeof(m_aafWeight[0][0]);

    return iSize;
}
//----------------------------------------------------------------------------
