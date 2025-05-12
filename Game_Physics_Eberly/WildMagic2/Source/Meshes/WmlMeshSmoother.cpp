// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlMeshSmoother.h"
using namespace Wml;

//----------------------------------------------------------------------------
template <class Real>
MeshSmoother<Real>::MeshSmoother ()
{
    m_iVQuantity = 0;
    m_akVertex = NULL;
    m_iTQuantity = 0;
    m_aiConnect = NULL;
    m_akNormal = NULL;
    m_akMean = NULL;
    m_aiNeighborCount = NULL;
}
//----------------------------------------------------------------------------
template <class Real>
MeshSmoother<Real>::MeshSmoother (int iVQuantity, Vector3<Real>* akVertex,
    int iTQuantity, int* aiConnect)
{
    m_akVertex = NULL;
    m_akNormal = NULL;
    m_aiConnect = NULL;
    m_akMean = NULL;
    m_aiNeighborCount = NULL;

    Create(iVQuantity,akVertex,iTQuantity,aiConnect);
}
//----------------------------------------------------------------------------
template <class Real>
MeshSmoother<Real>::~MeshSmoother ()
{
    Destroy();
}
//----------------------------------------------------------------------------
template <class Real>
void MeshSmoother<Real>::Create (int iVQuantity, Vector3<Real>* akVertex,
    int iTQuantity, int* aiConnect)
{
    // remove previously allocated smoother data
    Destroy();

    m_iVQuantity = iVQuantity;
    m_akVertex = akVertex;
    m_iTQuantity = iTQuantity;
    m_aiConnect = aiConnect;

    m_akNormal = new Vector3<Real>[m_iVQuantity];
    m_akMean = new Vector3<Real>[m_iVQuantity];
    m_aiNeighborCount = new int[m_iVQuantity];

    // count the number of vertex neighbors
    memset(m_aiNeighborCount,0,m_iVQuantity*sizeof(int));
    int* piConnect = m_aiConnect;
    for (int i = 0; i < m_iTQuantity; i++)
    {
        m_aiNeighborCount[*piConnect++] += 2;
        m_aiNeighborCount[*piConnect++] += 2;
        m_aiNeighborCount[*piConnect++] += 2;
    }
}
//----------------------------------------------------------------------------
template <class Real>
void MeshSmoother<Real>::Destroy ()
{
    delete[] m_akNormal;
    delete[] m_akMean;
    delete[] m_aiNeighborCount;
}
//----------------------------------------------------------------------------
template <class Real>
void MeshSmoother<Real>::Update (Real fTime)
{
    memset(m_akNormal,0,m_iVQuantity*sizeof(Vector3<Real>));
    memset(m_akMean,0,m_iVQuantity*sizeof(Vector3<Real>));

    int* piConnect = m_aiConnect;
    int i;
    for (i = 0; i < m_iTQuantity; i++)
    {
        int iV0 = *piConnect++;
        int iV1 = *piConnect++;
        int iV2 = *piConnect++;

        Vector3<Real>& rkV0 = m_akVertex[iV0];
        Vector3<Real>& rkV1 = m_akVertex[iV1];
        Vector3<Real>& rkV2 = m_akVertex[iV2];

        Vector3<Real> kEdge1 = rkV1 - rkV0;
        Vector3<Real> kEdge2 = rkV2 - rkV0;
        Vector3<Real> kNormal = kEdge1.Cross(kEdge2);

        m_akNormal[iV0] += kNormal;
        m_akNormal[iV1] += kNormal;
        m_akNormal[iV2] += kNormal;

        m_akMean[iV0] += rkV1 + rkV2;
        m_akMean[iV1] += rkV2 + rkV0;
        m_akMean[iV2] += rkV0 + rkV1;
    }

    for (i = 0; i < m_iVQuantity; i++)
    {
        m_akNormal[i].Normalize();
        m_akMean[i] /= (Real)m_aiNeighborCount[i];
    }

    for (i = 0; i < m_iVQuantity; i++)
    {
        if ( VertexInfluenced(i,fTime) )
        {
            Vector3<Real> kLocalDiff = m_akMean[i] - m_akVertex[i];
            Vector3<Real> kSurfaceNormal = kLocalDiff.Dot(m_akNormal[i]) *
                m_akNormal[i];
            Vector3<Real> kTangent = kLocalDiff - kSurfaceNormal;

            Real fTWeight = GetTangentWeight(i,fTime);
            Real fNWeight = GetNormalWeight(i,fTime);
            m_akVertex[i] += fTWeight*kTangent + fNWeight*m_akNormal[i];
        }
    }
}
//----------------------------------------------------------------------------
template <class Real>
bool MeshSmoother<Real>::VertexInfluenced (int, Real)
{
    return true;
}
//----------------------------------------------------------------------------
template <class Real>
Real MeshSmoother<Real>::GetTangentWeight (int, Real)
{
    return (Real)0.5;
}
//----------------------------------------------------------------------------
template <class Real>
Real MeshSmoother<Real>::GetNormalWeight (int, Real)
{
    return (Real)0.0;
}
//----------------------------------------------------------------------------
template <class Real>
int MeshSmoother<Real>::GetVQuantity () const
{
    return m_iVQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshSmoother<Real>::GetVertices () const
{
    return m_akVertex;
}
//----------------------------------------------------------------------------
template <class Real>
int MeshSmoother<Real>::GetTQuantity () const
{
    return m_iTQuantity;
}
//----------------------------------------------------------------------------
template <class Real>
const int* MeshSmoother<Real>::GetConnect () const
{
    return m_aiConnect;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshSmoother<Real>::GetNormals () const
{
    return m_akNormal;
}
//----------------------------------------------------------------------------
template <class Real>
const Vector3<Real>* MeshSmoother<Real>::GetMeans () const
{
    return m_akMean;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// explicit instantiation
//----------------------------------------------------------------------------
namespace Wml
{
template class WML_ITEM MeshSmoother<float>;
template class WML_ITEM MeshSmoother<double>;
}
//----------------------------------------------------------------------------
