// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

// #define _DEBUG_TEST

#include "WmlBoundingVolumeTree.h"
using namespace Wml;

//----------------------------------------------------------------------------
BoundingVolumeTree::BoundingVolumeTree (BoundingVolume::Type eType,
    int iVertexCount, const Vector3f* akVertex, int iTriangleCount,
    const int* aiConnect, int iMaxTrisPerLeaf, bool bStoreInteriorTris)
{
    assert( eType != BoundingVolume::BV_QUANTITY );
    m_pkWorldBound = NULL;

    // Centroids of triangles are used for splitting a mesh.  The centroids
    // are projected onto a splitting axis and sorted.  The split is based
    // on the median of the projections.
    Vector3f* akCentroid = new Vector3f[iTriangleCount];
    const float fOneThird = 1.0f/3.0f;
    int i = 0, iT;
    for (iT = 0; iT < iTriangleCount; iT++)
    {
        int i0 = aiConnect[i++];
        int i1 = aiConnect[i++];
        int i2 = aiConnect[i++];
        akCentroid[iT] = fOneThird*(akVertex[i0]+akVertex[i1]+akVertex[i2]);
    }

    // Initialize binary-tree arrays for storing triangle indices.  These
    // are used to store the indices when the mesh is split.
    int* aiISplit = new int[iTriangleCount];
    int* aiOSplit = new int[iTriangleCount];
    for (iT = 0; iT < iTriangleCount; iT++)
        aiISplit[iT] = iT;

    CreateTree(eType,iVertexCount,akVertex,iTriangleCount,aiConnect,
        iMaxTrisPerLeaf,bStoreInteriorTris,akCentroid,0,iTriangleCount-1,
        aiISplit,aiOSplit);

    delete[] akCentroid;
    delete[] aiISplit;
    delete[] aiOSplit;

#ifdef _DEBUG_TEST
    if ( bStoreInteriorTris )
    {
        float fEpsilon = 1e-05f;
        bool bSuccess = ContainsLeafData(akVertex,aiConnect,fEpsilon);
        assert( bSuccess );
    }
#endif
}
//----------------------------------------------------------------------------
BoundingVolumeTree::BoundingVolumeTree ()
{
    m_pkModelBound = NULL;
    m_pkWorldBound = NULL;
    m_pkLChild = NULL;
    m_pkRChild = NULL;
    m_iTriangleQuantity = 0;
    m_aiTriangle = NULL;
}
//----------------------------------------------------------------------------
BoundingVolumeTree::~BoundingVolumeTree ()
{
    delete m_pkModelBound;
    delete m_pkWorldBound;
    delete[] m_aiTriangle;
    delete m_pkLChild;
    delete m_pkRChild;
}
//----------------------------------------------------------------------------
void BoundingVolumeTree::CreateTree (BoundingVolume::Type eType,
    int iVertexCount, const Vector3f* akVertex, int iTriangleCount,
    const int* aiConnect, int iMaxTrisPerLeaf, bool bStoreInteriorTris,
    const Vector3f* akCentroid, int i0, int i1, int* aiISplit, int* aiOSplit)
{
    assert( i0 <= i1 );

    Vector3f kOrigin, kDirection;
    m_pkModelBound = BoundingVolume::Create(eType,iVertexCount,akVertex,
        aiConnect,i0,i1,aiISplit,kOrigin,kDirection);

    if ( i1 - i0 < iMaxTrisPerLeaf )
    {
        // leaf node
        m_iTriangleQuantity = i1 - i0 + 1;
        m_aiTriangle = new int[m_iTriangleQuantity];
        memcpy(m_aiTriangle,&aiISplit[i0],m_iTriangleQuantity*sizeof(int));

        m_pkLChild = NULL;
        m_pkRChild = NULL;
    }
    else
    {
        // interior node
        if ( bStoreInteriorTris )
        {
            m_iTriangleQuantity = i1 - i0 + 1;
            m_aiTriangle = new int[m_iTriangleQuantity];
            memcpy(m_aiTriangle,&aiISplit[i0],
                m_iTriangleQuantity*sizeof(int));
        }
        else
        {
            m_iTriangleQuantity = 0;
            m_aiTriangle = NULL;
        }

        int j0, j1;
        SplitTriangles(akCentroid,i0,i1,aiISplit,j0,j1,aiOSplit,kOrigin,
            kDirection);

        m_pkLChild = new BoundingVolumeTree;
        m_pkLChild->CreateTree(eType,iVertexCount,akVertex,iTriangleCount,
            aiConnect,iMaxTrisPerLeaf,bStoreInteriorTris,akCentroid,i0,j0,
            aiOSplit,aiISplit);

        m_pkRChild = new BoundingVolumeTree;
        m_pkRChild->CreateTree(eType,iVertexCount,akVertex,iTriangleCount,
            aiConnect,iMaxTrisPerLeaf,bStoreInteriorTris,akCentroid,j1,i1,
            aiOSplit,aiISplit);
    }
}
//----------------------------------------------------------------------------
int BoundingVolumeTree::Compare (const void* pvElement0,
    const void* pvElement1)
{
    const ProjectionInfo* pInfo0 = (const ProjectionInfo*) pvElement0;
    const ProjectionInfo* pInfo1 = (const ProjectionInfo*) pvElement1;

    if ( pInfo0->m_fProjection < pInfo1->m_fProjection )
        return -1;

    if ( pInfo0->m_fProjection > pInfo1->m_fProjection )
        return +1;

    return 0;
}
//----------------------------------------------------------------------------
void BoundingVolumeTree::SplitTriangles (const Vector3f* akCentroid,
    int i0, int i1, int* aiISplit, int& rj0, int& rj1, int* aiOSplit,
    const Vector3f& rkOrigin, const Vector3f& rkDirection)
{
    // project onto specified line
    int iQuantity = i1 - i0 + 1;
    ProjectionInfo* akInfo = new ProjectionInfo[iQuantity];
    int i, j;
    for (i = i0, j = 0; i <= i1; i++, j++)
    {
        int iTriangle = aiISplit[i];
        Vector3f kDiff = akCentroid[iTriangle] - rkOrigin;
        akInfo[j].m_iTriangle = iTriangle;
        akInfo[j].m_fProjection = rkDirection.Dot(kDiff);
    }

    // find median of projections by sorting
    qsort(akInfo,iQuantity,sizeof(ProjectionInfo),Compare);
    int iMedian = (iQuantity-1)/2;

    // partition the triangles by the median
    for (j = 0, rj0 = i0-1; j <= iMedian; j++)
        aiOSplit[++rj0] = akInfo[j].m_iTriangle;
    for (rj1 = i1+1; j < iQuantity; j++)
        aiOSplit[--rj1] = akInfo[j].m_iTriangle;

    delete[] akInfo;
}
//----------------------------------------------------------------------------
BoundingVolume* BoundingVolumeTree::GetWorldBound ()
{
    if ( !m_pkWorldBound )
    {
        m_pkWorldBound = BoundingVolume::Create(m_pkModelBound->GetType());
        m_pkWorldBound->Invalidate();
    }

    return m_pkWorldBound;
}
//----------------------------------------------------------------------------
void BoundingVolumeTree::InvalidateWorldBounds ()
{
    if ( m_pkWorldBound && m_pkWorldBound->IsValid() )
    {
        m_pkWorldBound->Invalidate();
        if ( m_pkLChild )
            m_pkLChild->InvalidateWorldBounds();
        if ( m_pkRChild )
            m_pkRChild->InvalidateWorldBounds();
    }
}
//----------------------------------------------------------------------------

#ifdef _DEBUG_TEST
//----------------------------------------------------------------------------
bool BoundingVolumeTree::ContainsLeafData (const Vector3f* akVertex,
    const int* aiConnect, float fEpsilon) const
{
    if ( m_pkLChild )
    {
        if ( !m_pkLChild->ContainsLeafData(akVertex,aiConnect,fEpsilon) )
            return false;
    }

    if ( m_pkRChild )
    {
        if ( !m_pkRChild->ContainsLeafData(akVertex,aiConnect,fEpsilon) )
            return false;
    }

    for (int iT = 0; iT < m_iTriangleQuantity; iT++)
    {
        int iIndex = 3*m_aiTriangle[iT];
        for (int i = 0; i < 3; i++)
        {
            Vector3f kPoint = akVertex[aiConnect[iIndex++]];
            if ( !m_pkModelBound->Contains(kPoint,fEpsilon) )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
#endif
