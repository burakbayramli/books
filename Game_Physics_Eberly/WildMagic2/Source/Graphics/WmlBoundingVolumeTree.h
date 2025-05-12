// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLBOUNDINGVOLUMETREE_H
#define WMLBOUNDINGVOLUMETREE_H

#include "WmlBoundingVolume.h"
#include "WmlVector3.h"

namespace Wml
{

class WML_ITEM BoundingVolumeTree
{
public:
    BoundingVolumeTree (BoundingVolume::Type eType, int iVertexCount,
        const Vector3f* akVertex, int iTriangleCount, const int* aiConnect,
        int iMaxTrisPerLeaf = 1, bool bStoreInteriorTris = false);

    ~BoundingVolumeTree ();

    // tree topology
    BoundingVolumeTree* GetLChild ();
    BoundingVolumeTree* GetRChild ();
    bool IsInteriorNode () const;
    bool IsLeafNode () const;

    // model space data
    const BoundingVolume* GetModelBound () const;
    int GetTriangleQuantity () const;
    int GetTriangle (int i) const;
    const int* GetTriangles () const;

    // world space data
    BoundingVolume* GetWorldBound ();
    void InvalidateWorldBounds ();

protected:
    // support for recursive construction of tree
    BoundingVolumeTree ();

    void CreateTree (BoundingVolume::Type eType, int iVertexCount,
        const Vector3f* akVertex, int iTriangleCount, const int* aiConnect,
        int iMaxTrisPerLeaf, bool bStoreInteriorTris,
        const Vector3f* akCentroid, int i0, int i1, int* aiISplit,
        int* aiOSplit);

    static void SplitTriangles (const Vector3f* akCentroid, int i0, int i1,
        int* aiISplit, int& rj0, int& rj1, int* aiOSplit,
        const Vector3f& rkOrigin, const Vector3f& rkDirection);


    // for quick-sort of centroid projections on axes
    class WML_ITEM ProjectionInfo
    {
    public:
        int m_iTriangle;
        float m_fProjection;
    };

    static int Compare (const void* pvElement0, const void* pvElement1);

    // bounds and child links
    BoundingVolume* m_pkModelBound;
    BoundingVolume* m_pkWorldBound;
    BoundingVolumeTree* m_pkLChild;
    BoundingVolumeTree* m_pkRChild;

    // If bStoreInteriorTris is set to 'false' in the constructor, the
    // interior nodes set the triangle quantity to zero and the array to null.
    // Leaf nodes set the quantity to the number of triangles at that node (1
    // if iMaxTrianglesPerLeaf was set to 1) and allocate an array of
    // triangle indices that are relative to the input mesh of the top level
    // constructor.
    //
    // If bStoreInteriorTris is set to 'true', the interior nodes also save
    // the triangle quantity and array of triangle indices for the mesh that
    // the node represents.
    int m_iTriangleQuantity;
    int* m_aiTriangle;

#ifdef _DEBUG_TEST
    // Checks to see if the vertices corresponding to the triangle mesh at
    // at each tree node are contained by the model space bounding volume.
    // The call is only made when _DEBUG_TEST has been defined *and* when
    // bStoreInteriorTris is set to 'true'.
    bool ContainsLeafData (const Vector3f* akVertex, const int* aiConnect,
        float fEpsilon) const;
#endif
};

#include "WmlBoundingVolumeTree.inl"

}

#endif
