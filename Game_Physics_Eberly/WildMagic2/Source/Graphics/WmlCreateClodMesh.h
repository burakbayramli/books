// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLCREATECLODMESH_H
#define WMLCREATECLODMESH_H

#include "WmlCollapseRecord.h"
#include "WmlVector3.h"
#include "WmlVETMesh.h"

namespace Wml
{

class WML_ITEM CreateClodMesh : public VETMesh
{
public:
    CreateClodMesh (int iVQuantity, Vector3f* akVertex, Vector3f* akNormal,
        ColorRGB* akColor, Vector2f* akTexture, int iTQuantity,
        int* aiTConnect, int& riCQuantity, CollapseRecord*& rakCRecord);

    ~CreateClodMesh ();

protected:
    class WML_ITEM HeapRecord
    {
    public:
        HeapRecord () : m_kEdge(-1,-1)
        {
            m_iHIndex = -1;
            m_fMetric = -1.0f;
        }

        Edge m_kEdge;
        int m_iHIndex;
        float m_fMetric;
    };

    virtual void OnVertexInsert (int iV, bool bCreate, void*& rpvData);
    virtual void OnVertexRemove (int iV, bool bDestroy, void* pvData);
    virtual void OnEdgeInsert (const Edge& rkE, bool bCreate, void*& rpvData);
    virtual void OnEdgeRemove (const Edge& rkE, bool bDestroy, void* pvData);
    virtual void OnTriangleInsert (const Triangle& rkT, bool bCreate,
        void*& rpvData);
    virtual void OnTriangleRemove (const Triangle& rkT, bool bDestroy,
        void* pvData);

    virtual void RemoveTriangle (const Triangle& rkT);
    void ModifyTriangle (Triangle& rkT, int iVKeep, int iVThrow);
    void CollapseEdge (int iVKeep, int iVThrow);
    void FlushVertices ();
    void FlushTriangles ();

    void DoCollapse ();
    bool CollapseCausesFolding (int iVKeep, int iVThrow);
    float GetMetric (MEIter pkE);
    void Reorder ();
    void ComputeRecords (int& riCQuantity, CollapseRecord*& rakCRecord);

    void InitializeHeap ();
    void Sort ();
    void Add (float fMetric);
    void Remove ();
    void Update (int iHIndex, float fMetric);
    bool IsValidHeap (int iStart, int iFinal);
    bool IsValidHeap ();
    void PrintHeap (const char* acFilename);

    // triangle mesh to be decimated
    int m_iVQuantity;
    Vector3f* m_akVertex;
    Vector3f* m_akNormal;
    ColorRGB* m_akColor;
    Vector2f* m_akTexture;
    int m_iTQuantity;
    int* m_aiConnect;

    int m_iHQuantity;
    HeapRecord** m_apkHeap;
    bool m_bCollapsing;

    // for reordering vertices and triangles
    std::set<int> m_kVDelete;
    int m_iVCurrent, m_iTCurrent;
    int* m_aiVOrdered;
    int* m_aiVPermute;
    int* m_aiNewConnect;
    std::vector<CollapseRecord> m_kEDelete;
};

}

#endif
