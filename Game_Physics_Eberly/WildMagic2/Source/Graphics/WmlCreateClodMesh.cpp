// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCreateClodMesh.h"
using namespace Wml;

#include <fstream>
#include <stack>
using namespace std;

//----------------------------------------------------------------------------
CreateClodMesh::CreateClodMesh (int iVQuantity, Vector3f* akVertex,
    Vector3f* akNormal, ColorRGB* akColor, Vector2f* akTexture,
    int iTQuantity, int* aiConnect, int& riCQuantity,
    CollapseRecord*& rakCRecord)
{
    // Hang onto these to avoid having to pass them through member function
    // calls.
    m_iVQuantity = iVQuantity;
    m_akVertex = akVertex;
    m_akNormal = akNormal;
    m_akColor = akColor;
    m_akTexture = akTexture;
    m_iTQuantity = iTQuantity;
    m_aiConnect = aiConnect;

    // for reordering vertices and triangles
    m_iVCurrent = m_iVQuantity-1;
    m_iTCurrent = m_iTQuantity-1;
    m_aiVOrdered = new int[m_iVQuantity];
    m_aiVPermute = new int[m_iVQuantity];
    m_aiNewConnect = new int[3*m_iTQuantity];

    // Insert the triangles into the mesh.  The triangle indices are attached
    // as extra data.
    m_bCollapsing = false;
    for (int i = 0; i < m_iTQuantity; i++)
    {
        int iV0 = aiConnect[3*i];
        int iV1 = aiConnect[3*i+1];
        int iV2 = aiConnect[3*i+2];
        assert( iV0 != iV1 && iV0 != iV2 && iV1 != iV2 );
        Triangle kT(aiConnect[3*i],aiConnect[3*i+1],aiConnect[3*i+2]);
        InsertTriangle(kT);
        *(int*)GetData(kT) = i;
    }

    assert( (int)(m_kVMap.size()) == m_iVQuantity );
    assert( (int)(m_kTMap.size()) == m_iTQuantity );

    InitializeHeap();

    m_bCollapsing = true;
    while ( m_iHQuantity > 0 )
    {
        if ( m_apkHeap[0]->m_fMetric == FLT_MAX )
        {
            // all remaining heap elements have infinite weight
            FlushVertices();
            FlushTriangles();
            break;
        }

        DoCollapse();

        assert( (int)(m_kVMap.size()) == m_iVCurrent+1 );
        assert( (int)(m_kTMap.size()) == m_iTCurrent+1 );
    }
    m_bCollapsing = false;

    // Permute the vertices and triangle connectivity so that the last
    // vertex/triangle in the array is the first vertex/triangle to be
    // removed.
    Reorder();

    // The collapse records store the incremental changes that are used for
    // dynamic LOD changes in the caller of this constructor.
    ComputeRecords(riCQuantity,rakCRecord);
}
//----------------------------------------------------------------------------
CreateClodMesh::~CreateClodMesh ()
{
    RemoveAllTriangles();
    delete[] m_apkHeap;
    delete[] m_aiVOrdered;
    delete[] m_aiVPermute;
    delete[] m_aiNewConnect;
}
//----------------------------------------------------------------------------
void CreateClodMesh::DoCollapse ()
{
    // Define a 2-edge to be an edge that has exactly two triangles sharing
    // it.  An edge is collapsible if it is a 2-edge and has at least one end
    // point whose sharing edges are all 2-edges.  In this case, such an end
    // point will be the 'throw' vertex.  This keeps the boundary and junction
    // edges from changing geometry and helps preserve the shape of the mesh.
    // The topology is always guaranteed not to change.

    // When this function is called, the metric has already been calculated
    // and is finite (so exactly two triangles must be sharing this edge).
    assert( m_apkHeap[0]->m_fMetric < FLT_MAX );
    Edge kEdge = m_apkHeap[0]->m_kEdge;

    // test end points to see if either has only 2-edges sharing it
    int i;
    for (i = 0; i < 2; i++)
    {
        const SmallSet<Edge>* pkESet = GetEdges(kEdge.m_aiV[i]);
        int j;
        for (j = 0; j < pkESet->GetSize(); j++)
        {
            MECIter pkEM = m_kEMap.find((*pkESet)[j]);
            assert( pkEM != m_kEMap.end() );
            if ( pkEM->second.m_kTSet.GetSize() != 2 )
                break;
        }

        if ( j == pkESet->GetSize() )
        {
            // all edges sharing this end point are 2-edges
            break;
        }
    }

    if ( i < 2 )
    {
        int iVThrow = kEdge.m_aiV[i];
        int iVKeep = kEdge.m_aiV[1-i];
        if ( !CollapseCausesFolding(iVKeep,iVThrow) )
        {
            Remove();
            CollapseEdge(iVKeep,iVThrow);
            return;
        }
    }

    // edge not collapsible, assign it infinite weight and update the heap
    Update(0,FLT_MAX);
}
//----------------------------------------------------------------------------
bool CreateClodMesh::CollapseCausesFolding (int iVKeep, int iVThrow)
{
    MVIter pkVT = m_kVMap.find(iVThrow);
    assert( pkVT != m_kVMap.end() );

    Edge kCollapse(iVKeep,iVThrow);
    for (int j = 0; j < pkVT->second.m_kTSet.GetSize(); j++)
    {
        Triangle kT = pkVT->second.m_kTSet[j];
        if ( kCollapse == Edge(kT.m_aiV[0],kT.m_aiV[1])
        ||   kCollapse == Edge(kT.m_aiV[1],kT.m_aiV[2])
        ||   kCollapse == Edge(kT.m_aiV[2],kT.m_aiV[0]) )
        {
            // This triangle would be removed in a collapse, so it does not
            // contribute to any folding.
            continue;
        }

        for (int i = 0; i < 3; i++)
        {
            if ( kT.m_aiV[i] == iVThrow )
            {
                // Test if potential replacement triangle (either ordering)
                // is in the mesh.
                int iV0 = iVKeep;
                int iV1 = kT.m_aiV[(i+1)%3];
                int iV2 = kT.m_aiV[(i+2)%3];

                if ( m_kTMap.find(Triangle(iV0,iV1,iV2)) != m_kTMap.end()
                ||   m_kTMap.find(Triangle(iV0,iV2,iV1)) != m_kTMap.end() )
                {
                    return true;
                }
            }
        }
    }

    return false;
}
//----------------------------------------------------------------------------
float CreateClodMesh::GetMetric (MEIter pkE)
{
    const float fLengthWeight = 10.0f;
    const float fAngleWeight = 1.0f;

    // Compute the metric for the edge.  Only manifold edges (exactly two
    // triangles sharing the edge) are allowed to collapse.
    if ( pkE->second.m_kTSet.GetSize() == 2 )
    {
        // length contribution
        Vector3f& rkEnd0 = m_akVertex[pkE->first.m_aiV[0]];
        Vector3f& rkEnd1 = m_akVertex[pkE->first.m_aiV[1]];
        Vector3f kDiff = rkEnd1 - rkEnd0;
        float fMetric = fLengthWeight*kDiff.Length();

        // angle/area contribution
        Triangle kT = pkE->second.m_kTSet[0];
        Vector3f kV0 = m_akVertex[kT.m_aiV[0]];
        Vector3f kV1 = m_akVertex[kT.m_aiV[1]];
        Vector3f kV2 = m_akVertex[kT.m_aiV[2]];
        Vector3f kE0 = kV1 - kV0;
        Vector3f kE1 = kV2 - kV0;
        Vector3f kN0 = kE0.Cross(kE1);

        kT = pkE->second.m_kTSet[1];
        kV0 = m_akVertex[kT.m_aiV[0]];
        kV1 = m_akVertex[kT.m_aiV[1]];
        kV2 = m_akVertex[kT.m_aiV[2]];
        kE0 = kV1 - kV0;
        kE1 = kV2 - kV0;
        Vector3f kN1 = kE0.Cross(kE1);

        Vector3f kCross = kN0.Cross(kN1);
        fMetric += fAngleWeight*kCross.Length();

        return fMetric;
    }

    // Boundary edges (one triangle containing edge) and junction edges
    // (3 or more triangles sharing edge) are not allowed to collapse.
    return FLT_MAX;
}
//----------------------------------------------------------------------------
void CreateClodMesh::RemoveTriangle (const Triangle& rkT)
{
    // If the triangle is an original one, reorder the connectivity array so
    // that the triangle occurs at the end.
    int iTIndex = *(int*)GetData(rkT);
    if ( iTIndex >= 0 )
    {
        assert( m_iTCurrent >= 0 );
        m_aiNewConnect[3*m_iTCurrent] = m_aiConnect[3*iTIndex];
        m_aiNewConnect[3*m_iTCurrent+1] = m_aiConnect[3*iTIndex+1];
        m_aiNewConnect[3*m_iTCurrent+2] = m_aiConnect[3*iTIndex+2];
        m_iTCurrent--;
    }

    VETMesh::RemoveTriangle(rkT);
}
//----------------------------------------------------------------------------
void CreateClodMesh::ModifyTriangle (Triangle& rkT, int iVKeep, int iVThrow)
{
#ifdef _DEBUG
    int iTStart = (int)m_kTMap.size();
#endif

    // Get the index of the pre-modified triangle, then remove the triangle
    // from the mesh.
    int iTIndex = *(int*)GetData(rkT);
    VETMesh::RemoveTriangle(rkT);

    // replace 'throw' by 'keep'
    for (int i = 0; i < 3; i++)
    {
        if ( rkT.m_aiV[i] == iVThrow )
        {
            rkT.m_aiV[i] = iVKeep;
            break;
        }
    }

    // Indices on modified triangles are the same as the indices on the
    // pre-modified triangles.
    InsertTriangle(rkT);
    *(int*)GetData(rkT) = iTIndex;

#ifdef _DEBUG
    int iTFinal = (int)m_kTMap.size();
    assert( iTFinal == iTStart );
#endif
}
//----------------------------------------------------------------------------
void CreateClodMesh::CollapseEdge (int iVKeep, int iVThrow)
{
#ifdef _DEBUG
    int iVStart = (int)m_kVMap.size();
    int iTStart = (int)m_kTMap.size();
    assert( iVStart > 0 && iTStart > 0 );
#endif

    // find the edge to collapse
    Edge kCollapse(iVKeep,iVThrow);
    MEIter pkEM = m_kEMap.find(kCollapse);
    assert ( pkEM != m_kEMap.end() );

    // keep track of vertices that are deleted in the collapse
    m_kVDelete.clear();

    // Remove the collapse-edge-shared triangles.  Using a copy of the
    // triangle set from the collapse edge is required since removal of the
    // last triangle sharing the collapse edge will remove that edge from
    // the edge map, thereby invalidating any iterator that points to data
    // in the collapse edge.
    SmallSet<Triangle> kTSet = pkEM->second.m_kTSet;
    int iTDeletions = kTSet.GetSize();
    assert( iTDeletions > 0 );
    int j;
    for (j = 0; j < kTSet.GetSize(); j++)
        RemoveTriangle(kTSet[j]);

    // Replace 'throw' vertices by 'keep' vertices in the remaining triangles
    // at the 'throw' vertex.  The old triangles are removed and the modified
    // triangles are inserted.
    Triangle kT;
    MVIter pkVM = m_kVMap.find(iVThrow);
    if ( pkVM != m_kVMap.end() )
    {
        kTSet = pkVM->second.m_kTSet;
        for (j = 0; j < kTSet.GetSize(); j++)
        {
            // The explicit typecast is needed for version 2.96 of g++ that
            // ships with Red Hat Linux 7.0.  The compiler incorrectly
            // complains that *pkTS is 'const Wml::VETMesh::Triangle'
            // when in fact it is 'Wml::VETMesh::Triangle'.
            //
            // TO DO.  I removed the typecast.  Check g++ again.
            kT = kTSet[j];
            ModifyTriangle(kT,iVKeep,iVThrow);
        }
    }

    // The set of potentially modified edges consists of all those edges that
    // are shared by the triangles containing the 'keep' vertex.  Modify these
    // metrics and update the heap.
    set<Edge> kModified;
    const SmallSet<Triangle>* pkTSet = GetTriangles(iVKeep);
    if ( pkTSet )
    {
        kTSet = *pkTSet;
        for (j = 0; j < kTSet.GetSize(); j++)
        {
            kT = kTSet[j];
            kModified.insert(Edge(kT.m_aiV[0],kT.m_aiV[1]));
            kModified.insert(Edge(kT.m_aiV[1],kT.m_aiV[2]));
            kModified.insert(Edge(kT.m_aiV[2],kT.m_aiV[0]));
        }

        set<Edge>::iterator pkES;
        for (pkES = kModified.begin(); pkES != kModified.end(); pkES++)
        {
            MEIter pkEM = m_kEMap.find(*pkES);
            HeapRecord* pkRecord = (HeapRecord*)pkEM->second.m_pvData;
            float fMetric = GetMetric(pkEM);
            Update(pkRecord->m_iHIndex,fMetric);
        }
    }

#ifdef _DEBUG
    int iVFinal = (int)m_kVMap.size();
    int iVDiff = iVStart - iVFinal;
    int iTFinal = (int)m_kTMap.size();
    int iTDiff = iTStart - iTFinal;
    assert( iVDiff == (int)(m_kVDelete.size()) && iTDiff == iTDeletions );
#endif

    // save vertex reordering information
    set<int>::iterator pkVS;
    for (pkVS = m_kVDelete.begin(); pkVS != m_kVDelete.end(); pkVS++)
    {
        assert( 0 <= m_iVCurrent && m_iVCurrent < m_iVQuantity );

        int iV = *pkVS;
        assert( 0 <= iV && iV < m_iVQuantity );

        m_aiVOrdered[m_iVCurrent] = iV;
        m_aiVPermute[iV] = m_iVCurrent;
        m_iVCurrent--;
    }

    // Save the collapse information for use in constructing the final
    // collapse records for the caller of the constructor of this class.
    CollapseRecord kCR(iVKeep,iVThrow,(int)m_kVDelete.size(),iTDeletions);
    m_kEDelete.push_back(kCR);
}
//----------------------------------------------------------------------------
void CreateClodMesh::FlushVertices ()
{
    for (MVCIter pkV = m_kVMap.begin(); pkV != m_kVMap.end(); pkV++)
    {
        assert( 0 <= m_iVCurrent && m_iVCurrent < m_iVQuantity );

        int iV = pkV->first;
        assert( 0 <= iV && iV < m_iVQuantity );

        m_aiVOrdered[m_iVCurrent] = iV;
        m_aiVPermute[iV] = m_iVCurrent;
        m_iVCurrent--;
    }

    assert( m_iVCurrent == -1 );
}
//----------------------------------------------------------------------------
void CreateClodMesh::FlushTriangles ()
{
    for (MTCIter pkT = m_kTMap.begin(); pkT != m_kTMap.end(); pkT++)
    {
        assert( pkT->second.m_pvData != NULL );
        int iTIndex = *(int*)(pkT->second.m_pvData);
        if ( iTIndex >= 0 )
        {
            assert( m_iTCurrent >= 0 );
            m_aiNewConnect[3*m_iTCurrent] = m_aiConnect[3*iTIndex];
            m_aiNewConnect[3*m_iTCurrent+1] = m_aiConnect[3*iTIndex+1];
            m_aiNewConnect[3*m_iTCurrent+2] = m_aiConnect[3*iTIndex+2];
            m_iTCurrent--;
        }
    }

    assert( m_iTCurrent == -1 );
}
//----------------------------------------------------------------------------
void CreateClodMesh::Reorder ()
{
    // permute the vertices and copy to the original array
    Vector3f* akNewVertex = new Vector3f[m_iVQuantity];
    int i;
    for (i = 0; i < m_iVQuantity; i++)
        akNewVertex[i] = m_akVertex[m_aiVOrdered[i]];
    memcpy(m_akVertex,akNewVertex,m_iVQuantity*sizeof(Vector3f));
    delete[] akNewVertex;

    // permute the normal vectors (if any)
    if ( m_akNormal )
    {
        Vector3f* akNewNormal = new Vector3f[m_iVQuantity];
        for (i = 0; i < m_iVQuantity; i++)
            akNewNormal[i] = m_akNormal[m_aiVOrdered[i]];
        memcpy(m_akNormal,akNewNormal,m_iVQuantity*sizeof(Vector3f));
        delete[] akNewNormal;
    }

    // permute the colors (if any)
    if ( m_akColor )
    {
        ColorRGB* akNewColor = new ColorRGB[m_iVQuantity];
        for (i = 0; i < m_iVQuantity; i++)
            akNewColor[i] = m_akColor[m_aiVOrdered[i]];
        memcpy(m_akColor,akNewColor,m_iVQuantity*sizeof(ColorRGB));
        delete[] akNewColor;
    }

    // permute the texture coordinates (if any)
    if ( m_akTexture )
    {
        Vector2f* akNewTexture = new Vector2f[m_iVQuantity];
        for (i = 0; i < m_iVQuantity; i++)
            akNewTexture[i] = m_akTexture[m_aiVOrdered[i]];
        memcpy(m_akTexture,akNewTexture,m_iVQuantity*sizeof(Vector2f));
        delete[] akNewTexture;
    }

    // permute the connectivity array and copy to the original array
    for (i = 0; i < 3*m_iTQuantity; i++)
        m_aiConnect[i] = m_aiVPermute[m_aiNewConnect[i]];

    // permute the keep/throw pairs
    for (i = 0; i < (int)m_kEDelete.size(); i++)
    {
        CollapseRecord& rkCR = m_kEDelete[i];
        rkCR.m_iVKeep = m_aiVPermute[rkCR.m_iVKeep];
        rkCR.m_iVThrow = m_aiVPermute[rkCR.m_iVThrow];
    }
}
//----------------------------------------------------------------------------
void CreateClodMesh::ComputeRecords (int& riCQuantity,
    CollapseRecord*& rakCRecord)
{
    // build the collapse records for the caller
    riCQuantity = (int)m_kEDelete.size() + 1;
    rakCRecord = new CollapseRecord[riCQuantity];

    // initial record only stores the initial vertex and triangle quantities
    rakCRecord[0].m_iVQuantity = m_iVQuantity;
    rakCRecord[0].m_iTQuantity = m_iTQuantity;

    // construct the replacement arrays
    int iVQuantity = m_iVQuantity, iTQuantity = m_iTQuantity;
    int iR, i;
    for (iR = 0; iR < (int)m_kEDelete.size(); iR++)
    {
        CollapseRecord& rkERecord = m_kEDelete[iR];
        CollapseRecord& rkRecord = rakCRecord[iR+1];

        iVQuantity -= rkERecord.m_iVQuantity;
        iTQuantity -= rkERecord.m_iTQuantity;

        rkRecord.m_iVKeep = rkERecord.m_iVKeep;
        rkRecord.m_iVThrow = rkERecord.m_iVThrow;
        rkRecord.m_iVQuantity = iVQuantity;
        rkRecord.m_iTQuantity = iTQuantity;
        rkRecord.m_iIQuantity = 0;

        if ( iTQuantity > 0 )
        {
            int iIMax = 3*iTQuantity;
            int* aiIndex = new int[iIMax];
            for (i = 0; i < iIMax; i++)
            {
                if ( m_aiConnect[i] == rkRecord.m_iVThrow )
                {
                    m_aiConnect[i] = rkRecord.m_iVKeep;
                    aiIndex[rkRecord.m_iIQuantity++] = i;
                }
            }

            if ( rkRecord.m_iIQuantity > 0 )
            {
                rkRecord.m_aiIndex = new int[rkRecord.m_iIQuantity];
                memcpy(rkRecord.m_aiIndex,aiIndex,
                    rkRecord.m_iIQuantity*sizeof(int));
            }

            delete[] aiIndex;
        }
        else
        {
            rkRecord.m_aiIndex = NULL;
        }
    }

    // expand mesh back to original
    for (iR = riCQuantity - 1; iR > 0; iR--)
    {
        // restore indices in connectivity array
        CollapseRecord& rkRecord = rakCRecord[iR];
        for (i = 0; i < rkRecord.m_iIQuantity; i++)
        {
            int iC = rkRecord.m_aiIndex[i];
            assert( m_aiConnect[iC] == rkRecord.m_iVKeep );
            m_aiConnect[iC] = rkRecord.m_iVThrow;
        }
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// heap operations
//----------------------------------------------------------------------------
void CreateClodMesh::InitializeHeap ()
{
    // It is possible that during an edge collapse, the number of *temporary*
    // edges is larger than the original number of edges in the mesh.  To
    // make sure there is enough heap space, allocate two times the number of
    // original edges.
    m_iHQuantity = (int)m_kEMap.size();
    m_apkHeap = new HeapRecord*[2*m_iHQuantity];

    int iHIndex = 0;
    for (MEIter pkE = m_kEMap.begin(); pkE != m_kEMap.end(); pkE++, iHIndex++)
    {
        m_apkHeap[iHIndex] = (HeapRecord*)pkE->second.m_pvData;
        m_apkHeap[iHIndex]->m_kEdge = pkE->first;
        m_apkHeap[iHIndex]->m_iHIndex = iHIndex;
        m_apkHeap[iHIndex]->m_fMetric = GetMetric(pkE);
    }

    Sort();
}
//----------------------------------------------------------------------------
void CreateClodMesh::Sort ()
{
    int iLast = m_iHQuantity - 1;
    for (int iLeft = iLast/2; iLeft >= 0; iLeft--)
    {
        HeapRecord* pkRecord = m_apkHeap[iLeft];
        int iPa = iLeft, iCh = 2*iLeft + 1;
        while ( iCh <= iLast )
        {
            if ( iCh < iLast )
            {
                int iChP = iCh+1;
                if ( m_apkHeap[iCh]->m_fMetric > m_apkHeap[iChP]->m_fMetric )
                    iCh = iChP;
            }

            if ( m_apkHeap[iCh]->m_fMetric >= pkRecord->m_fMetric )
                break;

            m_apkHeap[iCh]->m_iHIndex = iPa;
            m_apkHeap[iPa] = m_apkHeap[iCh];
            iPa = iCh;
            iCh = 2*iCh + 1;
        }

        pkRecord->m_iHIndex = iPa;
        m_apkHeap[iPa] = pkRecord;
    }
}
//----------------------------------------------------------------------------
void CreateClodMesh::Add (float fMetric)
{
    // Under normal heap operations, you would have to make sure that the
    // heap storage grows if necessary.  Increased storage demand will not
    // happen in this application.  The creation of the heap record itself is
    // done in OnEdgeCreate.
    m_iHQuantity++;

    int iCh = m_iHQuantity - 1;
    HeapRecord* pkRecord = m_apkHeap[iCh];
    pkRecord->m_fMetric = fMetric;
    while ( iCh > 0 )
    {
        int iPa = (iCh-1)/2;
        if ( m_apkHeap[iPa]->m_fMetric <= fMetric )
            break;

        m_apkHeap[iPa]->m_iHIndex = iCh;
        m_apkHeap[iCh] = m_apkHeap[iPa];
        pkRecord->m_iHIndex = iPa;
        pkRecord->m_fMetric = fMetric;
        m_apkHeap[iPa] = pkRecord;
        iCh = iPa;
    }

    m_apkHeap[iCh]->m_fMetric = fMetric;
}
//----------------------------------------------------------------------------
void CreateClodMesh::Remove ()
{
    HeapRecord* pkRoot = m_apkHeap[0];

    int iLast = m_iHQuantity - 1;
    HeapRecord* pkRecord = m_apkHeap[iLast];
    int iPa = 0, iCh = 1;
    while ( iCh <= iLast )
    {
        if ( iCh < iLast )
        {
            int iChP = iCh+1;
            if ( m_apkHeap[iCh]->m_fMetric > m_apkHeap[iChP]->m_fMetric )
                iCh = iChP;
        }

        if ( m_apkHeap[iCh]->m_fMetric >= pkRecord->m_fMetric )
            break;

        m_apkHeap[iCh]->m_iHIndex = iPa;
        m_apkHeap[iPa] = m_apkHeap[iCh];
        iPa = iCh;
        iCh = 2*iCh + 1;
    }

    pkRecord->m_iHIndex = iPa;
    m_apkHeap[iPa] = pkRecord;
    m_iHQuantity--;

    // To notify OnEdgeDestroy that this edge was already removed from the
    // heap, but the object must be deleted by that callback.
    pkRoot->m_iHIndex = -1;
}
//----------------------------------------------------------------------------
void CreateClodMesh::Update (int iHIndex, float fMetric)
{
    HeapRecord* pkRecord = m_apkHeap[iHIndex];
    int iPa, iCh, iChP, iMaxCh;

    if ( fMetric > pkRecord->m_fMetric )
    {
        pkRecord->m_fMetric = fMetric;

        // new weight larger than old, propagate it towards the leaves
        iPa = iHIndex;
        iCh = 2*iPa+1;
        while ( iCh < m_iHQuantity )
        {
            // at least one child exists
            if ( iCh < m_iHQuantity-1 )
            {
                // two children exist
                iChP = iCh+1;
                if ( m_apkHeap[iCh]->m_fMetric <= m_apkHeap[iChP]->m_fMetric )
                    iMaxCh = iCh;
                else
                    iMaxCh = iChP;
            }
            else
            {
                // one child exists
                iMaxCh = iCh;
            }

            if ( m_apkHeap[iMaxCh]->m_fMetric >= fMetric )
                break;

            m_apkHeap[iMaxCh]->m_iHIndex = iPa;
            m_apkHeap[iPa] = m_apkHeap[iMaxCh];
            pkRecord->m_iHIndex = iMaxCh;
            m_apkHeap[iMaxCh] = pkRecord;
            iPa = iMaxCh;
            iCh = 2*iPa+1;
        }
    }
    else if ( fMetric < pkRecord->m_fMetric )
    {
        pkRecord->m_fMetric = fMetric;

        // new weight smaller than old, propagate it towards the root
        iCh = iHIndex;
        while ( iCh > 0 )
        {
            // a parent exists
            iPa = (iCh-1)/2;

            if ( m_apkHeap[iPa]->m_fMetric <= fMetric )
                break;

            m_apkHeap[iPa]->m_iHIndex = iCh;
            m_apkHeap[iCh] = m_apkHeap[iPa];
            pkRecord->m_iHIndex = iPa;
            pkRecord->m_fMetric = fMetric;
            m_apkHeap[iPa] = pkRecord;
            iCh = iPa;
        }
    }
}
//----------------------------------------------------------------------------
bool CreateClodMesh::IsValidHeap (int iStart, int iFinal)
{
    for (int iC = iStart; iC <= iFinal; iC++)
    {
        int iP = (iC-1)/2;
        if ( iP > iStart )
        {
            if ( m_apkHeap[iP]->m_fMetric > m_apkHeap[iC]->m_fMetric )
                return false;

            if ( m_apkHeap[iP]->m_iHIndex != iP )
                return false;
        }
    }

    return true;
}
//----------------------------------------------------------------------------
bool CreateClodMesh::IsValidHeap ()
{
    return IsValidHeap(0,m_iHQuantity-1);
}
//----------------------------------------------------------------------------
void CreateClodMesh::PrintHeap (const char* acFilename)
{
    ofstream kOStr(acFilename);
    for (int i = 0; i < m_iHQuantity; i++)
    {
        HeapRecord* pkRecord = m_apkHeap[i];
        kOStr << pkRecord->m_iHIndex << "= <" << pkRecord->m_kEdge.m_aiV[0]
              << ',' << pkRecord->m_kEdge.m_aiV[1] << "> "
              << pkRecord->m_fMetric << endl;

        int iValue = i+2;
        int iBits = 0;
        while ( iValue != 0 )
        {
            if ( iValue & 1 )
                iBits++;
            iValue >>= 1;
        }
        if ( iBits == 1 )
            kOStr << endl;
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// mesh insert/remove callbacks
//----------------------------------------------------------------------------
void CreateClodMesh::OnVertexInsert (int iV, bool bCreate, void*&)
{
    // It is possible that a 'keep' vertex was removed because the triangles
    // sharing the collapse edge were removed first, but then the insertion
    // of a modified triangle reinserts the 'keep' vertex.
    if ( bCreate && m_bCollapsing )
        m_kVDelete.erase(iV);
}
//----------------------------------------------------------------------------
void CreateClodMesh::OnVertexRemove (int iV, bool bDestroy, void*)
{
    // Keep track of vertices removed during the edge collapse.
    if ( bDestroy && m_bCollapsing )
        m_kVDelete.insert(iV);
}
//----------------------------------------------------------------------------
void CreateClodMesh::OnEdgeInsert (const Edge& rkE, bool bCreate,
    void*& rpvData)
{
    if ( bCreate )
    {
        rpvData = new HeapRecord;
        if ( m_bCollapsing )
        {
            m_apkHeap[m_iHQuantity] = (HeapRecord*)rpvData;
            m_apkHeap[m_iHQuantity]->m_kEdge = rkE;
            m_apkHeap[m_iHQuantity]->m_iHIndex = m_iHQuantity;
            Add(GetMetric(m_kEMap.find(rkE)));
        }
    }
    else
    {
        if ( m_bCollapsing )
        {
            HeapRecord* pkRecord = (HeapRecord*)rpvData;
            assert( pkRecord->m_kEdge == rkE );
            if ( pkRecord->m_iHIndex >= 0 )
            {
                Update(pkRecord->m_iHIndex,GetMetric(m_kEMap.find(rkE)));
            }
            else
            {
                assert( pkRecord->m_iHIndex == -1 );
                pkRecord->m_iHIndex = m_iHQuantity;
                Add(GetMetric(m_kEMap.find(rkE)));
            }
        }
    }
}
//----------------------------------------------------------------------------
void CreateClodMesh::OnEdgeRemove (const Edge&, bool bDestroy, void* pvData)
{
    // Remove the edge from the heap.  The metric of the edge is set to
    // -INFINITY so that it has the minimum value of all edges.  The update
    // call bubbles the edge to the root of the heap.  The edge is then
    // removed from the root.

    if ( bDestroy )
    {
        HeapRecord* pkRecord = (HeapRecord*)pvData;
        if ( pkRecord->m_iHIndex >= 0 )
        {
            Update(pkRecord->m_iHIndex,-FLT_MAX);
            Remove();
        }
        delete pkRecord;
    }
}
//----------------------------------------------------------------------------
void CreateClodMesh::OnTriangleInsert (const Triangle&, bool bCreate,
    void*& rpvData)
{
    if ( bCreate )
        rpvData = new int(-1);
}
//----------------------------------------------------------------------------
void CreateClodMesh::OnTriangleRemove (const Triangle&, bool bDestroy,
    void* pvData)
{
    if ( bDestroy )
        delete (int*)pvData;
}
//----------------------------------------------------------------------------
