// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlETNonmanifoldMesh.h"
using namespace Wml;

#include <fstream>
#include <set>
using namespace std;

//----------------------------------------------------------------------------
ETNonmanifoldMesh::ETNonmanifoldMesh (ECreator oECreator, TCreator oTCreator)
{
    m_oECreator = ( oECreator ? oECreator : CreateEdge );
    m_oTCreator = ( oTCreator ? oTCreator : CreateTriangle );
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::~ETNonmanifoldMesh ()
{
    EMapIterator pkEIter;
    for (pkEIter = m_kEMap.begin(); pkEIter != m_kEMap.end(); pkEIter++)
        delete pkEIter->second;

    TMapIterator pkTIter;
    for (pkTIter = m_kTMap.begin(); pkTIter != m_kTMap.end(); pkTIter++)
        delete pkTIter->second;
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::EPtr ETNonmanifoldMesh::CreateEdge (int iV0, int iV1)
{
    return new Edge(iV0,iV1);
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::TPtr ETNonmanifoldMesh::CreateTriangle (int iV0, int iV1,
    int iV2)
{
    return new Triangle(iV0,iV1,iV2);
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::TPtr ETNonmanifoldMesh::InsertTriangle (int iV0, int iV1,
    int iV2)
{
    TriangleKey kTKey(iV0,iV1,iV2);
    TMapIterator pkTIter = m_kTMap.find(kTKey);
    if ( pkTIter != m_kTMap.end() )
    {
        // triangle already exists
        return NULL;
    }

    // add new triangle
    TPtr pkT = m_oTCreator(iV0,iV1,iV2);
    m_kTMap[kTKey] = pkT;

    // add edges to mesh
    for (int i0 = 2, i1 = 0; i1 < 3; i0 = i1++)
    {
        EdgeKey kEKey(pkT->V[i0],pkT->V[i1]);
        EPtr pkE;
        EMapIterator pkEIter = m_kEMap.find(kEKey);
        if ( pkEIter == m_kEMap.end() )
        {
            // first time edge encountered
            pkE = m_oECreator(pkT->V[i0],pkT->V[i1]);
            m_kEMap[kEKey] = pkE;
        }
        else
        {
            // edge previously encountered and created
            pkE = pkEIter->second;
        }

        // update edge and triangle
        pkE->T.InsertNoCheck(pkT);
        pkT->E[i0] = pkE;
    }

    return pkT;
}
//----------------------------------------------------------------------------
bool ETNonmanifoldMesh::RemoveTriangle (int iV0, int iV1, int iV2)
{
    TriangleKey kTKey(iV0,iV1,iV2);
    TMapIterator pkTIter = m_kTMap.find(kTKey);
    if ( pkTIter == m_kTMap.end() )
    {
        // triangle does not exist
        return false;
    }

    TPtr pkT = pkTIter->second;
    for (int i = 0; i < 3; i++)
    {
        // inform edges you are going away
        EPtr pkE = pkT->E[i];
        pkE->T.Remove(pkT);

        // remove edge if you had the last reference to it
        if ( pkE->T.GetSize() == 0 )
        {
            EdgeKey kEKey(pkE->V[0],pkE->V[1]);
            m_kEMap.erase(kEKey);
            delete pkE;
        }
    }

    m_kTMap.erase(kTKey);
    delete pkT;
    return true;
}
//----------------------------------------------------------------------------
bool ETNonmanifoldMesh::IsManifold () const
{
    EMapCIterator pkEIter = m_kEMap.begin();
    for (/**/; pkEIter != m_kEMap.end(); pkEIter++)
    {
        if ( pkEIter->second->T.GetSize() > 2 )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------
bool ETNonmanifoldMesh::IsClosed () const
{
    EMapCIterator pkEIter = m_kEMap.begin();
    for (/**/; pkEIter != m_kEMap.end(); pkEIter++)
    {
        if ( pkEIter->second->T.GetSize() != 2 )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------
bool ETNonmanifoldMesh::IsConnected () const
{
    // Perform a breadth-first search to locate the connected component
    // containing the first triangle in the triangle map.
    if ( m_kTMap.begin() == m_kTMap.end() )
    {
        // no triangles in the mesh, by default mesh is connected
        return true;
    }

    // start search at first triangle in mesh
    set<Triangle*> kComponent, kBoundary;
    kBoundary.insert(m_kTMap.begin()->second);

    while ( kBoundary.size() > 0 )
    {
        set<Triangle*> kExterior;

        // process boundary triangles
        set<Triangle*>::iterator pkTIter = kBoundary.begin();
        for (/**/; pkTIter != kBoundary.end(); pkTIter++)
        {
            // boundary triangle is adjacent to current connected component
            TPtr pkT = *pkTIter;
            kComponent.insert(pkT);

            // locate adjacent, exterior triangles for later processing
            for (int i = 0; i < 3; i++)
            {
                EPtr pkE = pkT->E[i];
                for (int j = 0; j < pkE->T.GetSize(); j++)
                {
                    Triangle* pkA = pkE->T[j];
                    if ( pkA != pkT
                    &&   kComponent.find(pkA) == kComponent.end()
                    &&   kBoundary.find(pkA) == kBoundary.end() )
                    {
                        kExterior.insert(pkA);
                    }
                }
            }
        }

        // exterior triangles are next in line to be processed
        kBoundary = kExterior;
    }

    return kComponent.size() == m_kTMap.size();
}
//----------------------------------------------------------------------------
void ETNonmanifoldMesh::RemoveComponent (int& riIQuantity, int* aiConnect)
{
    // Do a breadth-first search of the mesh to find connected components.
    // The input array is assumed to be large enough to hold the component.
    // (See the comments in ETNonmanifoldMesh.h for RemoveComponent.)
    riIQuantity = 0;

    if ( m_kTMap.size() == 0 )
        return;

    // find the connected component containing the first triangle in the mesh
    set<Triangle*> kVisited;
    kVisited.insert(m_kTMap.begin()->second);

    // traverse the connected component
    while ( !kVisited.empty() )
    {
        // start at the current triangle
        Triangle* pkT = *kVisited.begin();

        // add adjacent triangles to the set for recursive processing
        for (int i = 0; i < 3; i++)
        {
            EPtr pkE = pkT->E[i];
            for (int j = 0; j < pkE->T.GetSize(); j++)
            {
                Triangle* pkA = pkE->T[j];
                if ( pkA != pkT )
                    kVisited.insert(pkA);
            }
        }

        // add triangle to connectivity array
        aiConnect[riIQuantity++] = pkT->V[0];
        aiConnect[riIQuantity++] = pkT->V[1];
        aiConnect[riIQuantity++] = pkT->V[2];

        // remove the current triangle (visited, no longer needed)
        kVisited.erase(pkT);
        RemoveTriangle(pkT->V[0],pkT->V[1],pkT->V[2]);
    }
}
//----------------------------------------------------------------------------
void ETNonmanifoldMesh::Print (const char* acFilename)
{
    ofstream kOStr(acFilename);
    if ( !kOStr )
        return;

    int i, j;

    // assign unique indices to the edges
    map<Edge*,int> kEIndex;
    kEIndex[0] = 0;
    i = 1;
    EMapIterator pkEIter;
    for (pkEIter = m_kEMap.begin(); pkEIter != m_kEMap.end(); pkEIter++)
    {
        if ( pkEIter->second )
            kEIndex[pkEIter->second] = i++;
    }

    // assign unique indices to the triangles
    map<Triangle*,int> kTIndex;
    kTIndex[0] = 0;
    i = 1;
    TMapIterator pkTIter;
    for (pkTIter = m_kTMap.begin(); pkTIter != m_kTMap.end(); pkTIter++)
    {
        if ( pkTIter->second )
            kTIndex[pkTIter->second] = i++;
    }

    // print edges
    kOStr << "edge quantity = " << (int)m_kEMap.size() << endl;
    for (pkEIter = m_kEMap.begin(); pkEIter != m_kEMap.end(); pkEIter++)
    {
        Edge* pkE = pkEIter->second;
        kOStr << 'e' << kEIndex[pkE] << " <"
              << 'v' << pkE->V[0] << ",v" << pkE->V[1] << "; ";

        for (j = 0; j < pkE->T.GetSize(); j++)
            kOStr << 't' << kTIndex[pkE->T[j]] << ',';
        kOStr << '>' << endl;
    }
    kOStr << endl;

    // print triangles
    kOStr << "triangle quantity = " << (int)m_kTMap.size() << endl;
    for (pkTIter = m_kTMap.begin(); pkTIter != m_kTMap.end(); pkTIter++)
    {
        Triangle* pkT = pkTIter->second;
        kOStr << 't' << kTIndex[pkT] << " <"
              << 'v' << pkT->V[0] << ",v" << pkT->V[1] << ",v" << pkT->V[2]
              << "; ";
        if ( pkT->E[0] )
            kOStr << 'e' << kEIndex[pkT->E[0]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( pkT->E[1] )
            kOStr << 'e' << kEIndex[pkT->E[1]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( pkT->E[2] )
            kOStr << 'e' << kEIndex[pkT->E[2]];
        else
            kOStr << '*';
        kOStr << '>' << endl;
    }
    kOStr << endl;
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::Edge::Edge (int iV0, int iV1)
    :
    T(2,2)
{
    V[0] = iV0;
    V[1] = iV1;
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::Edge::~Edge ()
{
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::Triangle::Triangle (int iV0, int iV1, int iV2)
{
    V[0] = iV0;
    V[1] = iV1;
    V[2] = iV2;

    for (int i = 0; i < 3; i++)
        E[i] = NULL;
}
//----------------------------------------------------------------------------
ETNonmanifoldMesh::Triangle::~Triangle ()
{
}
//----------------------------------------------------------------------------
