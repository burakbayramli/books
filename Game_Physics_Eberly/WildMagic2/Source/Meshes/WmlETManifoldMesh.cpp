// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlETManifoldMesh.h"
using namespace Wml;

#include <fstream>
using namespace std;

//----------------------------------------------------------------------------
ETManifoldMesh::ETManifoldMesh (ECreator oECreator, TCreator oTCreator)
{
    m_oECreator = ( oECreator ? oECreator : CreateEdge );
    m_oTCreator = ( oTCreator ? oTCreator : CreateTriangle );
}
//----------------------------------------------------------------------------
ETManifoldMesh::~ETManifoldMesh ()
{
    EMap::iterator pkEIter;
    for (pkEIter = m_kEMap.begin(); pkEIter != m_kEMap.end(); pkEIter++)
        delete pkEIter->second;

    TMap::iterator pkTIter;
    for (pkTIter = m_kTMap.begin(); pkTIter != m_kTMap.end(); pkTIter++)
        delete pkTIter->second;
}
//----------------------------------------------------------------------------
ETManifoldMesh::EPtr ETManifoldMesh::CreateEdge (int iV0, int iV1)
{
    return new Edge(iV0,iV1);
}
//----------------------------------------------------------------------------
ETManifoldMesh::TPtr ETManifoldMesh::CreateTriangle (int iV0, int iV1,
    int iV2)
{
    return new Triangle(iV0,iV1,iV2);
}
//----------------------------------------------------------------------------
ETManifoldMesh::TPtr ETManifoldMesh::InsertTriangle (int iV0, int iV1,
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

            // update edge and triangle
            pkE->T[0] = pkT;
            pkT->E[i0] = pkE;
        }
        else
        {
            // second time edge encountered
            pkE = pkEIter->second;
            assert( pkE );

            // update edge
            if ( pkE->T[1] )
            {
                assert( false );  // mesh must be manifold
                return NULL;
            }
            pkE->T[1] = pkT;

            // update adjacent triangles
            TPtr pkAdj = pkE->T[0];
            assert( pkAdj );
            for (int i = 0; i < 3; i++)
            {
                if ( pkAdj->E[i] == pkE )
                {
                    pkAdj->T[i] = pkT;
                    break;
                }
            }

            // update triangle
            pkT->E[i0] = pkE;
            pkT->T[i0] = pkAdj;
        }
    }

    return pkT;
}
//----------------------------------------------------------------------------
bool ETManifoldMesh::RemoveTriangle (int iV0, int iV1, int iV2)
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
        Edge* pkE = pkT->E[i];
        assert( pkE );
        if ( pkE->T[0] == pkT )
        {
            // one-triangle edges always have pointer in slot zero
            pkE->T[0] = pkE->T[1];
            pkE->T[1] = NULL;
        }
        else if ( pkE->T[1] == pkT )
        {
            pkE->T[1] = NULL;
        }
        else
        {
            assert( false );
            return false;
        }

        // remove edge if you had the last reference to it
        if ( !pkE->T[0] && !pkE->T[1] )
        {
            EdgeKey kEKey(pkE->V[0],pkE->V[1]);
            m_kEMap.erase(kEKey);
            delete pkE;
        }

        // inform adjacent triangles you are going away
        TPtr pkA = pkT->T[i];
        if ( pkA )
        {
            for (int j = 0; j < 3; j++)
            {
                if ( pkA->T[j] == pkT )
                {
                    pkA->T[j] = NULL;
                    break;
                }
            }
        }
    }

    m_kTMap.erase(kTKey);
    delete pkT;
    return true;
}
//----------------------------------------------------------------------------
bool ETManifoldMesh::IsClosed () const
{
    EMapCIterator pkEIter;
    for (pkEIter = m_kEMap.begin(); pkEIter != m_kEMap.end(); pkEIter++)
    {
        const Edge* pkE = pkEIter->second;
        if ( !pkE->T[0] || !pkE->T[1] )
            return false;
    }
    return true;
}
//----------------------------------------------------------------------------
void ETManifoldMesh::Print (const char* acFilename)
{
    ofstream kOStr(acFilename);
    if ( !kOStr )
        return;

    // assign unique indices to the edges
    map<Edge*,int> kEIndex;
    kEIndex[0] = 0;
    int i = 1;
    EMapIterator pkEIter;
    for (pkEIter = m_kEMap.begin(); pkEIter != m_kEMap.end(); pkEIter++)
    {
        if ( pkEIter->second )
            kEIndex[pkEIter->second] = i++;
    }

    // assign unique indices to the triangles
    map<TPtr,int> kTIndex;
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
        const Edge& rkE = *pkEIter->second;
        kOStr << 'e' << kEIndex[pkEIter->second] << " <"
              << 'v' << rkE.V[0] << ",v" << rkE.V[1] << "; ";
        if ( rkE.T[0] )
            kOStr << 't' << kTIndex[rkE.T[0]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( rkE.T[1] )
            kOStr << 't' << kTIndex[rkE.T[1]];
        else
            kOStr << '*';
        kOStr << '>' << endl;
    }
    kOStr << endl;

    // print triangles
    kOStr << "triangle quantity = " << (int)m_kTMap.size() << endl;
    for (pkTIter = m_kTMap.begin(); pkTIter != m_kTMap.end(); pkTIter++)
    {
        const Triangle& rkT = *pkTIter->second;
        kOStr << 't' << kTIndex[pkTIter->second] << " <"
              << 'v' << rkT.V[0] << ",v" << rkT.V[1] << ",v" << rkT.V[2]
              << "; ";
        if ( rkT.E[0] )
            kOStr << 'e' << kEIndex[rkT.E[0]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( rkT.E[1] )
            kOStr << 'e' << kEIndex[rkT.E[1]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( rkT.E[2] )
            kOStr << 'e' << kEIndex[rkT.E[2]];
        else
            kOStr << '*';
        kOStr << "; ";

        if ( rkT.T[0] )
            kOStr << 't' << kTIndex[rkT.T[0]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( rkT.T[1] )
            kOStr << 't' << kTIndex[rkT.T[1]];
        else
            kOStr << '*';
        kOStr << ',';
        if ( rkT.T[2] )
            kOStr << 't' << kTIndex[rkT.T[2]];
        else
            kOStr << '*';
        kOStr << '>' << endl;
    }
    kOStr << endl;
}
//----------------------------------------------------------------------------
ETManifoldMesh::Edge::Edge (int iV0, int iV1)
{
    V[0] = iV0;
    V[1] = iV1;
    T[0] = NULL;
    T[1] = NULL;
}
//----------------------------------------------------------------------------
ETManifoldMesh::Edge::~Edge ()
{
}
//----------------------------------------------------------------------------
ETManifoldMesh::Triangle::Triangle (int iV0, int iV1, int iV2)
{
    V[0] = iV0;
    V[1] = iV1;
    V[2] = iV2;

    for (int i = 0; i < 3; i++)
    {
        E[i] = NULL;
        T[i] = NULL;
    }
}
//----------------------------------------------------------------------------
ETManifoldMesh::Triangle::~Triangle ()
{
}
//----------------------------------------------------------------------------
