// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLETMANIFOLDMESH_H
#define WMLETMANIFOLDMESH_H

#include "WmlEdgeKey.h"
#include "WmlTriangleKey.h"
#include <map>

namespace Wml
{

class WML_ITEM ETManifoldMesh
{
public:
    // edge data types
    class Edge;
    typedef Edge* EPtr;
    typedef const Edge* ECPtr;
    typedef EPtr (*ECreator)(int,int);
    typedef std::map<EdgeKey,Edge*> EMap;
    typedef EMap::iterator EMapIterator;
    typedef EMap::const_iterator EMapCIterator;

    // triangle data types
    class Triangle;
    typedef Triangle* TPtr;
    typedef const Triangle* TCPtr;
    typedef TPtr (*TCreator)(int,int,int);
    typedef std::map<TriangleKey,Triangle*> TMap;
    typedef TMap::iterator TMapIterator;
    typedef TMap::const_iterator TMapCIterator;

    // edge object
    class WML_ITEM Edge
    {
    public:
        Edge (int iV0, int iV1);
        virtual ~Edge ();

        int V[2];
        TPtr T[2];
    };

    // triangle object
    class WML_ITEM Triangle
    {
    public:
        Triangle (int iV0, int iV1, int iV2);
        virtual ~Triangle ();

        // vertices, listed in counterclockwise order (V[0],V[1],V[2])
        int V[3];

        // adjacent edges
        // E[0] points to edge (V[0],V[1])
        // E[1] points to edge (V[1],V[2])
        // E[2] points to edge (V[2],V[0])
        EPtr E[3];

        // adjacent triangles
        //   T[0] points to triangle sharing edge E[0]
        //   T[1] points to triangle sharing edge E[1]
        //   T[2] points to triangle sharing edge E[2]
        TPtr T[3];
    };


    // construction and destruction
    ETManifoldMesh (ECreator oECreator = NULL, TCreator oTCreator = NULL);
    virtual ~ETManifoldMesh ();

    // member access
    const EMap& GetEdges () const;
    const TMap& GetTriangles () const;

    // mesh manipulation
    TPtr InsertTriangle (int iV0, int iV1, int iV2);
    bool RemoveTriangle (int iV0, int iV1, int iV2);

    // manifold mesh is closed if each edge is shared twice
    bool IsClosed () const;

    void Print (const char* acFilename);

protected:
    // edges
    static EPtr CreateEdge (int iV0, int iV1);
    ECreator m_oECreator;
    EMap m_kEMap;

    // triangles
    static TPtr CreateTriangle (int iV0, int iV1, int iV2);
    TCreator m_oTCreator;
    TMap m_kTMap;
};

#include "WmlETManifoldMesh.inl"

}

#endif
