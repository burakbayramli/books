// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMTVERTEX_H
#define WMLMTVERTEX_H

#include "WmlUnorderedSet.h"

namespace Wml
{

class WML_ITEM MTVertex
{
public:
    MTVertex (int iLabel = -1, int iEGrow = 0, int iTGrow = 0);
    MTVertex (const MTVertex& rkV);
    virtual ~MTVertex ();

    MTVertex& operator= (const MTVertex& rkV);

    // Vertex labels are read-only since they are used for maps in the MTMesh
    // class for inverse look-up.
    int GetLabel () const;

    int GetEdgeQuantity () const;
    int GetEdge (int i) const;
    bool InsertEdge (int iE);
    bool RemoveEdge (int iE);
    bool ReplaceEdge (int iEOld, int iENew);

    int GetTriangleQuantity () const;
    int GetTriangle (int i) const;
    bool InsertTriangle (int iT);
    bool RemoveTriangle (int iT);
    bool ReplaceTriangle(int iTOld, int iTNew);

    bool operator== (const MTVertex& rkV) const;

protected:
    int m_iLabel;
    UnorderedSet<int> m_kESet, m_kTSet;
};

#include "WmlMTVertex.inl"

}

#endif
