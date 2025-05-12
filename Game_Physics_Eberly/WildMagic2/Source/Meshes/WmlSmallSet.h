// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSMALLSET_H
#define WMLSMALLSET_H

// This template class is used by Mgc::TriangleMesh to store the edges and
// triangles that share a vertex and to store the triangles that share an
// edge.  Before this the 'set' class was used from STL, but the memory
// overhead for the class is enormous.  As an example, I used the code to
// build the adjacency information for a level surface that was a collection
// of 825K vertices and 1.6M triangles (mesh had 2.5M edges).  The memory
// used by the program that built the adjacency information was 980M.  The
// program based on Mgc::SmallSet used 503M.
//
// The idea is that for a typical mesh, the average number of edges sharing
// a vertex is 6 and the average number of triangles sharing a vertex is 6.
// The average number of triangles sharing an edge is 2.  Since the sets of
// adjacent objects are small, the Insert method does a linear search to
// check if the element already exists.  Reallocation occurs if the current
// set is full to make room for the new element.  The Remove method does a
// linear search and, if the requested element is found, removes it and
// shifts the higher-indexed array elements to fill in the gap.  Because of
// the linear searches, this class should not be used for large sets.  Large
// sets are better handled by a hashed data structure.

#include "WmlSystem.h"

namespace Wml
{

template <class T>
class SmallSet
{
public:
    SmallSet ();
    SmallSet (int iCapacity, int iGrowBy);
    SmallSet (const SmallSet& rkSet);
    ~SmallSet ();

    SmallSet& operator= (const SmallSet& rkSet);

    int GetCapacity () const;
    int GetGrowBy () const;
    int GetSize () const;
    const T* GetElements () const;
    const T& operator[] (int i) const;

    bool Insert (const T& rkElement);
    void InsertNoCheck (const T& rkElement);
    bool Remove (const T& rkElement);
    bool Exists (const T& rkElement);

    // make empty set, keep capacity and growth parameters
    void Clear ();

    // make empty set, reallocate using new capacity and growth parameters
    void Clear (int iCapacity, int iGrowBy);

protected:
    int m_iCapacity, m_iGrowBy, m_iSize;
    T* m_atElement;
};

#include "WmlSmallSet.inl"

}

#endif
