// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMTTRIANGLE_H
#define WMLMTTRIANGLE_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM MTTriangle
{
public:
    MTTriangle (int iLabel = -1);
    MTTriangle (const MTTriangle& rkT);
    virtual ~MTTriangle ();

    MTTriangle& operator= (const MTTriangle& rkT);

    int GetLabel () const;
    int& Label ();

    int GetVertex (int i) const;
    int& Vertex (int i);
    bool ReplaceVertex (int iVOld, int iVNew);

    int GetEdge (int i) const;
    int& Edge (int i);
    bool ReplaceEdge (int iEOld, int iENew);

    int GetAdjacent (int i) const;
    int& Adjacent (int i);
    bool ReplaceAdjacent (int iAOld, int iANew);

    bool operator== (const MTTriangle& rkT) const;

protected:
    int m_iLabel;
    int m_aiVertex[3];
    int m_aiEdge[3];
    int m_aiAdjacent[3];
};

#include "WmlMTTriangle.inl"

}

#endif
