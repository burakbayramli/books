// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMTEDGE_H
#define WMLMTEDGE_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM MTEdge
{
public:
    MTEdge (int iLabel = -1);
    MTEdge (const MTEdge& rkE);
    virtual ~MTEdge ();

    MTEdge& operator= (const MTEdge& rkE);

    int GetLabel () const;
    int& Label ();

    int GetVertex (int i) const;
    int& Vertex (int i);
    bool ReplaceVertex (int iVOld, int iVNew);

    int GetTriangle (int i) const;
    int& Triangle (int i);
    bool ReplaceTriangle (int iTOld, int iTNew);

    bool operator== (const MTEdge& rkE) const;

protected:
    int m_iLabel;
    int m_aiVertex[2];
    int m_aiTriangle[2];
};

#include "WmlMTEdge.inl"

}

#endif
