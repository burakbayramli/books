// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMTIEDGE_H
#define WMLMTIEDGE_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM MTIEdge
{
public:
    MTIEdge (int iLabel0, int iLabel1);
    bool operator< (const MTIEdge& rkE) const;
    bool operator== (const MTIEdge& rkE) const;
    bool operator!= (const MTIEdge& rkE) const;

    int GetLabel (int i) const;

protected:
    int m_aiLabel[2];
};

#include "WmlMTIEdge.inl"

}

#endif
