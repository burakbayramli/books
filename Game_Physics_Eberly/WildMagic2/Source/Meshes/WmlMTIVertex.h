// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMTIVERTEX_H
#define WMLMTIVERTEX_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM MTIVertex
{
public:
    MTIVertex (int iLabel);

    bool operator< (const MTIVertex& rkV) const;
    bool operator== (const MTIVertex& rkV) const;
    bool operator!= (const MTIVertex& rkV) const;

    int GetLabel () const;

protected:
    int m_iLabel;
};

#include "WmlMTIVertex.inl"

}

#endif
