// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLEDGEKEY_H
#define WMLEDGEKEY_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM EdgeKey
{
public:
    EdgeKey (int iV0 = -1, int iV1 = -1);
    bool operator< (const EdgeKey& rkKey) const;
    int V[2];
};

#include "WmlEdgeKey.inl"

}

#endif
