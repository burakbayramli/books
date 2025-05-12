// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLMTITRIANGLE_H
#define WMLMTITRIANGLE_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM MTITriangle
{
public:
    MTITriangle (int iLabel0, int iLabel1, int iLabel2);
    bool operator< (const MTITriangle& rkT) const;
    bool operator== (const MTITriangle& rkT) const;
    bool operator!= (const MTITriangle& rkT) const;

    int GetLabel (int i) const;

protected:
    int m_aiLabel[3];
};

#include "WmlMTITriangle.inl"

}

#endif
