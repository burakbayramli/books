// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLTERRAINVERTEX_H
#define WMLTERRAINVERTEX_H

#include "WmlSystem.h"

namespace Wml
{

class WML_ITEM TerrainVertex
{
public:
    TerrainVertex ();

    void SetDependent (int i, TerrainVertex* pkDependent);
    TerrainVertex* GetDependent (int i);
    bool GetEnabled () const;
    void Enable ();
    void Disable ();

protected:
    TerrainVertex* m_akDependent[2];
    bool m_bEnabled;
};

#include "WmlTerrainVertex.inl"

}

#endif
