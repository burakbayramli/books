// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlTerrainVertex.h"
using namespace Wml;

//----------------------------------------------------------------------------
TerrainVertex::TerrainVertex ()
{
    m_akDependent[0] = NULL;
    m_akDependent[1] = NULL;
    m_bEnabled = false;
}
//----------------------------------------------------------------------------
void TerrainVertex::Enable ()
{
    m_bEnabled = true;

    if ( m_akDependent[0] )
    {
        if ( !m_akDependent[0]->m_bEnabled )
            m_akDependent[0]->Enable();
    }
    
    if ( m_akDependent[1] )
    {
        if ( !m_akDependent[1]->m_bEnabled )
            m_akDependent[1]->Enable();
    }
}
//----------------------------------------------------------------------------
void TerrainVertex::Disable ()
{
    m_bEnabled = false;
    
    if ( m_akDependent[0] )
    {
        if ( m_akDependent[0]->m_bEnabled )
            m_akDependent[0]->Disable();
    }

    if ( m_akDependent[1] )
    {
        if ( m_akDependent[1]->m_bEnabled )
            m_akDependent[1]->Disable();
    }
}
//----------------------------------------------------------------------------
