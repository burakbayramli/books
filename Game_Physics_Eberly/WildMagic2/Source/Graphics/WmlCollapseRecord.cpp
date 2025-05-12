// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlCollapseRecord.h"
using namespace Wml;

//----------------------------------------------------------------------------
void CollapseRecord::Read (Stream& rkStream)
{
    StreamRead(rkStream,m_iVKeep);
    StreamRead(rkStream,m_iVThrow);
    StreamRead(rkStream,m_iVQuantity);
    StreamRead(rkStream,m_iTQuantity);
    StreamRead(rkStream,m_iIQuantity);
    if ( m_iIQuantity > 0 )
    {
        m_aiIndex = new int[m_iIQuantity];
        StreamRead(rkStream,m_aiIndex,m_iIQuantity);
    }
    else
    {
        m_aiIndex = NULL;
    }
}
//----------------------------------------------------------------------------
void CollapseRecord::Write (Stream& rkStream)
{
    StreamWrite(rkStream,m_iVKeep);
    StreamWrite(rkStream,m_iVThrow);
    StreamWrite(rkStream,m_iVQuantity);
    StreamWrite(rkStream,m_iTQuantity);
    StreamWrite(rkStream,m_iIQuantity);
    if ( m_iIQuantity > 0 )
        StreamWrite(rkStream,m_aiIndex,m_iIQuantity);
}
//----------------------------------------------------------------------------
int CollapseRecord::GetMemoryUsed () const
{
    int iBaseSize = sizeof(CollapseRecord);
    int iDynaSize = m_iIQuantity*sizeof(m_aiIndex[0]);
    return iBaseSize + iDynaSize;
}
//----------------------------------------------------------------------------
int CollapseRecord::GetDiskUsed () const
{
    return sizeof(m_iVKeep) + sizeof(m_iVThrow) + sizeof(m_iVQuantity) +
        sizeof(m_iTQuantity) + sizeof(m_iIQuantity) +
        ( m_iIQuantity > 0 ? m_iIQuantity*sizeof(m_aiIndex[0]) : 0 );
}
//----------------------------------------------------------------------------
