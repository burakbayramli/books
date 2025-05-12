// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline MTIEdge::MTIEdge (int iLabel0, int iLabel1)
{
    if ( iLabel0 < iLabel1 )
    {
        // L0 is minimum
        m_aiLabel[0] = iLabel0;
        m_aiLabel[1] = iLabel1;
    }
    else
    {
        // L1 is minimum
        m_aiLabel[0] = iLabel1;
        m_aiLabel[1] = iLabel0;
    }
}
//----------------------------------------------------------------------------
inline int MTIEdge::GetLabel (int i) const
{
    assert( 0 <= i && i < 2 );
    return m_aiLabel[i];
}
//----------------------------------------------------------------------------
inline bool MTIEdge::operator< (const MTIEdge& rkE) const
{
    if ( m_aiLabel[1] < rkE.m_aiLabel[1] )
        return true;

    if ( m_aiLabel[1] == rkE.m_aiLabel[1] )
        return m_aiLabel[0] < rkE.m_aiLabel[0];

    return false;
}
//----------------------------------------------------------------------------
inline bool MTIEdge::operator== (const MTIEdge& rkE) const
{
    return m_aiLabel[0] == rkE.m_aiLabel[0]
        && m_aiLabel[1] == rkE.m_aiLabel[1];
}
//----------------------------------------------------------------------------
inline bool MTIEdge::operator!= (const MTIEdge& rkE) const
{
    return !operator==(rkE);
}
//----------------------------------------------------------------------------
