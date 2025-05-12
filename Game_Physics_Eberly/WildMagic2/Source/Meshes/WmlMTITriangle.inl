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
inline MTITriangle::MTITriangle (int iLabel0, int iLabel1, int iLabel2)
{
    if ( iLabel0 < iLabel1 )
    {
        if ( iLabel0 < iLabel2 )
        {
            // L0 is minimum
            m_aiLabel[0] = iLabel0;
            m_aiLabel[1] = iLabel1;
            m_aiLabel[2] = iLabel2;
        }
        else
        {
            // L2 is minimum
            m_aiLabel[0] = iLabel2;
            m_aiLabel[1] = iLabel0;
            m_aiLabel[2] = iLabel1;
        }
    }
    else
    {
        if ( iLabel1 < iLabel2 )
        {
            // L1 is minimum
            m_aiLabel[0] = iLabel1;
            m_aiLabel[1] = iLabel2;
            m_aiLabel[2] = iLabel0;
        }
        else
        {
            // L2 is minimum
            m_aiLabel[0] = iLabel2;
            m_aiLabel[1] = iLabel0;
            m_aiLabel[2] = iLabel1;
        }
    }
}
//----------------------------------------------------------------------------
inline int MTITriangle::GetLabel (int i) const
{
    assert( 0 <= i && i < 3 );
    return m_aiLabel[i];
}
//----------------------------------------------------------------------------
inline bool MTITriangle::operator< (const MTITriangle& rkT) const
{
    if ( m_aiLabel[2] < rkT.m_aiLabel[2] )
        return true;

    if ( m_aiLabel[2] == rkT.m_aiLabel[2] )
    {
        if ( m_aiLabel[1] < rkT.m_aiLabel[1] )
            return true;

        if ( m_aiLabel[1] == rkT.m_aiLabel[1] )
            return m_aiLabel[0] < rkT.m_aiLabel[0];
    }

    return false;
}
//----------------------------------------------------------------------------
inline bool MTITriangle::operator== (const MTITriangle& rkT) const
{
    return (m_aiLabel[0] == rkT.m_aiLabel[0]) &&
    ((m_aiLabel[1] == rkT.m_aiLabel[1] && m_aiLabel[2] == rkT.m_aiLabel[2])
    ||
    (m_aiLabel[1] == rkT.m_aiLabel[2] && m_aiLabel[2] == rkT.m_aiLabel[1]));
}
//----------------------------------------------------------------------------
inline bool MTITriangle::operator!= (const MTITriangle& rkT) const
{
    return !operator==(rkT);
}
//----------------------------------------------------------------------------
