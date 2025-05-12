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
inline MTIVertex::MTIVertex (int iLabel)
{
    m_iLabel = iLabel;
}
//----------------------------------------------------------------------------
inline int MTIVertex::GetLabel () const
{
    return m_iLabel;
}
//----------------------------------------------------------------------------
inline bool MTIVertex::operator< (const MTIVertex& rkV) const
{
    return m_iLabel < rkV.m_iLabel;
}
//----------------------------------------------------------------------------
inline bool MTIVertex::operator== (const MTIVertex& rkV) const
{
    return m_iLabel == rkV.m_iLabel;
}
//----------------------------------------------------------------------------
inline bool MTIVertex::operator!= (const MTIVertex& rkV) const
{
    return m_iLabel != rkV.m_iLabel;
}
//----------------------------------------------------------------------------
