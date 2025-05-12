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
inline void StringTree::SetStringQuantity (int iQuantity)
{
    m_kStrings.resize(iQuantity);
}
//----------------------------------------------------------------------------
inline int StringTree::GetStringQuantity () const
{
    return (int)m_kStrings.size();
}
//----------------------------------------------------------------------------
inline char* StringTree::GetString (int i)
{
    return ( i < (int)m_kStrings.size() ? m_kStrings[i] : NULL );
}
//----------------------------------------------------------------------------
inline void StringTree::SetChildQuantity (int iQuantity)
{
    m_kChildren.resize(iQuantity);
}
//----------------------------------------------------------------------------
inline int StringTree::GetChildQuantity () const
{
    return (int)m_kChildren.size();
}
//----------------------------------------------------------------------------
inline StringTree* StringTree::GetChild (int i)
{
    return ( i < (int)m_kChildren.size() ? m_kChildren[i] : NULL );
}
//----------------------------------------------------------------------------
