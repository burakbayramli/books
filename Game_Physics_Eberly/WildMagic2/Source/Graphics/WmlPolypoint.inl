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
inline void Polypoint::SetActiveQuantity (int iActiveQuantity)
{
    if ( 0 <= iActiveQuantity && iActiveQuantity <= m_iVertexQuantity )
        m_iActiveQuantity = iActiveQuantity;
    else
        m_iActiveQuantity = m_iVertexQuantity;
}
//----------------------------------------------------------------------------
inline int Polypoint::GetActiveQuantity () const
{
    return m_iActiveQuantity;
}
//----------------------------------------------------------------------------
inline const int* Polypoint::Indices () const
{
    return m_aiIndex;
}
//----------------------------------------------------------------------------
