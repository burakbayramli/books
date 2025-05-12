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
inline int BezierPatch::GetDegree () const
{
    return m_iDegree;
}
//----------------------------------------------------------------------------
inline int BezierPatch::GetIndexQuantity () const
{
    return m_iIndexQuantity;
}
//----------------------------------------------------------------------------
inline const int* BezierPatch::Indices () const
{
    return m_aiIndex;
}
//----------------------------------------------------------------------------
