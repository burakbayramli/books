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
inline BoundingVolume::Type BoxBV::GetType () const
{
    return BV_BOX;
}
//----------------------------------------------------------------------------
inline bool BoxBV::IsValid () const
{
    return m_kBox.Center().X() != Mathf::MAX_REAL;
}
//----------------------------------------------------------------------------
inline void BoxBV::Invalidate ()
{
    m_kBox.Center().X() = Mathf::MAX_REAL;
}
//----------------------------------------------------------------------------
