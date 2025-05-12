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
inline BoundingVolume::Type SphereBV::GetType () const
{
    return BV_SPHERE;
}
//----------------------------------------------------------------------------
inline bool SphereBV::IsValid () const
{
    return m_kSphere.Center().X() != Mathf::MAX_REAL;
}
//----------------------------------------------------------------------------
inline void SphereBV::Invalidate ()
{
    m_kSphere.Center().X() = Mathf::MAX_REAL;
}
//----------------------------------------------------------------------------
