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
inline NodePtr PlanarReflection::GetCaster () const
{
    return m_spkCaster;
}
//----------------------------------------------------------------------------
inline TriMeshPtr PlanarReflection::GetPlane () const
{
    return m_spkPlane;
}
//----------------------------------------------------------------------------
inline const Vector3f& PlanarReflection::GetPlaneNormal () const
{
    return m_kNormal;
}
//----------------------------------------------------------------------------
inline const Vector3f& PlanarReflection::GetPointOnPlane () const
{
    return m_kPointOnPlane;
}
//----------------------------------------------------------------------------
inline const int& PlanarReflection::GetStencilValue () const
{
    return m_iStencilValue;
}
//----------------------------------------------------------------------------
inline const float& PlanarReflection::GetReflectance () const
{
    return m_fReflectance;
}
//----------------------------------------------------------------------------
