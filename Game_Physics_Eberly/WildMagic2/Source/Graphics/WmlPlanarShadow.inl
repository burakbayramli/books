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
inline NodePtr PlanarShadow::GetCaster () const
{
    return m_spkCaster;
}
//----------------------------------------------------------------------------
inline TriMeshPtr PlanarShadow::GetPlane () const
{
    return m_spkPlane;
}
//----------------------------------------------------------------------------
inline LightPtr PlanarShadow::GetLight () const
{
    return m_spkLight;
}
//----------------------------------------------------------------------------
inline const Vector3f& PlanarShadow::GetPlaneNormal () const
{
    return m_kNormal;
}
//----------------------------------------------------------------------------
inline const Vector3f& PlanarShadow::GetPointOnPlane () const
{
    return m_kPointOnPlane;
}
//----------------------------------------------------------------------------
inline const int& PlanarShadow::GetStencilValue () const
{
    return m_iStencilValue;
}
//----------------------------------------------------------------------------
