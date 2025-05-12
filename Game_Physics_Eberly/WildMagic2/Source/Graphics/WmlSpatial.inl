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
inline Matrix3f& Spatial::Rotate ()
{
    return m_kRotate;
}
//----------------------------------------------------------------------------
inline const Matrix3f& Spatial::Rotate () const
{
    return m_kRotate;
}
//----------------------------------------------------------------------------
inline Vector3f& Spatial::Translate ()
{
    return m_kTranslate;
}
//----------------------------------------------------------------------------
inline const Vector3f& Spatial::Translate () const
{
    return m_kTranslate;
}
//----------------------------------------------------------------------------
inline float& Spatial::Scale ()
{
    return m_fScale;
}
//----------------------------------------------------------------------------
inline const float& Spatial::Scale () const
{
    return m_fScale;
}
//----------------------------------------------------------------------------
inline Matrix3f& Spatial::WorldRotate ()
{
    return m_kWorldRotate;
}
//----------------------------------------------------------------------------
inline const Matrix3f& Spatial::WorldRotate () const
{
    return m_kWorldRotate;
}
//----------------------------------------------------------------------------
inline Vector3f& Spatial::WorldTranslate ()
{
    return m_kWorldTranslate;
}
//----------------------------------------------------------------------------
inline const Vector3f& Spatial::WorldTranslate () const
{
    return m_kWorldTranslate;
}
//----------------------------------------------------------------------------
inline float& Spatial::WorldScale ()
{
    return m_fWorldScale;
}
//----------------------------------------------------------------------------
inline const float& Spatial::WorldScale () const
{
    return m_fWorldScale;
}
//----------------------------------------------------------------------------
inline void Spatial::SetWorldTransformToIdentity ()
{
    m_kWorldRotate = Matrix3f::IDENTITY;
    m_kWorldTranslate = Vector3f::ZERO;
    m_fWorldScale = 1.0f;
}
//----------------------------------------------------------------------------
inline Bound& Spatial::WorldBound ()
{
    return m_kWorldBound;
}
//----------------------------------------------------------------------------
inline const Bound& Spatial::WorldBound () const
{
    return m_kWorldBound;
}
//----------------------------------------------------------------------------
inline bool& Spatial::ForceCull ()
{
    return m_bForceCull;
}
//----------------------------------------------------------------------------
inline const bool& Spatial::ForceCull () const
{
    return m_bForceCull;
}
//----------------------------------------------------------------------------
inline void Spatial::SetParent (Node* pkParent)
{
    m_pkParent = pkParent;
}
//----------------------------------------------------------------------------
inline Node* Spatial::GetParent ()
{
    return m_pkParent;
}
//----------------------------------------------------------------------------
inline bool Spatial::PickRecord::operator< (const PickRecord& rkRecord) const
{
    return m_fRayT < rkRecord.m_fRayT;
}
//----------------------------------------------------------------------------
