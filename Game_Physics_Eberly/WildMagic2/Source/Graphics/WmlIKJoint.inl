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
inline bool& IKJoint::AllowTranslation (int i)
{
    assert( 0 <= i && i < 3 );
    return m_abAllowTrn[i];
}
//----------------------------------------------------------------------------
inline float IKJoint::GetTranslationMin (int i) const
{
    assert( 0 <= i && i < 3 );
    return m_afMinTrn[i];
}
//----------------------------------------------------------------------------
inline float IKJoint::GetTranslationMax (int i) const
{
    assert( 0 <= i && i < 3 );
    return m_afMaxTrn[i];
}
//----------------------------------------------------------------------------
inline float& IKJoint::TranslationSpringDamp (int i)
{
    assert( 0 <= i && i < 3 );
    return m_afDampTrn[i];
}
//----------------------------------------------------------------------------
inline bool& IKJoint::AllowRotation (int i)
{
    assert( 0 <= i && i < 3 );
    return m_abAllowRot[i];
}
//----------------------------------------------------------------------------
inline float IKJoint::GetRotationMin (int i) const
{
    assert( 0 <= i && i < 3 );
    return m_afMinRot[i];
}
//----------------------------------------------------------------------------
inline float IKJoint::GetRotationMax (int i) const
{
    assert( 0 <= i && i < 3 );
    return m_afMaxRot[i];
}
//----------------------------------------------------------------------------
inline float& IKJoint::RotationSpringDamp (int i)
{
    assert( 0 <= i && i < 3 );
    return m_afDampRot[i];
}
//----------------------------------------------------------------------------
