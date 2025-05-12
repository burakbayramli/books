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
inline bool& FogState::Enabled ()
{
    return m_bEnabled;
}
//----------------------------------------------------------------------------
inline float& FogState::Start ()
{
    return m_fStart;
}
//----------------------------------------------------------------------------
inline float& FogState::End ()
{
    return m_fEnd;
}
//----------------------------------------------------------------------------
inline float& FogState::Density ()
{
    return m_fDensity;
}
//----------------------------------------------------------------------------
inline ColorRGB& FogState::Color ()
{
    return m_kColor;
}
//----------------------------------------------------------------------------
inline FogState::DensityFunction& FogState::DFunction ()
{
    return m_eDFunction;
}
//----------------------------------------------------------------------------
inline FogState::ApplyFunction& FogState::AFunction ()
{
    return m_eAFunction;
}
//----------------------------------------------------------------------------


