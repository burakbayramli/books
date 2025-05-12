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
inline bool& AlphaState::BlendEnabled ()
{
    return m_bBlendEnabled;
}
//----------------------------------------------------------------------------
inline AlphaState::SrcBlendFunction& AlphaState::SrcBlend ()
{
    return m_eSrcBlend;
}
//----------------------------------------------------------------------------
inline AlphaState::DstBlendFunction& AlphaState::DstBlend ()
{
    return m_eDstBlend;
}
//----------------------------------------------------------------------------
inline bool& AlphaState::TestEnabled ()
{
    return m_bTestEnabled;
}
//----------------------------------------------------------------------------
inline AlphaState::TestFunction& AlphaState::Test ()
{
    return m_eTest;
}
//----------------------------------------------------------------------------
inline float& AlphaState::Reference ()
{
    return m_fReference;
}
//----------------------------------------------------------------------------
