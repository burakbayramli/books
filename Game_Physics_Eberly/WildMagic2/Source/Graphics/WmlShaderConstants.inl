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
inline ShaderConstPtr ShaderConstants::GetConstant (int iConstantNum) const
{
    return m_kConstants[iConstantNum];
}
//----------------------------------------------------------------------------
inline int ShaderConstants::GetNumConstants () const
{
    return (int)m_kConstants.size();
}
//----------------------------------------------------------------------------
