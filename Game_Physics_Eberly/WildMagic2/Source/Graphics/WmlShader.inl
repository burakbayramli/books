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
inline char* Shader::GetProgram (ProgramType iType)
{
    return m_aacProgram[iType];
}
//----------------------------------------------------------------------------
inline Shader::ShaderVersion Shader::GetVersion (ProgramType iType) const
{
    return m_aiVersion[iType];
}
//----------------------------------------------------------------------------
inline Shader::ShaderType Shader::GetType () const
{
    return NONE;
}
//----------------------------------------------------------------------------
inline void Shader::SetUserData (int iSize, const void* pvData)
{
    assert( 1 <= iSize && iSize <= 8 );
    memcpy(m_acUserData,pvData,iSize*sizeof(char));
}
//----------------------------------------------------------------------------
inline void Shader::GetUserData (int iSize, void* pvData)
{
    assert( 1 <= iSize && iSize <= 8 );
    memcpy(pvData,m_acUserData,iSize*sizeof(char));
}
//----------------------------------------------------------------------------
