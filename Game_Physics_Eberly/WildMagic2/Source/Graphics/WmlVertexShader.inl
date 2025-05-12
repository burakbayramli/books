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
inline Shader::ShaderType VertexShader::GetType () const
{
    return VERTEX_SHADER;
}
//----------------------------------------------------------------------------
inline bool VertexShader::NeedsNormals ()
{
    return m_iNormalRegHint != NONE;
}
//----------------------------------------------------------------------------
inline bool VertexShader::NeedsColor ()
{
    return m_iColorRegHint != NONE;
}
//----------------------------------------------------------------------------
inline bool VertexShader::NeedsTexCoords (int iTexCoordNum)
{
    return m_aiTexCoordRegHint[iTexCoordNum] != NONE;
}
//----------------------------------------------------------------------------
inline int VertexShader::GetVertexRegHint ()
{
    return m_iVertexRegHint;
}
//----------------------------------------------------------------------------
inline int VertexShader::GetNormalRegHint ()
{
    return m_iNormalRegHint;
}
//----------------------------------------------------------------------------
inline int VertexShader::GetColorRegHint ()
{
    return m_iColorRegHint;
}
//----------------------------------------------------------------------------
inline int VertexShader::GetTexCoordRegHint (int iTexCoordNum)
{
    return m_aiTexCoordRegHint[iTexCoordNum];
}
//----------------------------------------------------------------------------
