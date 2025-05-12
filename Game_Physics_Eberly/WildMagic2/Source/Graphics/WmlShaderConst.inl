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
inline int ShaderConst::GetRegister () const
{
    return m_iRegister;
}
//----------------------------------------------------------------------------
inline const float* ShaderConst::GetData () const
{
    return m_afData;
}
//----------------------------------------------------------------------------
inline float* ShaderConst::GetData ()
{
    return m_afData;
}
//----------------------------------------------------------------------------
inline int ShaderConst::GetSize () const
{
    return m_iSize;
}
//----------------------------------------------------------------------------
inline StateConstantType ShaderConst::GetType () const
{
    return m_iType;
}
//----------------------------------------------------------------------------
inline int ShaderConst::GetTypeOption () const
{
    return m_iTypeOption;
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetRegister (int iRegister)
{
    m_iRegister = iRegister;
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetData (const float* afData)
{
    memcpy(m_afData,afData,sizeof(float)*4*m_iSize);
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetData (const Vector2f& rkData)
{
    m_afData[0] = rkData.X();
    m_afData[1] = rkData.Y();
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetData (const Vector3f& rkData)
{
    m_afData[0] = rkData.X();
    m_afData[1] = rkData.Y();
    m_afData[2] = rkData.Z();
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetData (const Vector4f& rkData)
{
    m_afData[0] = rkData.X();
    m_afData[1] = rkData.Y();
    m_afData[2] = rkData.Z();
    m_afData[3] = rkData.W();
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetData (const Matrix4f& rkData)
{
    assert( m_iSize >= 4 );

    m_afData[ 0] = rkData[0][0];
    m_afData[ 1] = rkData[0][1];
    m_afData[ 2] = rkData[0][2];
    m_afData[ 3] = rkData[0][3];

    m_afData[ 4] = rkData[1][0];
    m_afData[ 5] = rkData[1][1];
    m_afData[ 6] = rkData[1][2];
    m_afData[ 7] = rkData[1][3];

    m_afData[ 8] = rkData[2][0];
    m_afData[ 9] = rkData[2][1];
    m_afData[10] = rkData[2][2];
    m_afData[11] = rkData[2][3];

    m_afData[12] = rkData[3][0];
    m_afData[13] = rkData[3][1];
    m_afData[14] = rkData[3][2];
    m_afData[15] = rkData[3][3];
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetData (float fX, float fY, float fZ, float fW)
{
    m_afData[0] = fX;
    m_afData[1] = fY;
    m_afData[2] = fZ;
    m_afData[3] = fW;
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetSize (int iSize)
{
    delete[] m_afData;
    m_afData = new float[4*iSize];
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetType (StateConstantType iType)
{
    m_iType = iType;
}
//----------------------------------------------------------------------------
inline void ShaderConst::SetTypeOption (int iTypeOption)
{
    m_iTypeOption = iTypeOption;
}
//----------------------------------------------------------------------------
inline bool ShaderConst::NameMatches (const char* acConstantName) const
{
    if ( ( !m_acConstantName ) || ( !acConstantName ) )
        return false;

    return ( !strcmp(acConstantName,m_acConstantName) );
}
//----------------------------------------------------------------------------
