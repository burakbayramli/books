// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlShaderConst.h"
using namespace Wml;

WmlImplementRTTI(ShaderConst,Object);
WmlImplementStream(ShaderConst);

//----------------------------------------------------------------------------
ShaderConst::ShaderConst ()
{
    m_acConstantName = NULL;
    m_iRegister = 0;
    m_iSize = 0;
    m_afData = NULL;
    m_iType = USER_DEFINED;
    m_iTypeOption = 0;
}
//----------------------------------------------------------------------------
ShaderConst::ShaderConst (const char* acConstantName, int iRegister,
    int iSize, StateConstantType iType, int iTypeOption)
{
    m_acConstantName = new char[strlen(acConstantName)+1];
    strcpy(m_acConstantName,acConstantName);
    m_iRegister = iRegister;
    m_afData = new float[4*iSize];
    memset(m_afData,0,4*iSize*sizeof(float));
    m_iSize = iSize;
    m_iType = iType;
    m_iTypeOption = iTypeOption;
}
//----------------------------------------------------------------------------
ShaderConst::ShaderConst (const ShaderConst* pkShaderConst)
{
    assert( pkShaderConst );
    if ( !pkShaderConst )
        return;

    m_acConstantName = NULL;
    m_afData = NULL;
    *this = *pkShaderConst;
}
//----------------------------------------------------------------------------
ShaderConst::ShaderConst (const ShaderConst& rkConst)
{
    m_acConstantName = NULL;
    m_afData = NULL;
    *this = rkConst;
}
//----------------------------------------------------------------------------
ShaderConst& ShaderConst::operator= (const ShaderConst& rkConst)
{
    delete[] m_acConstantName;
    if ( rkConst.m_acConstantName )
    {
        m_acConstantName = new char[strlen(rkConst.m_acConstantName)+1];
        strcpy(m_acConstantName,rkConst.m_acConstantName);
    }
    else
    {
        m_acConstantName = NULL;
    }
    m_iRegister = rkConst.m_iRegister;

    delete[] m_afData;
    m_afData = new float[4*rkConst.m_iSize];
    m_iSize = rkConst.m_iSize;
    memcpy(m_afData,rkConst.m_afData,4*m_iSize*sizeof(float));
    m_iType = rkConst.m_iType;
    m_iTypeOption = rkConst.m_iTypeOption;
    
    return *this;
}
//----------------------------------------------------------------------------
ShaderConst::~ShaderConst ()
{
    delete[] m_acConstantName;
    delete[] m_afData;
}
//----------------------------------------------------------------------------
void ShaderConst::Write (std::ostream& rkOStr)
{
#ifdef WML_BIG_ENDIAN
    int iDummy = m_iRegister;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&m_iRegister,sizeof(int));
#endif

#ifdef WML_BIG_ENDIAN
    iDummy = m_iSize;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&m_iSize,sizeof(int));
#endif

#ifdef WML_BIG_ENDIAN
    for (int i = 0; i < 4*m_iSize; i++)
    {
        float fDummy = m_afData[i];
        System::SwapBytes(sizeof(float),&fDummy);
        rkOStr.write((const char*)&fDummy,sizeof(float));
    }
#else
    rkOStr.write((const char*)m_afData,sizeof(float)*4*m_iSize);
#endif

    // TO DO.  This is an 'enum' type.  Use Wml::Stream style for writing
    // the enum.
#ifdef WML_BIG_ENDIAN
    iDummy = m_iType;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&m_iType,sizeof(StateConstantType));
#endif

#ifdef WML_BIG_ENDIAN
    iDummy = m_iTypeOption;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&m_iTypeOption,sizeof(int));
#endif

    int iTemp = (int)strlen(m_acConstantName);
#ifdef WML_BIG_ENDIAN
    iDummy = iTemp;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&iTemp,sizeof(int));
#endif

    rkOStr.write(m_acConstantName,iTemp);
}
//----------------------------------------------------------------------------
void ShaderConst::Read (std::istream& rkIStr)
{
    rkIStr.read((char*)&m_iRegister,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&m_iRegister);
#endif

    rkIStr.read((char*)&m_iSize,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&m_iSize);
#endif

    delete[] m_afData;
    m_afData = new float[4*m_iSize];
    rkIStr.read((char*)m_afData,sizeof(float)*4*m_iSize);
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(float),4*m_iSize,m_afData);
#endif

    rkIStr.read((char*)&m_iType,sizeof(StateConstantType));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&m_iType);
#endif

    rkIStr.read((char*)&m_iTypeOption,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&m_iTypeOption);
#endif
    
    int iTemp;
    rkIStr.read((char*)&iTemp,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&iTemp);
#endif

    delete[] m_acConstantName;
    m_acConstantName = new char[iTemp+1];
    rkIStr.read(m_acConstantName,iTemp);
    m_acConstantName[iTemp] = 0;
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ShaderConst::Factory (Stream&)
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
void ShaderConst::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // fix later
}
//----------------------------------------------------------------------------
void ShaderConst::Link (Stream& rkStream, Stream::Link* pkLink)
{
    // fix later
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ShaderConst::Register (Stream& rkStream)
{
    // fix later

    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void ShaderConst::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    // fix later
}
//----------------------------------------------------------------------------
StringTree* ShaderConst::SaveStrings ()
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
int ShaderConst::GetMemoryUsed () const
{
    // fix later
    int iTotalSize = Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ShaderConst::GetDiskUsed () const
{
    // fix later
    return Object::GetDiskUsed();
}
//----------------------------------------------------------------------------
