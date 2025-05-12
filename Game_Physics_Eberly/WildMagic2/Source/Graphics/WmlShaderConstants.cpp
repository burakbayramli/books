// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlShaderConstants.h"
using namespace Wml;

WmlImplementRTTI(ShaderConstants,Object);
WmlImplementStream(ShaderConstants);

//----------------------------------------------------------------------------
ShaderConstants::ShaderConstants ()
{
}
//----------------------------------------------------------------------------
ShaderConstants::ShaderConstants (const ShaderConstants& rkConstants)
{
    for (int i = 0; i < rkConstants.GetNumConstants(); i++)
        m_kConstants.push_back(rkConstants.GetConstant(i));
}
//----------------------------------------------------------------------------
ShaderConstants::~ShaderConstants ()
{
}
//----------------------------------------------------------------------------
void ShaderConstants::AddConstant (const char* acConstantName, int iRegister,
    int iSize, StateConstantType iType, int iTypeOption)
{
    m_kConstants.push_back(
        new ShaderConst(acConstantName,iRegister,iSize,iType,iTypeOption));
}
//----------------------------------------------------------------------------
void ShaderConstants::AddConstant (const char* acConstantName, int iRegister,
    int iSize, StateConstantType iType, int iTypeOption,
    const float* afInitialData)
{
    m_kConstants.push_back(
        new ShaderConst(acConstantName,iRegister,iSize,iType,iTypeOption));

    m_kConstants[m_kConstants.size()-1]->SetData(afInitialData);
}
//----------------------------------------------------------------------------
void ShaderConstants::AddConstant (const char* acConstantName, int iRegister,
    int iSize, StateConstantType iType, int iTypeOption,
    const double* adInitialData)
{
    m_kConstants.push_back(
        new ShaderConst(acConstantName,iRegister,iSize,iType,iTypeOption));

    float* afData = m_kConstants[m_kConstants.size()-1]->GetData();
    for (int i = 0; i < 4*iSize; i++)
        afData[i] = (float)adInitialData[i];
}
//----------------------------------------------------------------------------
ShaderConstPtr ShaderConstants::GetConstant (const char* acName) const
{
    for (int i = 0; i < (int)m_kConstants.size(); i++)
    {
        if ( m_kConstants[i]->NameMatches(acName) )
            return GetConstant(i);
    }

    // none found
    return NULL;
}
//----------------------------------------------------------------------------
void ShaderConstants::Write (std::ostream& rkOStr)
{
    int iSize = (int)m_kConstants.size();

#ifdef WML_BIG_ENDIAN
    int iDummy = iSize;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&iSize,sizeof(int));
#endif
    for (int i = 0; i < iSize; i++)
        m_kConstants[i]->Write(rkOStr);
}
//----------------------------------------------------------------------------
void ShaderConstants::Read (std::istream& rkIStr)
{
    int iTemp;
    rkIStr.read((char*)&iTemp,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&iTemp);
#endif

    m_kConstants.resize(iTemp);
    for (int i = 0; i < (int)m_kConstants.size(); i++)
    {
        m_kConstants[i] = new ShaderConst;
        m_kConstants[i]->Read(rkIStr);
    }
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* ShaderConstants::Factory (Stream&)
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
void ShaderConstants::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // fix later
}
//----------------------------------------------------------------------------
void ShaderConstants::Link (Stream& rkStream, Stream::Link* pkLink)
{
    // fix later
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool ShaderConstants::Register (Stream& rkStream)
{
    // fix later

    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void ShaderConstants::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    // fix later
}
//----------------------------------------------------------------------------
StringTree* ShaderConstants::SaveStrings ()
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
int ShaderConstants::GetMemoryUsed () const
{
    // fix later
    int iTotalSize = Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int ShaderConstants::GetDiskUsed () const
{
    // fix later
    return Object::GetDiskUsed();
}
//----------------------------------------------------------------------------
