// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlShader.h"
#include "WmlRenderer.h"
using namespace Wml;

WmlImplementRTTI(Shader,Object);
WmlImplementStream(Shader);

//----------------------------------------------------------------------------
Shader::Shader (ShaderConstants* pkConstTemplate)
    :
    m_pkConstTemplate(pkConstTemplate)
{
    memset(m_acUserData,0,8*sizeof(char));
    for (int i = 0; i < MAXTYPE; i++)
    {
        m_aiVersion[i] = INVALID;
        m_aacProgram[i] = NULL;
    }
}
//----------------------------------------------------------------------------
Shader::~Shader ()
{
    for (int i = 0; i < MAXTYPE; i++)
        delete[] m_aacProgram[i];

    delete m_pkConstTemplate;

    Renderer::OnDestroyShader(this);
}
//----------------------------------------------------------------------------
void Shader::AddProgram (ProgramType iType, const char* acProgram, 
    ShaderVersion iVersion)
{
    delete[] m_aacProgram[iType];
    m_aacProgram[iType] = new char[strlen(acProgram)+1];
    strcpy(m_aacProgram[iType],acProgram);
    m_aacProgram[iType][strlen(acProgram)] = 0;
    m_aiVersion[iType] = iVersion;
}
//----------------------------------------------------------------------------
ShaderConstants* Shader::BuildConstants ()
{
    ShaderConstants* pkConsts = new ShaderConstants(*m_pkConstTemplate);
    return pkConsts;
}
//----------------------------------------------------------------------------
void Shader::Write (std::ostream& rkOStr)
{
    // numtypes
    int i, iNumPrograms = 0;
    for (i = 0; i < MAXTYPE; i++)
    {
        if ( m_aacProgram[i] )
            iNumPrograms++;
    }

#ifdef WML_BIG_ENDIAN
    int iDummy = iNumPrograms;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));
#else
    rkOStr.write((const char*)&iNumPrograms,sizeof(int));
#endif

    for (i = 0; i < MAXTYPE; i++)
    {
        if ( !m_aacProgram[i] )
            break;

#ifdef WML_BIG_ENDIAN
        iDummy = i;
        System::SwapBytes(sizeof(int),&iDummy);
        rkOStr.write((const char*)&iDummy,sizeof(int));
#else
        rkOStr.write((const char*)&i,sizeof(int));
#endif

        int iLen = (int)strlen(m_aacProgram[i]);
#ifdef WML_BIG_ENDIAN
        iDummy = iLen;
        System::SwapBytes(sizeof(int),&iDummy);
        rkOStr.write((const char*)&iDummy,sizeof(int));
#else
        rkOStr.write((const char*)&iLen,sizeof(int));
#endif

        rkOStr.write(m_aacProgram[i],iLen);

        // TO DO.  This is an enum.  Replace with Wml::Stream style enum
        // write to avoid 'small' enum storage issues.
#ifdef WML_BIG_ENDIAN
        iDummy = m_aiVersion[i];
        System::SwapBytes(sizeof(int),&iDummy);
        rkOStr.write((const char*)&iDummy,sizeof(int));
#else
        rkOStr.write((const char*)&m_aiVersion[i],sizeof(ShaderVersion));
#endif
    }
    m_pkConstTemplate->Write(rkOStr);
}
//----------------------------------------------------------------------------
void Shader::Read (std::istream& rkIStr)
{
    // Initialize all shaders to invalid versions with null programs in case
    // the file only includes a single type.
    int i;
    for (i = 0; i < MAXTYPE; i++)
    {
        if ( m_aacProgram[i] )
        {
            delete[] m_aacProgram[i];
            m_aacProgram[i] = NULL;
        }

        m_aiVersion[i] = INVALID;
    }

    int iNumPrograms;
    rkIStr.read((char*)&iNumPrograms,sizeof(int));
#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&iNumPrograms);
#endif

    for (int iProg = 0; iProg < iNumPrograms; iProg++)
    {
        rkIStr.read((char*)&i,sizeof(int));
#ifdef WML_BIG_ENDIAN
        System::SwapBytes(sizeof(int),&i);
#endif

        int iSize;
        rkIStr.read((char*)&iSize,sizeof(int));
#ifdef WML_BIG_ENDIAN
        System::SwapBytes(sizeof(int),&iSize);
#endif

        m_aacProgram[i] = new char[iSize+1];
        rkIStr.read(m_aacProgram[i],iSize);
        m_aacProgram[i][iSize] = 0;

        int iTemp;
        rkIStr.read((char*)&iTemp,sizeof(int));
#ifdef WML_BIG_ENDIAN
        System::SwapBytes(sizeof(int),&iTemp);
#endif

        m_aiVersion[i] = (ShaderVersion)iTemp;
    }

    if ( !m_pkConstTemplate )
        m_pkConstTemplate = new ShaderConstants();

    // read the constants
    m_pkConstTemplate->Read(rkIStr);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* Shader::Factory (Stream&)
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
void Shader::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // fix later
}
//----------------------------------------------------------------------------
void Shader::Link (Stream& rkStream, Stream::Link* pkLink)
{
    // fix later
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool Shader::Register (Stream& rkStream)
{
    // fix later

    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void Shader::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    // fix later
}
//----------------------------------------------------------------------------
StringTree* Shader::SaveStrings ()
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
int Shader::GetMemoryUsed () const
{
    // fix later
    int iTotalSize = Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int Shader::GetDiskUsed () const
{
    // fix later
    return Object::GetDiskUsed();
}
//----------------------------------------------------------------------------
