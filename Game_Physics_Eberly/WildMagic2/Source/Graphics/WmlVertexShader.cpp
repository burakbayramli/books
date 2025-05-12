// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlVertexShader.h"
using namespace Wml;

#include <fstream>
using namespace std;

WmlImplementRTTI(VertexShader,Object);
WmlImplementStream(VertexShader);

const int VertexShader::NONE = -1;
const int VertexShader::MAX_TEXCOORDS = 8;

//----------------------------------------------------------------------------
VertexShader::VertexShader (ShaderConstants* pkConstTemplate, 
    int iVertexRegHint, int iNormalRegHint, int iColorRegHint,
    int iTexCoord0RegHint, int iTexCoord1RegHint, int iTexCoord2RegHint,
    int iTexCoord3RegHint,  int iTexCoord4RegHint, int iTexCoord5RegHint,
    int iTexCoord6RegHint, int iTexCoord7RegHint)
    :
    Shader(pkConstTemplate)
{
    m_iVertexRegHint = iVertexRegHint;
    m_iNormalRegHint = iNormalRegHint;
    m_iColorRegHint = iColorRegHint;
    m_aiTexCoordRegHint[0] = iTexCoord0RegHint;
    m_aiTexCoordRegHint[1] = iTexCoord1RegHint;
    m_aiTexCoordRegHint[2] = iTexCoord2RegHint;
    m_aiTexCoordRegHint[3] = iTexCoord3RegHint;
    m_aiTexCoordRegHint[4] = iTexCoord4RegHint;
    m_aiTexCoordRegHint[5] = iTexCoord5RegHint;
    m_aiTexCoordRegHint[6] = iTexCoord6RegHint;
    m_aiTexCoordRegHint[7] = iTexCoord7RegHint;
}
//----------------------------------------------------------------------------
VertexShader::VertexShader ()
    :
    Shader(NULL)
{
    // internal constructor, for use with load
}
//----------------------------------------------------------------------------
VertexShader* VertexShader::Load (const char* acFilename)
{
    // open stream, load
    ifstream kIStr(acFilename,ios::in|ios::binary);
    if ( !kIStr )
        return NULL;

    VertexShader* pkShader = new VertexShader;
    pkShader->Read(kIStr);
    kIStr.close();
    return pkShader;
}
//----------------------------------------------------------------------------
VertexShader::~VertexShader ()
{
}
//----------------------------------------------------------------------------
void VertexShader::Write (const char* acFilename)
{
    ofstream kOStr(acFilename,ios::out|ios::binary|ios::trunc);
    assert(kOStr.is_open());
    Write(kOStr);
    kOStr.close();
}
//----------------------------------------------------------------------------
void VertexShader::Write (ostream& rkOStr)
{
#ifdef WML_BIG_ENDIAN
    int iDummy = m_iVertexRegHint;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));

    iDummy = m_iNormalRegHint;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));

    iDummy = m_iColorRegHint;
    System::SwapBytes(sizeof(int),&iDummy);
    rkOStr.write((const char*)&iDummy,sizeof(int));

    for (int i = 0; i < 8; i++)
    {
        iDummy = m_aiTexCoordRegHint[i];
        System::SwapBytes(sizeof(int),&iDummy);
        rkOStr.write((const char*)&iDummy,sizeof(int));
    }
#else
    rkOStr.write((const char*)&m_iVertexRegHint,sizeof(int));
    rkOStr.write((const char*)&m_iNormalRegHint,sizeof(int));
    rkOStr.write((const char*)&m_iColorRegHint,sizeof(int));
    rkOStr.write((const char*)m_aiTexCoordRegHint,8*sizeof(int));
#endif

    Shader::Write(rkOStr);
}
//----------------------------------------------------------------------------
void VertexShader::Read (istream& rkIStr)
{
    rkIStr.read((char*)&m_iVertexRegHint,sizeof(int));
    rkIStr.read((char*)&m_iNormalRegHint,sizeof(int));
    rkIStr.read((char*)&m_iColorRegHint,sizeof(int));
    rkIStr.read((char*)m_aiTexCoordRegHint,sizeof(int)*8);

#ifdef WML_BIG_ENDIAN
    System::SwapBytes(sizeof(int),&m_iVertexRegHint);
    System::SwapBytes(sizeof(int),&m_iNormalRegHint);
    System::SwapBytes(sizeof(int),&m_iColorRegHint);
    System::SwapBytes(sizeof(int),8,m_aiTexCoordRegHint);
#endif

    Shader::Read(rkIStr);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* VertexShader::Factory (Stream&)
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
void VertexShader::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // fix later
}
//----------------------------------------------------------------------------
void VertexShader::Link (Stream& rkStream, Stream::Link* pkLink)
{
    // fix later
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool VertexShader::Register (Stream& rkStream)
{
    // fix later

    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void VertexShader::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    // fix later
}
//----------------------------------------------------------------------------
StringTree* VertexShader::SaveStrings ()
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
int VertexShader::GetMemoryUsed () const
{
    // fix later
    int iTotalSize = Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int VertexShader::GetDiskUsed () const
{
    // fix later
    return Object::GetDiskUsed();
}
//----------------------------------------------------------------------------
