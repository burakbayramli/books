// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlPixelShader.h"
using namespace Wml;

#include <fstream>
using namespace std;

WmlImplementRTTI(PixelShader,Object);
WmlImplementStream(PixelShader);

//----------------------------------------------------------------------------
PixelShader::PixelShader (ShaderConstants* pkConstTemplate)
    :
    Shader(pkConstTemplate)
{
}
//----------------------------------------------------------------------------
PixelShader::PixelShader ()
    :
    Shader(NULL)
{
    // internal constructor, for use with load
}
//----------------------------------------------------------------------------
PixelShader* PixelShader::Load (const char* acFilename)
{
    // open stream, load
    ifstream kIStr(acFilename,ios::in|ios::binary);
    if ( !kIStr )
        return NULL;

    PixelShader* pkShader = new PixelShader;
    pkShader->Read(kIStr);
    kIStr.close();
    return pkShader;
}
//----------------------------------------------------------------------------
PixelShader::~PixelShader ()
{
}
//----------------------------------------------------------------------------
void PixelShader::Write (const char* acFilename)
{
    ofstream kOStr(acFilename,ios::out|ios::binary|ios::trunc);
    assert(kOStr.is_open());
    Shader::Write(kOStr);
    kOStr.close();
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// streaming
//----------------------------------------------------------------------------
Object* PixelShader::Factory (Stream&)
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
void PixelShader::Load (Stream& rkStream, Stream::Link* pkLink)
{
    Object::Load(rkStream,pkLink);

    // fix later
}
//----------------------------------------------------------------------------
void PixelShader::Link (Stream& rkStream, Stream::Link* pkLink)
{
    // fix later
    Object::Link(rkStream,pkLink);
}
//----------------------------------------------------------------------------
bool PixelShader::Register (Stream& rkStream)
{
    // fix later

    return Object::Register(rkStream);
}
//----------------------------------------------------------------------------
void PixelShader::Save (Stream& rkStream)
{
    Object::Save(rkStream);

    // native data
    // fix later
}
//----------------------------------------------------------------------------
StringTree* PixelShader::SaveStrings ()
{
    // fix later
    return NULL;
}
//----------------------------------------------------------------------------
int PixelShader::GetMemoryUsed () const
{
    // fix later
    int iTotalSize = Object::GetMemoryUsed();
    return iTotalSize;
}
//----------------------------------------------------------------------------
int PixelShader::GetDiskUsed () const
{
    // fix later
    return Object::GetDiskUsed();
}
//----------------------------------------------------------------------------
