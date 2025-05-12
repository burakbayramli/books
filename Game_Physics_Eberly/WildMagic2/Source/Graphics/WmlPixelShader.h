// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLPIXELSHADER_H
#define WMLPIXELSHADER_H

#include "WmlShader.h"

namespace Wml
{

class WML_ITEM PixelShader : public Shader
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    PixelShader (ShaderConstants* pkConstTemplate);

    // returns NULL if invalid or non-existant file
    static PixelShader* Load (const char* acFilename);

    virtual ~PixelShader ();

    virtual Shader::ShaderType GetType () const;

    virtual void Write (const char* acFilename);

protected:
    // internal constructor for load function
    PixelShader ();
};

#include "WmlPixelShader.inl"
WmlSmartPointer(PixelShader);
WmlRegisterStream(PixelShader);

}

#endif
