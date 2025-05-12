// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSHADER_H
#define WMLSHADER_H

#include "WmlShaderConstants.h"

namespace Wml
{

class WML_ITEM Shader : public Object
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    // Abstract base class.  Create shaders using derived classes PixelShader
    // and VertexShader.
    virtual ~Shader ();

    // shader types
    enum ShaderType
    {
        NONE,
        VERTEX_SHADER,
        PIXEL_SHADER
    };

    // Program type.  Ideally the abstract shader class should be unaware of
    // the underlying graphics API.  The fact is that the different APIs
    // need different underlying programs, so the shader has to have some API
    // specific information.
    //    
    // This enum indexes into the m_kPrograms array.
    //
    // It could have worked by only using one program, but if shaders were
    // ever to be streamed or to be cross-platform compatible, it makes
    // sense to have both so that it could be loaded and used with any API.
    //
    // This isn't such a big deal because shaders are generally about 1-4kb
    // a piece, which is pretty small.  I figured the user convenience of
    // having a single cross-API compatible file was worth the extra size.
    //
    // If this design decision is bad, the CgConverter program (or whatever
    // creates the shader object) could only create a single object and set
    // the other one to version INVALID, which would cause it to not be
    // able to load in the renderer.

    enum ProgramType
    {
        OPENGL,
        DIRECTX,
        MAXTYPE  // invalid type
    };

    // Shader version.  This enum needs each subset to be in order from most
    // restrictive to least restrictive.  It is used both for the API-specific
    // renderer to store what shader version it supports as well as for the
    // program itself to declare what version it is.  A renderer can use a
    // shader if (renderer.shaderversion >= shader.version).  Perhaps a bad
    // way to do this, but it's pretty flexible and I couldn't come up with
    // a more elegant solution.

    enum ShaderVersion
    {
        // unsupported (API supports no versions)
        UNSUPPORTED,

        // DX Vertex shaders
        VS_1_1,
        VS_2_0,
        VS_2_X,

        // DX Pixel shaders
        PS_1_1,
        PS_1_2,
        PS_1_3,
        PS_2_0,
        PS_2_X,

        // GL Vertex shaders
        ARBVP1,

        // GL Pixel shaders
        ARBFP1,

        // unsupported (too high of a version/invalid program)
        INVALID
    };

    char* GetProgram (ProgramType iType);
    ShaderVersion GetVersion (ProgramType iType) const;
    virtual ShaderType GetType () const;

    // Store and retrieve up to 8 bytes of data.  The value iSize should be
    // between 1 and 8.
    void SetUserData (int iSize, const void* pvData);
    void GetUserData (int iSize, void* pvData);

    // Creates a ShaderConstants object for this shader.  The caller is
    // responsible for deleting the returned object.
    ShaderConstants* BuildConstants ();

    void AddProgram (ProgramType iType, const char* acProgram, 
        ShaderVersion iVersion);

    // TO DO.  Is this for streaming?
    virtual void Write (std::ostream& rkOStr);
    virtual void Write (const char* acFilename) = 0;
    virtual void Read (std::istream& rkIStr);

protected:
    // To facilitate the usage of shaders by multiple objects (so that a user
    // does not have to set up everything).  Shaders will take a
    // ShaderConstants object, which can be used to pass to the constructor
    // of ShaderConstants to create a copy.  Shader will handle the deletion
    // of pkConstTemplate.
    Shader (ShaderConstants* pkConstTemplate);

    // the shader program(s)
    char* m_aacProgram[MAXTYPE];

    // the shader program versions
    Shader::ShaderVersion m_aiVersion[MAXTYPE];

    // the DX handle or the GL index for the program
    char m_acUserData[8];

    // constants that are needed by this shader
    ShaderConstants* m_pkConstTemplate;
};

#include "WmlShader.inl"
WmlSmartPointer(Shader);
WmlRegisterStream(Shader);

}

#endif
