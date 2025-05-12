// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLVERTEXSHADER_H
#define WMLVERTEXSHADER_H

#include "WmlShader.h"

namespace Wml
{

class WML_ITEM VertexShader : public Shader
{
    WmlDeclareRTTI;
    WmlDeclareStream;

public:
    static const int NONE;
    static const int MAX_TEXCOORDS;

    // Create a vertex shader with a program.  This class also needs
    // to know what information the Shader needs to be passed and what
    // registers it is expecting them in.  
    
    // Every shader needs vertex info.  VertexShader::NONE implies that
    // some information isn't needed.  Other than that, any non-negative
    // number is a hint to the renderer about which register it should put
    // it in.  It is entirely dependent on the renderer whether or not it
    // follows these hints.  (OpenGL cannot, DirectX should).

    // You can have cross-API compatible programs (assuming that you have
    // compiled the same shader from some higher level language into a
    // format for each API).  In this case, it is recommended to pass what
    // DirectX expects for the registers, which will be ignored for OpenGL.

    // DX supports up to 8 texture coordinates.  However, the actual amount
    // supported will be entirely dependant on the maximal amount of textures
    // supported by both the engine and your video card.
    VertexShader (ShaderConstants* pkConstTemplate, int iVertexRegHint, 
        int iNormalRegHint = NONE, int iColorRegHint = NONE,
        int iTexCoord0RegHint = NONE, int iTexCoord1RegHint = NONE,
        int iTexCoord2RegHint = NONE, int iTexCoord3RegHint = NONE, 
        int iTexCoord4RegHint = NONE, int iTexCoord5RegHint = NONE,
        int iTexCoord6RegHint = NONE, int iTexCoord7RegHint = NONE);

    // returns NULL if invalid or nonexistant file
    static VertexShader* Load (const char* acFilename);

    virtual ~VertexShader ();

    virtual Shader::ShaderType GetType () const;

    bool NeedsNormals ();
    bool NeedsColor ();
    bool NeedsTexCoords (int iTexCoordNum);

    int GetVertexRegHint ();
    int GetNormalRegHint ();
    int GetColorRegHint ();
    int GetTexCoordRegHint (int iTexCoordNum);

    virtual void Write (const char* acFilename);
    virtual void Write (std::ostream& rkOStr);
    virtual void Read (std::istream& rkIStr);

protected:
    // internal constructor for load function
    VertexShader ();

    int m_iVertexRegHint;
    int m_iNormalRegHint;
    int m_iColorRegHint;
    int m_aiTexCoordRegHint[8];

};

#include "WmlVertexShader.inl"
WmlSmartPointer(VertexShader);
WmlRegisterStream(VertexShader);

}

#endif
