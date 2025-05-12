// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef CGCONVERTER_H
#define CGCONVERTER_H

#include "CgIncludes.h"
#include "WmlStateConstant.h"
#include "WmlShaderConstants.h"
#include "WmlShader.h"
#include "WmlVertexShader.h"
#include "WmlPixelShader.h"

#include <vector>
using std::vector;

namespace Wml
{

/*
Usage:
cgconverter <inputfile> [-quiet] [-entry <entry>] [-output <output>] -vertex
cgconverter <inputfile> [-quiet] [-entry <entry>] [-output <output>] -pixel
*/


class CgConverter
{

public:
    enum ShaderType
    {
        VERTEXSHADER,
        PIXELSHADER
    };

    CgConverter (char* acFilename, ShaderType iType, char* acEntry = NULL,
        bool bQuiet = false, bool bForceVS2 = false);
    virtual ~CgConverter ();

    void WriteObjectToFile (char* acFilename);

protected:

    // helper functions
    void CreateShaderConstants ();
    void RecurseProgramParameters(CGparameter kParam);
    void AddParam (CGparameter kParam);
    int GetRegister (CGparameter kParam);
    void MatchConstantName (const char* acName, StateConstantType& riType,
        int& riTypeOption);
    bool TryVersion (CGprofile kProfile, const char* acFilename,
        const char* acEntry, const char** aacArgs);
    int GetSize (CGparameter kParam);

    void DbgPrintf (char* acFormat, ...);
    void FatalError (const char* acMessage);

    // state variables
    bool m_bQuiet;
    bool m_bForceVS2;

    // what this converter creates
    CGcontext m_kContext;
    CGprogram m_kProgram;
    ShaderConstants* m_pkConsts;
    Shader* m_pkShader;
    ShaderType m_kType;

    // if a vertex shader, we need:
    int m_iVertexRegHint;
    int m_iNormalRegHint;
    int m_iColorRegHint;
    int m_aiTexCoordRegHint[8];
};

};

#endif

