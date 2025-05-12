// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "CgConverter.h"
#include "WmlCommand.h"

using namespace Wml;

/*
    Possible future improvement: OpenGL has access to (nearly) all state
    in assembly language.  It would be possible to eliminate the need for
    the OpenGLRenderer to look up and set all this state with some creative
    text replacement in the assembly language.  This would require that
    once the OpenGL program has been created, go through it and check
    for all parameter declarations "PARAM c#[#] = { ... };" and for all of the
    state constants that refer to state in openGL, replace what's inside the
    brackets with the correct state.  i.e. if the state constant is
    WmlModViewInverse and it has been bound to c8, then replace the line in
    the program that says "PARAM c8[4] = { program.local[8..11] };" with a 
    line that says "PARAM c8[4] = { state.matrix.inverse.modelview };" (or
    something like that, I can't remember the exact gl state syntax).

    Then, in the OpenGL renderer, in the setstateconstant call, catch all
    of the state constants that you have replaced here and do nothing there.
    You need to leave the constants in the structure because there is no
    equivalent way to do this for DirectX.

    Right now the current behavior in the OGLRenderer is to mimic what the 
    OpenGL driver does by creating the same matrices it would have loaded.
*/

/*
    Other possible fixes.  Right now the -forceVS2 option is a horrible ugly
    kludge.  The CG run-time will gladly compile programs of any length, even
    if DirectX will not compile them due to length limitations (128 for
    vs_1_1)  However, cgc.exe will issue a warning (but not an error??) for
    it, but there is no way to get this warning.  Even -strict does not force
    the error to become a warning.  Thus, there's no real way to tell,
    (aside from grepping the output for the "// %i instructions" line and
    comparing with some preset values in the TryVersion function) to say
    that vs1.1 will not work for a given program.  That would be just as
    ugly a hack, if not uglier.  This is slightly more elegant.  Right now
    it just doesn't try vs1.1 if you have the -forceVS2 option.  Hopefully
    you're running cgc before cgconverter in your build file anyway and you
    might catch the warning as it flies by.  You'll know when the file won't
    compile on DirectX, too.  :P

    If there is a way to do it automagically and elegantly, I think that 
    would be a much better solution.
*/

//----------------------------------------------------------------------------
void print_usage()
{
    printf( "Usage:\n"\
        "cgconverter <inputfile> [-quiet] [-entry <entry>] "\
        "[-output <output>] [-forceVS2] -vertex\n"\
        "cgconverter <inputfile> [-quiet] [-entry <entry>] "\
        "[-output <output>] -pixel\n" );
}
//----------------------------------------------------------------------------
int main(int iArgc, char** aacArgv)
{
    // get the args
    Command kCommand(iArgc, aacArgv);

    char* acFilename = NULL;

    if ( kCommand.Boolean("help") )
    {
        print_usage();
        return -1;
    }

    if ( !kCommand.Filename(acFilename) )
    {
        printf( "Error: no inputfile\n" );
        print_usage();
        return -1;
    }

    bool bQuiet = ( kCommand.Boolean("quiet") ? true : false );
    char* acEntry = NULL;
    kCommand.String("entry",acEntry);
    char* acOutput = NULL;
    kCommand.String("output",acOutput);
    bool bForceVS2 = false;

    CgConverter::ShaderType iType;

    if ( kCommand.Boolean("vertex") )
    {
        iType = CgConverter::VERTEXSHADER;
        // default output is to append .wvs to the filename
        if ( !acOutput )
        {
            acOutput = new char[strlen(acFilename)+5];
            strcpy(acOutput, acFilename);
            strcpy(&acOutput[strlen(acFilename)],".wvs");
        }

        bForceVS2 = ( kCommand.Boolean("forceVS2") ? true : false );
    }
    else if ( kCommand.Boolean("pixel") )
    {
        iType = CgConverter::PIXELSHADER;
        // default output is to append .wps to the filename
        if ( !acOutput )
        {
            acOutput = new char[strlen(acFilename)+5];
            strcpy(acOutput, acFilename);
            strcpy(&acOutput[strlen(acFilename)],".wps");
        }
    }
    else
    {
        if ( !bQuiet )
        {
            printf( "Error: Need to specify -vertex or -pixel\n");
            print_usage();
        }
        return -1;
    }

    // do all the work
    CgConverter kConv(acFilename, iType, acEntry, bQuiet, bForceVS2);
    kConv.WriteObjectToFile(acOutput);

    // clean up
    if ( acEntry )
        delete[] acEntry;
    if ( acFilename )
        delete[] acFilename;
    if ( acOutput )
        delete[] acOutput;
 
    return 0;
}
//----------------------------------------------------------------------------
CgConverter::CgConverter (char* acFilename, ShaderType iType, char* acEntry,
    bool bQuiet, bool bForceVS2 )
{
    // Initialize data
    m_pkConsts = NULL;
    m_pkShader = NULL;
    m_iVertexRegHint = VertexShader::NONE;
    m_iNormalRegHint = VertexShader::NONE;
    m_iColorRegHint = VertexShader::NONE;
    m_bQuiet = bQuiet;
    m_bForceVS2 = bForceVS2;

    // We're going to pass this as a compiler option to all DX compiles
    // This is because DX9 requires this option.
    const char * aacArgs[] = { "-profileopts dcls", "-strict", 0 };

    for (int i = 0; i < 8; i++)
    {
        m_aiTexCoordRegHint[i] = VertexShader::NONE;
    }

    // find entry point
    if ( !acEntry )
    {
        // none defined, guess at something
        if ( iType == VERTEXSHADER )
        {
            int iLen = (int)strlen("vmain")+1;
            acEntry = new char[iLen];
            strcpy(acEntry,"vmain");
        }
        else if ( iType == PIXELSHADER )
        {
            int iLen = (int)strlen("pmain")+1;
            acEntry = new char[iLen];
            strcpy(acEntry,"pmain");
        }
        else
        {
            FatalError( "Invalid shader type." );
        }
    }

    // create context
    m_kContext = cgCreateContext();
    if (!cgIsContext( m_kContext) )
    {
        FatalError( "Could not create CG context." );
    }

    DbgPrintf("Parameter Listing:\n");

    // create the shader
    if ( iType == VERTEXSHADER )
    {
        // try to compile program with most lenient profile
        // using VS_2_X will not limit registers/instructions, 
        // will not allow GL state in the code
        m_kProgram = cgCreateProgramFromFile(m_kContext, CG_SOURCE, 
            acFilename, CG_PROFILE_VS_2_X, acEntry, aacArgs );

        // make sure it compiled
        if (!cgIsProgram( m_kProgram ) )
        {
            DbgPrintf( "***WARNING: Could not compile under profile "\
                "VS_2_X (trying ARBVP1). %s..\n", 
                cgGetErrorString(cgGetError()));

            cgDestroyProgram(m_kProgram);

            m_kProgram = cgCreateProgramFromFile(m_kContext, CG_SOURCE, 
                acFilename, CG_PROFILE_ARBVP1, acEntry, NULL );

            if (!cgIsProgram( m_kProgram ) )
            {
                DbgPrintf( "***WARNING: Could not compile under profile "\
                    "ARBVP1 either. %s..\n", 
                    cgGetErrorString(cgGetError()));

                FatalError( "Compile errors.  Cannot compile under any "\
                    "profile." );            }
        }

        // build constants
        CreateShaderConstants();

        DbgPrintf("Compiled Program Listing:\n");

        // make sure that if texture coordinate i is given, that
        // texture coordinates 0-(i-1) are also given.
        for (int iMaxTexCoord = 7; iMaxTexCoord >=0; iMaxTexCoord--)
        {
            if ( m_aiTexCoordRegHint[iMaxTexCoord] != VertexShader::NONE )
                break;
        }

        for (int k = 0; k < iMaxTexCoord; k++)
        {
            // due to the way shaders/wild magic works the texture coordinates
            // must all fall consecutively in a range 0-n, where 0 <= n <= 7.
            if ( m_aiTexCoordRegHint[k] == VertexShader::NONE )
            {
                FatalError ("Must use sequential tex coords." );
            }
        }

        m_pkShader = new VertexShader( m_pkConsts, 
            m_iVertexRegHint, m_iNormalRegHint, m_iColorRegHint,
            m_aiTexCoordRegHint[0], m_aiTexCoordRegHint[1],
            m_aiTexCoordRegHint[2], m_aiTexCoordRegHint[3],
            m_aiTexCoordRegHint[4], m_aiTexCoordRegHint[5],
            m_aiTexCoordRegHint[6], m_aiTexCoordRegHint[7] );
        m_pkConsts = NULL;

        cgDestroyProgram( m_kProgram );

        // Now, compile the program and find the minimal version that it
        // will run on under each API.

        // OpenGL has only one version
        if (TryVersion(CG_PROFILE_ARBVP1, acFilename, acEntry, NULL))
        {
            m_pkShader->AddProgram (Shader::OPENGL, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM), 
                (Shader::ShaderVersion)Shader::ARBVP1 );
            DbgPrintf( "\tOPENGL-ARBVP1 profile created\n");
        }
        else
        {
            DbgPrintf( "\t***Warning!!! Program will not work under OPENGL.\n" );
        }

        cgDestroyProgram( m_kProgram );

        // DirectX has a few possible versions
        if (!m_bForceVS2 && TryVersion(CG_PROFILE_VS_1_1, acFilename, acEntry, 
            aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::VS_1_1 );
            DbgPrintf( "\tDX-VS 1.1 profile created\n");
        }
        else if (TryVersion(CG_PROFILE_VS_2_0, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::VS_2_0 );
            DbgPrintf( "\tDX-VS 2.0 profile created\n");
        }
        else if (TryVersion(CG_PROFILE_VS_2_X, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::VS_2_X );
            DbgPrintf( "\t***Warning: VS 2.X profiles may not work with "\
                "every graphics card.\n" );
            DbgPrintf( "\tDX-VS 2.X profile created\n");
        }
        else
        {
            DbgPrintf( "\t***Warning!!! Program will not work under DX.\n" );
        }

        cgDestroyProgram( m_kProgram );
    }
    else // if ( iType == PIXELSHADER )
    {
        // try to compile program with most lenient profile
        // using PS_2_X will not limit registers/instructions, 
        // will not allow GL state in the code
        m_kProgram = cgCreateProgramFromFile(m_kContext, CG_SOURCE, 
            acFilename, CG_PROFILE_PS_2_X, acEntry, aacArgs );

        // make sure it compiled
        if (!cgIsProgram( m_kProgram ) )
        {
            DbgPrintf( "***WARNING: Could not compile under profile "\
                "PS_2_X (trying ARBFP1). %s..\n", 
                cgGetErrorString(cgGetError()));

            cgDestroyProgram(m_kProgram);

            m_kProgram = cgCreateProgramFromFile(m_kContext, CG_SOURCE, 
                acFilename, CG_PROFILE_ARBFP1, acEntry, NULL );

            if (!cgIsProgram( m_kProgram ) )
            {
                DbgPrintf( "***WARNING: Could not compile under profile "\
                    "ARBFP1 either. %s..\n", 
                    cgGetErrorString(cgGetError()));
                FatalError( "Compile errors.  Cannot compile under any "\
                    "profile." );
            }
        }
        // build constants
        CreateShaderConstants();

        DbgPrintf("Compiled Program Listing:\n");

        m_pkShader = new PixelShader(m_pkConsts);
        m_pkConsts = NULL;

        // Now, compile the program and find the minimal version that it
        // will run on under each API.

        // OpenGL has only one version
        if (TryVersion(CG_PROFILE_ARBFP1, acFilename, acEntry, NULL))
        {
            m_pkShader->AddProgram (Shader::OPENGL, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM), 
                (Shader::ShaderVersion)Shader::ARBFP1 );
            DbgPrintf( "\tOPENGL-ARBFP1 profile created\n");
        }
        else
        {
            DbgPrintf( "\t***Warning!!! Program will not work under OPENGL.\n" );
        }

        cgDestroyProgram( m_kProgram );

        // DirectX has a few possible versions
        
        if (TryVersion(CG_PROFILE_PS_1_1, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::PS_1_1 );
            DbgPrintf( "\tDX-PS 1.1 profile created\n");
        }
        else if (TryVersion(CG_PROFILE_PS_1_2, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::PS_1_2 );
            DbgPrintf( "\tDX-PS 1.2 profile created\n");
        }
        else if (TryVersion(CG_PROFILE_PS_1_3, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::PS_1_3 );
            DbgPrintf( "\tDX-PS 1.3 profile created\n");
        }
        else if (TryVersion(CG_PROFILE_PS_2_0, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::PS_2_0 );
            DbgPrintf( "\tDX-PS 2.0 profile created\n");
        }
        else if (TryVersion(CG_PROFILE_PS_2_X, acFilename, acEntry, aacArgs))
        {
            m_pkShader->AddProgram (Shader::DIRECTX, 
                cgGetProgramString(m_kProgram, CG_COMPILED_PROGRAM),
                Shader::PS_2_X );
            DbgPrintf( "\t***Warning: PS 2.X profiles may not work with "\
                "every graphics card.\n" );
            DbgPrintf( "\tDX-PS 2.X profile created\n");
        }
        else
        {
            DbgPrintf( "\t***Warning!!! Program will not work under DX.\n" );
        }

        cgDestroyProgram( m_kProgram );
    }

    DbgPrintf( "\n");
}
//----------------------------------------------------------------------------
CgConverter::~CgConverter ()
{
    if ( cgIsContext(m_kContext) )
    {
        cgDestroyContext(m_kContext);
    }

    if ( cgIsProgram(m_kProgram) )
    {
        cgDestroyProgram(m_kProgram);
    }

    if ( m_pkShader )
        delete m_pkShader;

    if ( m_pkConsts )
        delete m_pkConsts;
}
//----------------------------------------------------------------------------
void CgConverter::CreateShaderConstants ()
{
    if ( m_pkConsts )
        delete m_pkConsts;
    
    m_pkConsts = new ShaderConstants();
    RecurseProgramParameters(cgGetFirstParameter(m_kProgram, CG_PROGRAM));

    DbgPrintf( "Total User/State Constants: %i\n", 
        m_pkConsts->GetNumConstants() );
}
//----------------------------------------------------------------------------
void CgConverter::RecurseProgramParameters(CGparameter kParam)
{
    if (kParam == 0)
        return;

    int i, arraySize;

    do
    {
        switch(cgGetParameterType(kParam))
        {
            case CG_STRUCT:
                RecurseProgramParameters(cgGetFirstStructParameter(kParam));
                break;
            case CG_ARRAY:
                arraySize = cgGetArraySize(kParam, 0);
                for (i = 0; i < arraySize; ++i)
                {
                    RecurseProgramParameters(cgGetArrayParameter(kParam, i));
                }
                break;
            default:
                // handle the single parameter

                // sanity check
                if (!cgIsParameter(kParam))
                {
                    FatalError("Not a parameter.");
                }
                // make sure it is actually used in the program
                if ( cgIsParameterReferenced(kParam) )
                {
                    AddParam( kParam );
                }
                break;
        }
    } while((kParam = cgGetNextParameter(kParam))!= 0);
}

//----------------------------------------------------------------------------
void CgConverter::AddParam (CGparameter kParam)
{
    CGresource kResource;
    const char* acName = cgGetParameterName(kParam);

    int iTemp, iTemp2, iRegister, iSize, iTypeOption;
    StateConstantType iConstType = USER_DEFINED;

    // We don't need to handle out parameters (at this point)
    // DX and GL handle both of these automatically
    if ( cgGetParameterDirection(kParam) == CG_OUT )
    {
        DbgPrintf( "\tOutput (%s), ignored\n", acName );
        return;
    }

    switch (cgGetParameterVariability(kParam))
    {
        case CG_VARYING:
            // update the 
            DbgPrintf( "\tVarying (%s), ", acName );

            kResource = cgGetParameterBaseResource(kParam);

            if ( kResource == CG_TEXCOORD0 )
            {
                iTemp = cgGetParameterResourceIndex(kParam);

                iRegister = GetRegister(kParam);
                m_aiTexCoordRegHint[iTemp] = iRegister;
                DbgPrintf( "TEXCOORD%i (v%i)\n", iTemp, iRegister );
            }
            else if ( kResource == CG_POSITION0 )
            {
                iTemp = cgGetParameterResourceIndex(kParam);
                // only position 0 is accepted in WML
                if (iTemp)
                {
                    FatalError( "May only use Position0 or Position.");
                }

                iRegister = GetRegister(kParam);
                m_iVertexRegHint = iRegister;
                DbgPrintf( "POSITION (v%i)\n", iRegister );
            }
            else if ( kResource == CG_NORMAL0 )
            {
                iTemp = cgGetParameterResourceIndex(kParam);
                // only normal 0 is accepted in WML
                if (iTemp)
                {
                    FatalError( "May only use Normal0 or Normal.");
                }
                iRegister = GetRegister(kParam);
                m_iNormalRegHint = iRegister;
                DbgPrintf( "NORMAL (v%i)\n", iRegister );
            }
            else if ( kResource == CG_COLOR0 )
            {
                iTemp = cgGetParameterResourceIndex(kParam);
                // only color 0 is accepted in WML
                if (iTemp)
                {
                    FatalError( "May only use Color0 or Color.");
                }
                iRegister = GetRegister(kParam);
                m_iColorRegHint = iRegister;
                DbgPrintf( "COLOR (v%i)\n", iRegister );
            }
            else
            {
                DbgPrintf( "ignored\n" );
            }
 

            break;
        case CG_UNIFORM:
            DbgPrintf( "\tUniform (%s), ", acName );

            iRegister = cgGetParameterResourceIndex(kParam);

            iSize = GetSize( kParam );

            if (!iSize)
                return;

            if (!strncmp(acName, "Wml", 3))
            {
                MatchConstantName (acName, iConstType, iTypeOption);

                // you must pick the correct type (float4 or float4x4) for
                // state constants
                if ( StateConstant::Size(iConstType) != iSize )
                {
                    FatalError( "Wrong size for state constant." );
                }
                m_pkConsts->AddConstant( acName, iRegister, 
                    StateConstant::Size(iConstType), iConstType, 
                    iTypeOption);
                DbgPrintf( "state constant (c%i)\n", iRegister );
            }
            else
            {
                iConstType = USER_DEFINED;
                m_pkConsts->AddConstant( acName, iRegister, 
                    iSize, iConstType, 0);
                DbgPrintf( "user constant (c%i)\n", iRegister );

            }
 
            break;
        case CG_CONSTANT:
            DbgPrintf( "\tUniform (%s), ", acName );

            iRegister = cgGetParameterResourceIndex(kParam);
            iSize = GetSize( kParam );
            if (!iSize)
                return;

            iConstType = NUMERICAL_CONSTANT;
            m_pkConsts->AddConstant( acName, iRegister, 
                iSize, iConstType, 0, cgGetParameterValues(kParam,
                CG_CONSTANT, &iTemp2));

            DbgPrintf( "numerical constant (c%i) [", iRegister );
            for( iTemp = 0; iTemp < 4*iSize-1; iTemp++ )
            {
                DbgPrintf( "%.2f, ", (float)(cgGetParameterValues(kParam,
                    CG_CONSTANT, &iTemp2)[iTemp]) );
            }
            DbgPrintf( "%.2f", (float)(cgGetParameterValues(kParam,
                CG_CONSTANT, &iTemp2)[iTemp]) );

            DbgPrintf( "]\n");
            break;

        default:
            FatalError( "Unknown parameter type!\n" );
    }
}
//----------------------------------------------------------------------------
void CgConverter::MatchConstantName (const char* acName, 
    StateConstantType& riType, int& riTypeOption)
{
    int iTemp;

    for (int i = 0; i < StateConstant::NumTypes(); i++)
    {
        if ( !strncmp(acName, StateConstant::Name(i), 
            strlen(StateConstant::Name(i))) )
        {
            riType = (StateConstantType)i;
            switch( riType )
            {
                case RENDERER_MODVIEW:
                case RENDERER_PROJ:
                case RENDERER_MODVIEWPROJ:
                case RENDERER_MOD:
                    iTemp = (int)strlen(StateConstant::Name(i));
                    if ( strlen(acName) == iTemp )
                    {
                        riTypeOption = (int)SC_NORMAL;
                    }
                    else if (!strcmp(&acName[iTemp], "InvTrans"))
                    {
                        riTypeOption = (int)SC_INVERSETRANSPOSE;
                    }
                    else if (!strcmp(&acName[iTemp], "Inv"))
                    {
                        riTypeOption = (int)SC_INVERSE;
                    }
                    else if (!strcmp(&acName[iTemp], "Trans"))
                    {
                        riTypeOption = (int)SC_TRANSPOSE;
                    }
                    else
                    {
                        FatalError( "Invalid Suffix on Matrix type.\n" );
                    }
                    break;
                case LIGHT_POSITION:
                case LIGHT_DIRECTION:
                case LIGHT_AMBIENT:
                case LIGHT_DIFFUSE:
                case LIGHT_SPECULAR:
                case LIGHT_SPOTCUTOFF:
                case LIGHT_ATTENPARAMS:
                    if ( strlen(acName) == strlen(StateConstant::Name(i))+1 )
                    {
                        // lights have a single digit after them
                        // e.g. LIGHT_POSITION0 or LIGHT_AMBIENT7
                        riTypeOption = (int)acName[strlen(acName)-1]-(int)'0';
                        if ( ( riTypeOption < 0 ) || ( riTypeOption >=8 ) )
                        {
                            FatalError( "Illegal light num." );
                        }
                    }
                    else
                    {
                        FatalError( "Lights must have one digit"\
                            " in their name.\n" );
                     }
                    break;
                default:
                    if ( strlen(acName) == strlen(StateConstant::Name(i)) )
                    {
                        riTypeOption = 0;
                    }
                    else
                    {
                        FatalError( "Invalid trailing characters "\
                            "on constant." );
                    }
                    break;
            }

            // if found one, don't check anymore
            break;
        }
    }

    // if none found
    if ( i == StateConstant::NumTypes() )
    {
        // See list in StateConstant.{h, cpp} for valid constant names
        FatalError( "Invalid state constant name." );
    }
}
//----------------------------------------------------------------------------
int CgConverter::GetRegister (CGparameter kParam)
{
    return cgD3D9ResourceToDeclUsage(cgGetParameterResource(kParam));
}
//----------------------------------------------------------------------------
bool CgConverter::TryVersion (CGprofile kProfile, const char* acFilename,
    const char* acEntry, const char** aacArgs)
{
    if ( cgIsProgram(m_kProgram) )
        cgDestroyProgram(m_kProgram);

    m_kProgram = cgCreateProgramFromFile(m_kContext, CG_SOURCE, 
        acFilename, kProfile, acEntry, aacArgs );

    if (cgIsProgram(m_kProgram))
    {
        // Probably need to look for "// %i instructions" line
        return true;
    }
    else
        return false;
}
//----------------------------------------------------------------------------
void CgConverter::WriteObjectToFile (char* acFilename)
{
    m_pkShader->Write( acFilename );
}
//----------------------------------------------------------------------------
void CgConverter::DbgPrintf (char* acFormat, ...)
{
    if ( m_bQuiet )
        return;

    va_list kArgs;
    va_start(kArgs, acFormat);
    vprintf(acFormat,kArgs);
    va_end(kArgs);
}
//----------------------------------------------------------------------------
void CgConverter::FatalError (const char* acMessage)
{
    printf( "\n***ERROR: " );
    printf( acMessage );
    printf( "\n" );
    exit(-1);
}
//----------------------------------------------------------------------------
int CgConverter::GetSize (CGparameter kParam)
{
    CGtype kType = cgGetParameterType(kParam);
    switch( kType )
    {
        case CG_FLOAT:
        case CG_FLOAT2:
        case CG_FLOAT3:
        case CG_FLOAT4:
            return 1;
        case CG_FLOAT3x3:
            return 3;
        case CG_FLOAT4x4:
            return 4;
        case CG_SAMPLER1D:
        case CG_SAMPLER2D:
        case CG_SAMPLER3D:
        case CG_SAMPLERRECT:
        case CG_SAMPLERCUBE:
            // types not handled by the converter, should be ignored
            return 0;
            break;
        default:
            FatalError( "Type not handled by WML (sorry)!\n" );
            return 0;
            break;
    }
}
//----------------------------------------------------------------------------