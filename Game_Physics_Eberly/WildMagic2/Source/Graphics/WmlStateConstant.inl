// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

//----------------------------------------------------------------------------
inline int StateConstant::Size (int i)
{
    return ms_acSizes[i];
}
//----------------------------------------------------------------------------
inline int StateConstant::Size (StateConstantType iType)
{
    return ms_acSizes[iType];
}
//----------------------------------------------------------------------------
inline const char* StateConstant::Name (int i)
{
    return ms_aacNames[i];
}
//----------------------------------------------------------------------------
inline const char* StateConstant::Name (StateConstantType iType)
{
    return ms_aacNames[iType];
}
//----------------------------------------------------------------------------
inline int StateConstant::NumTypes ()
{
    return ms_iNumTypes;
}
//----------------------------------------------------------------------------
inline bool StateConstant::AllowedInPixelShader (int i)
{
    return ms_abOKInPixelShader[i];
}
//----------------------------------------------------------------------------
