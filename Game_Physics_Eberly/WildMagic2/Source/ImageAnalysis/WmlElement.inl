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
inline unsigned short GetColor16 (unsigned char ucR, unsigned char ucG,
    unsigned char ucB)
{
    return (unsigned short)(( unsigned(ucB) >> 3 ) 
        |  ((unsigned(ucG) >> 3 ) << 5 ) 
        |  ((unsigned(ucR) >> 3 ) << 10));
}
//----------------------------------------------------------------------------
inline unsigned char GetBlue16 (unsigned short usColor)
{ 
    return (unsigned char)(usColor & 0x001F); 
}
//----------------------------------------------------------------------------
inline unsigned char GetGreen16 (unsigned short usColor)
{ 
    return (unsigned char)((usColor & 0x03E0) >> 5); 
}
//----------------------------------------------------------------------------
inline unsigned char GetRed16 (unsigned short usColor)
{ 
    return (unsigned char)((usColor & 0x7C00) >> 10);
}
//----------------------------------------------------------------------------
inline unsigned int GetColor24 (unsigned char ucR, unsigned char ucG,
    unsigned char ucB)
{
    return ( unsigned(ucB) )
        |  ((unsigned(ucG)) << 8)
        |  ((unsigned(ucR)) << 16);
}
//----------------------------------------------------------------------------
inline unsigned char GetBlue24 (unsigned int uiColor)
{ 
    return (unsigned char)(uiColor & 0x000000FF); 
}
//----------------------------------------------------------------------------
inline unsigned char GetGreen24 (unsigned int uiColor)
{ 
    return (unsigned char)((uiColor & 0x0000FF00) >> 8); 
}
//----------------------------------------------------------------------------
inline unsigned char GetRed24 (unsigned int uiColor)
{ 
    return (unsigned char)((uiColor & 0x00FF0000) >> 16);
}
//----------------------------------------------------------------------------
