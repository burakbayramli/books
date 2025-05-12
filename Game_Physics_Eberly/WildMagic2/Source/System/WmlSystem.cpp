// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
void System::SwapBytes (int iSize, void* pvValue)
{
    assert( iSize >= 1 );
    if ( iSize == 1 )
        return;

    // size must be even
    assert( (iSize & 1) == 0 );

    char* acBytes = (char*) pvValue;
    for (int i0 = 0, i1 = iSize-1; i0 < iSize/2; i0++, i1--)
    {
        char cSave = acBytes[i0];
        acBytes[i0] = acBytes[i1];
        acBytes[i1] = cSave;
    }
}
//----------------------------------------------------------------------------
void System::SwapBytes (int iSize, int iQuantity, void* pvValue)
{
    assert( iSize >= 1 );
    if ( iSize == 1 )
        return;

    // size must be even
    assert( (iSize & 1) == 0 );

    char* acBytes = (char*) pvValue;
    for (int i = 0; i < iQuantity; i++, acBytes += iSize)
    {
        for (int i0 = 0, i1 = iSize-1; i0 < iSize/2; i0++, i1--)
        {
            char cSave = acBytes[i0];
            acBytes[i0] = acBytes[i1];
            acBytes[i1] = cSave;
        }
    }
}
//----------------------------------------------------------------------------
bool System::IsPowerOfTwo (int iValue)
{
    return (iValue != 0) && ((iValue & -iValue) == iValue);
}
//----------------------------------------------------------------------------
bool System::FileExists (const char* acFilename)
{
    FILE* pkFile = fopen(acFilename,"r");
    if ( pkFile )
    {
        fclose(pkFile);
        return true;
    }
    return false;
}
//----------------------------------------------------------------------------
