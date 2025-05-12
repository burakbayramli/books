// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2002.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.wild-magic.com/License/WildMagic.pdf and
// may not be copied or disclosed except in accordance with the terms of that
// agreement.

#include "WmlSystem.h"
using namespace Wml;

//----------------------------------------------------------------------------
void System::EndianCopy (int iSize, const void* pvSrc, void* pvDst)
{
    memcpy(pvDst,pvSrc,iSize);
}
//----------------------------------------------------------------------------
void System::EndianCopy (int iSize, int iQuantity, const void* pvSrc,
    void* pvDst)
{
    memcpy(pvDst,pvSrc,iSize*iQuantity);
}
//----------------------------------------------------------------------------
