// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSYSTEM_H
#define WMLSYSTEM_H

#if defined(WIN32)
#include "WmlWinSystem.h"
#elif defined(__MACOS__)
#include "WmlMacSystem.h"
#else
#include "WmlLnxSystem.h"
#endif

namespace Wml
{

class WML_ITEM System
{
public:
    // little/big endian support
    static void SwapBytes (int iSize, void* pvValue);
    static void SwapBytes (int iSize, int iQuantity, void* pvValue);
    static void EndianCopy (int iSize, const void* pvSrc, void* pvDst);
    static void EndianCopy (int iSize, int iQuantity, const void* pvSrc,
        void* pvDst);

    // time utilities
    static double GetTime ();

    // TO DO.  Pathname handling to access files in subdirectories.
    static bool FileExists (const char* acFilename);

    // convenient utilities
    static bool IsPowerOfTwo (int iValue);
};

// allocation and deallocation of 2D arrays
template <class T> void Allocate2D (int iCols, int iRows, T**& raatArray);
template <class T> void Deallocate2D (T** aatArray);

// allocation and deallocation of 3D arrays
template <class T> void Allocate3D (int iCols, int iRows, int iSlices,
    T***& raaatArray);
template <class T> void Deallocate3D (int iRows, int iSlices,
    T*** aaatArray);

#include "WmlSystem.inl"

}

#endif

