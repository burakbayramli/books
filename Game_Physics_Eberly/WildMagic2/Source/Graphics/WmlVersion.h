// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLVERSION_H
#define WMLVERSION_H

#include "WmlSystem.h"

// File version information.  The string stored in a file is of the form
// "Magic3D Version x.yy" where the major version is x in [1,9], the minor
// version is y in [00,99].  The length of the string is 20, but the null
// terminator is written to disk, so total number of file bytes used by the
// version is 21.  The current version is "1.07"

namespace Wml
{

class WML_ITEM Version
{
public:
    static const int MAJOR;       // 1
    static const int MINOR;       // 5
    static const char CURRENT[];  // Magic3D Version 1.07"
    static const int LENGTH;      // 21 = strlen(CURRENT)+1

    Version (int iMajor = -1, int iMinor = -1);
    Version (const char* acString);

    int GetMajor () const;
    int GetMinor () const;

    // The version is valid if major in [0,9] and minor in [0,99].
    bool IsValid () const;

    // For comparisons of versions.  This is useful in the Stream support in
    // an Object-derived class whenever a change to that class causes a file
    // format change.
    bool operator== (const Version& rkVersion) const;
    bool operator!= (const Version& rkVersion) const;
    bool operator<  (const Version& rkVersion) const;
    bool operator<= (const Version& rkVersion) const;
    bool operator>  (const Version& rkVersion) const;
    bool operator>= (const Version& rkVersion) const;

protected:
    int GetCombined () const;  // 100*major + minor

    int m_iMajor, m_iMinor;
};

}

#endif

