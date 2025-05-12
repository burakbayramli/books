// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRTTI_H
#define WMLRTTI_H

#include "WmlSystem.h"
#include "WmlRTTI.mcr"
#include <string>

namespace Wml
{

class WML_ITEM RTTI
{
public:
    RTTI (const char* acName, const RTTI* pkBaseRTTI)
    {
        // TO DO.  Appending "Mgc" keeps the file version at 1.00.  When
        // the prefix is removed, the file version must change and a
        // converter should be supplied.
        m_kName = "Mgc"+std::string(acName);
        m_pkBaseRTTI = pkBaseRTTI;
    }

    const RTTI* GetBaseRTTI () const
    {
        return m_pkBaseRTTI;
    }

    const char* GetName () const
    {
        return m_kName.c_str();
    }

private:
    const RTTI* m_pkBaseRTTI;
    std::string m_kName;
};

}

#endif
