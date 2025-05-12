// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#include "WmlVersion.h"
using namespace Wml;

const int Version::MAJOR = 1;
const int Version::MINOR = 7;
const char Version::CURRENT[] = "Magic3D Version 1.07";
const int Version::LENGTH = 21;

//----------------------------------------------------------------------------
Version::Version (int iMajorVersion, int iMinorVersion)
{
    m_iMajor = iMajorVersion;
    m_iMinor = iMinorVersion;
}
//----------------------------------------------------------------------------
Version::Version (const char* acString)
{
    m_iMajor = -1;
    m_iMinor = -1;

    if ( acString
    &&   strlen(acString) >= LENGTH-1
    &&   acString[LENGTH-1] == 0
    &&   strncmp(acString,CURRENT,LENGTH-5) == 0 )
    {
        int iArgs = sscanf(acString+LENGTH-5,"%1d.%2d",&m_iMajor,&m_iMinor);
        if ( iArgs != 2 )
        {
            m_iMajor = -1;
            m_iMinor = -1;
        }
    }
}
//----------------------------------------------------------------------------
bool Version::IsValid () const
{
    return 0 <  m_iMajor && m_iMajor <  10
        && 0 <= m_iMinor && m_iMinor < 100;
}
//----------------------------------------------------------------------------
int Version::GetMajor () const
{
    return m_iMajor;
}
//----------------------------------------------------------------------------
int Version::GetMinor () const
{
    return m_iMinor;
}
//----------------------------------------------------------------------------
int Version::GetCombined () const
{
    return 100*m_iMajor + m_iMinor;
}
//----------------------------------------------------------------------------
bool Version::operator== (const Version& rkVersion) const
{
    assert( IsValid() && rkVersion.IsValid() );
    return GetCombined() == rkVersion.GetCombined();
}
//----------------------------------------------------------------------------
bool Version::operator!= (const Version& rkVersion) const
{
    assert( IsValid() && rkVersion.IsValid() );
    return GetCombined() != rkVersion.GetCombined();
}
//----------------------------------------------------------------------------
bool Version::operator< (const Version& rkVersion) const
{
    assert( IsValid() && rkVersion.IsValid() );
    return GetCombined() < rkVersion.GetCombined();
}
//----------------------------------------------------------------------------
bool Version::operator<= (const Version& rkVersion) const
{
    assert( IsValid() && rkVersion.IsValid() );
    return GetCombined() <= rkVersion.GetCombined();
}
//----------------------------------------------------------------------------
bool Version::operator> (const Version& rkVersion) const
{
    assert( IsValid() && rkVersion.IsValid() );
    return GetCombined() > rkVersion.GetCombined();
}
//----------------------------------------------------------------------------
bool Version::operator>= (const Version& rkVersion) const
{
    assert( IsValid() && rkVersion.IsValid() );
    return GetCombined() >= rkVersion.GetCombined();
}
//----------------------------------------------------------------------------


