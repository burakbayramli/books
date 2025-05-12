// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLELEMENT_H
#define WMLELEMENT_H

#include "WmlSystem.h"

// wrappers for native types
const int WML_ELEMENT_QUANTITY = 12;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
typedef unsigned short rgb5;
typedef unsigned int rgb8;

#define WmlDeclareElement(T) \
class WML_ITEM E##T \
{ \
public: \
    E##T (T tValue = 0) { m_tValue = tValue; } \
    \
    E##T& operator= (E##T kElement) \
    { \
        m_tValue = kElement.m_tValue; \
        return *this; \
    } \
    \
    operator T () { return m_tValue; } \
    \
    static int GetRTTI () { return ms_iRTTI; } \
    \
protected: \
    T m_tValue; \
    static const int ms_iRTTI; \
}

#define WmlImplementElement(T,iRTTI) \
const int E##T::ms_iRTTI = iRTTI

namespace Wml
{

WmlDeclareElement(char);
WmlDeclareElement(uchar);
WmlDeclareElement(short);
WmlDeclareElement(ushort);
WmlDeclareElement(int);
WmlDeclareElement(uint);
WmlDeclareElement(long);
WmlDeclareElement(ulong);
WmlDeclareElement(float);
WmlDeclareElement(double);
WmlDeclareElement(rgb5);
WmlDeclareElement(rgb8);

#include "WmlElement.inl"

}

#endif
