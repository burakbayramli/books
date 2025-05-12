// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSMARTPOINTER_MCR
#define WMLSMARTPOINTER_MCR

// insert in Object class declaration
#define WmlDeclareRootSmartPointer \
public: \
    void IncrementReferences () \
    { \
        m_uiReferences++; \
    } \
    \
    void DecrementReferences () \
    { \
        if ( --m_uiReferences == 0 ) \
            delete this; \
    } \
    \
    unsigned int GetReferences () const \
    { \
        return m_uiReferences; \
    } \
    \
private: \
    unsigned int m_uiReferences


// convenient naming for smart pointer classes
#define WmlSmartPointer(classname) \
    class classname; \
    typedef Wml::Pointer<classname> classname##Ptr

// cast a smart pointer of one type to a pointer of another type.
#define WmlSmartPointerCast(type,smartptr) ((type*)(void*)(smartptr))

#endif
