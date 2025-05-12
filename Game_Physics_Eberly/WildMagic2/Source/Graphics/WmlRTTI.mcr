// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRTTI_MCR
#define WMLRTTI_MCR

#include <cstddef>

// insert in Object class declaration
#define WmlDeclareRootRTTI \
public: \
    static const Wml::RTTI ms_kRTTI; \
    virtual const Wml::RTTI* GetRTTI () const; \
    bool IsExactlyClass (const Wml::RTTI* pkQueryRTTI) const; \
    bool IsDerivedFromClass (const Wml::RTTI* pkQueryRTTI) const; \
    void* DynamicCast (const Wml::RTTI* pkQueryRTTI) const

// insert in source file of Object class
#define WmlImplementRootRTTI \
const Wml::RTTI Wml::Object::ms_kRTTI("Object",NULL); \
const Wml::RTTI* Wml::Object::GetRTTI () const \
{ \
    return &ms_kRTTI; \
} \
\
bool Wml::Object::IsExactlyClass (const Wml::RTTI* pkQueryRTTI) const \
{ \
    return ( GetRTTI() == pkQueryRTTI ); \
} \
\
bool Wml::Object::IsDerivedFromClass (const Wml::RTTI* pkQueryRTTI) const \
{ \
    const Wml::RTTI* pkRTTI = GetRTTI(); \
    while ( pkRTTI ) \
    { \
        if ( pkRTTI == pkQueryRTTI ) \
            return true; \
        pkRTTI = pkRTTI->GetBaseRTTI(); \
    } \
    return false; \
} \
\
void* Wml::Object::DynamicCast (const Wml::RTTI* pkQueryRTTI) const \
{ \
    return (void*)( IsDerivedFromClass(pkQueryRTTI) ? this : NULL ); \
}

// insert in derived class declaration
#define WmlDeclareRTTI \
public: \
    static const Wml::RTTI ms_kRTTI; \
    virtual const Wml::RTTI* GetRTTI () const { return &ms_kRTTI; }

// insert in source file of derived class of inheritance tree
#define WmlImplementRTTI(classname,baseclassname) \
const Wml::RTTI classname::ms_kRTTI(#classname,&baseclassname::ms_kRTTI)

// runtime type testing and casting
#define WmlIsExactlyClass(classname,pObj) \
( (pObj) ? (pObj)->IsExactlyClass(&classname::ms_kRTTI) : false )

#define WmlIsDerivedFromClass(classname,pObj) \
( (pObj) ? (pObj)->IsDerivedFromClass(&classname::ms_kRTTI) : false )

#define WmlStaticCast(classname,pObj) \
((classname*)(void*)(pObj))

#define WmlDynamicCast(classname,pObj) \
( (pObj) ? (classname*)(pObj)->DynamicCast(&classname::ms_kRTTI) : NULL )

#endif


