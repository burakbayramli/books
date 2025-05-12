// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLSTREAM_MCR
#define WMLSTREAM_MCR

// insert in Object class declaration
#define WmlDeclareRootStream \
public: \
    static std::map<std::string,FactoryFunction>* ms_pkFactory; \
    static int RegisterFactory (); \
    static Object* Factory (Stream& rkStream); \
    virtual void Load (Stream& rkStream, Stream::Link* pkLink); \
    virtual void Link (Stream& rkStream, Stream::Link* pkLink); \
    virtual bool Register (Stream& rkStream); \
    virtual void Save (Stream& rkStream); \
    virtual StringTree* SaveStrings (); \
    virtual int GetMemoryUsed () const; \
    virtual int GetDiskUsed () const; \
    static void Initialize () \
    { \
        if ( !ms_pkFactory ) \
        { \
            ms_pkFactory = new std::map<std::string,FactoryFunction>; \
        } \
        const std::string kString("MgcObject"); \
        (*ms_pkFactory)[kString] = (FactoryFunction)Factory; \
    } \
    static void Terminate () \
    { \
        delete ms_pkFactory; \
        ms_pkFactory = NULL; \
    } \
    friend class _ObjectInitTerm

// insert in source file of Object class
#define WmlImplementRootStream \
std::map<std::string,Wml::FactoryFunction>* Wml::Object::ms_pkFactory = 0; \
class _ObjectInitTerm \
{ \
public: \
    _ObjectInitTerm () \
    { \
        Object::Initialize(); \
    } \
    ~_ObjectInitTerm () \
    { \
        Object::Terminate(); \
    } \
}; \
static ::_ObjectInitTerm _forceObjectInitTerm

// insert in derived class declaration
#define WmlDeclareStream \
public: \
    static int RegisterFactory (); \
    static Wml::Object* Factory (Wml::Stream& rkStream); \
    virtual void Load (Wml::Stream& rkStream, Wml::Stream::Link* pkLink); \
    virtual void Link (Wml::Stream& rkStream, Wml::Stream::Link* pkLink); \
    virtual bool Register (Wml::Stream& rkStream); \
    virtual void Save (Wml::Stream& rkStream); \
    virtual Wml::StringTree* SaveStrings (); \
    virtual int GetMemoryUsed () const; \
    virtual int GetDiskUsed () const

// insert in source file of derived class of inheritance tree
#define WmlImplementStream(classname) \
int classname::RegisterFactory () \
{ \
    if ( !ms_pkFactory ) \
    { \
        ms_pkFactory = new std::map<std::string,Wml::FactoryFunction>; \
    } \
    const std::string kString = std::string("Mgc")+std::string(#classname); \
    (*ms_pkFactory)[kString] = (Wml::FactoryFunction)classname::Factory; \
    return 0; \
}

// Insert in derived class header file.  Forces pre-main registration of
// factory whenever the header file is included in an application source file.
#define WmlRegisterStream(classname) \
static int _##classname##registerFactory = classname::RegisterFactory ()

#endif
