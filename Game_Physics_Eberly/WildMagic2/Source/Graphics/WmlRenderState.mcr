// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRENDERSTATE_MCR
#define WMLRENDERSTATE_MCR

#include <cstddef>

#define WmlDeclareDefaultState(classname) \
    public: \
        static classname* GetDefault () { return ms_spkDefault; } \
        static void Initialize () \
        { \
            ms_spkDefault = new classname; \
            Wml::RenderState::Type eType = ms_spkDefault->GetType(); \
            ms_apkDefault[eType] = ms_spkDefault; \
        } \
        static void Terminate () \
        { \
            ms_spkDefault = NULL; \
        } \
    protected: \
        static classname##Ptr ms_spkDefault; \
        friend class _##classname##InitTermDS

#define WmlImplementDefaultState(classname) \
    classname##Ptr classname::ms_spkDefault = NULL; \
    class _##classname##InitTermDS \
    { \
    public: \
        _##classname##InitTermDS () { classname::Initialize(); } \
        ~_##classname##InitTermDS () {  classname::Terminate(); } \
    }; \
    static ::_##classname##InitTermDS _force##classname##InitTermDS

#define WmlRenderStateCast(classname,spState) \
    (classname*)(RenderState*)spState

#endif
