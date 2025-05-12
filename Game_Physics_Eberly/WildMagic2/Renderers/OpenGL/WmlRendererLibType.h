// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLRENDERERLIBTYPE_H
#define WMLRENDERERLIBTYPE_H

// For the DLL library.
#ifdef WML_RENDERER_DLL_EXPORT
#define WML_RENDERER_ITEM __declspec(dllexport)

// For a client of the DLL library.
#else
#ifdef WML_RENDERER_DLL_IMPORT
#define WML_RENDERER_ITEM __declspec(dllimport)

// For the static library.
#else
#define WML_RENDERER_ITEM

#endif
#endif
#endif
