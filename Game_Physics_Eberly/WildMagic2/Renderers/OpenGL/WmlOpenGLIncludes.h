// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Wild Magic Library (WML) source code is supplied under the terms of
// the license agreement http://www.magic-software.com/License/WildMagic.pdf
// and may not be copied or disclosed except in accordance with the terms of
// that agreement.

#ifndef WMLOPENGLINCLUDES_H
#define WMLOPENGLINCLUDES_H

// GLUT
#if defined(WML_USE_GLUT)

#ifdef __MACOS__
#include <GLUT/glut.h>
#include <OpenGL/glext.h>
#else
#include <GL/glut.h>
#include <GL/glprocs.h>
#endif


// Microsoft Windows
#elif defined(WML_USE_WGL)

#include <windows.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glprocs.h>


// Macintosh
#elif defined(WML_USE_AGL)

#if __MACH__
#include <AGL/agl.h>
#include <OpenGL/glu.h>
#else
#include <agl.h>
#include <glu.h>
#endif
#include <OpenGL/glext.h>

#else
#error WML_USE_GLUT or WML_USE_WGL or WML_USE_AGL must be defined.
#endif

#endif
