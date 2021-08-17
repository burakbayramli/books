// NDG_headers.h
// 
// 2006/12/15
//---------------------------------------------------------
#ifndef NDG__headers_H__INCLUDED
#define NDG__headers_H__INCLUDED


#include "LOG_funcs.h"

// forward declarations
bool InitGlobalInfo();
void FreeGlobalInfo();

#ifdef WIN32
// process management routines
#include <Windows.h>
// clear {min,max} macros
#undef min
#undef max
#endif

#endif  // NDG__headers_H__INCLUDED
