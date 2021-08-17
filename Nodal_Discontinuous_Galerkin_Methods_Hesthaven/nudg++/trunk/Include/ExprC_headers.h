// exprc_headers.h
//
// 2007/01/18
//---------------------------------------------------------
#ifndef NDG__Exprc_headers_H__INCLUDED
#define NDG__Exprc_headers_H__INCLUDED

//#define WIN32_LEAN_AND_MEAN
//#define NO_MIN_MAX

// adjust trace output
#undef VERBOSEPARSE
#undef VERBOSEEVALUATE
#undef VERBOSETOKENIZE

#ifdef _DEBUG
//#define VERBOSEPARSE
//#define VERBOSEEVALUATE
//#define VERBOSETOKENIZE
#endif


#include <cctype>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>
#include <cassert>

#include <iostream>
#include <fstream>

#include "Mat_COL.h"
#include "ExprC/ExpCmp.h"
#include "ExprC/ExpCmpI.h"

#endif  // NDG__Exprc_headers_H__INCLUDED
