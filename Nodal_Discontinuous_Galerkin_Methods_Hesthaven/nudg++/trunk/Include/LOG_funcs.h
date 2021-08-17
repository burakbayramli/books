// LOG_funcs.h
// declare log and message routines
// 2006/10/15
//---------------------------------------------------------
#ifndef NDG__umLOG_funcs__INCLUDED
#define NDG__umLOG_funcs__INCLUDED

#include <string>
#include <cstdio>

void  umLOG(int n, const char* format_str, ...);
void  umMSG(int n, const char* format_str, ...);
void  umTRC(int n, const char* format_str, ...);

void  umLOG(const std::string& msg, int n=0);
void  umMSG(const std::string& msg, int n=0);
void  umTRC(const std::string& msg, int n=0);

void  umWARNING(char* function_name, ...);
void  umERROR(char* function_name, ...);
void  umQUIT();

char* umOFORM(const char* fmt, ...);

std::string trim(const char* sz);

extern FILE* g_TRCFile;

#endif  // NDG__umLOG_funcs__INCLUDED
