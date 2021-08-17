// LOG.h
//
// 2006/10/15
//---------------------------------------------------------
#ifndef NDG__LOG__INCLUDED
#define NDG__LOG__INCLUDED


// declare log and message routines
#include "LOG_funcs.h"


//---------------------------------------------------------
// Global log targets
//---------------------------------------------------------

// Redirect messages file in console mode
extern FILE* g_LOGFile;
extern FILE* g_MSGFile;
extern FILE* g_TRCFile;

// Clear message windows (cf: Matlab's "clc")
void  Clear_umLOG();
void  Clear_umMSG();

// Redirect EC printf() to file in console mode
extern FILE* EC_fout;
extern FILE* EC_ferr;

// Redirect printf() to file
extern FILE* g_pStdOut;
extern FILE* g_pStdErr;

//---------------------------------------------------------
// Some string utilities
//---------------------------------------------------------
#include <iosfwd>

void        umEATWHITE(std::ifstream& ins);
bool        getNextDataLine(std::ifstream& is, char* buf);
int         findOneOf(const char* source, const char* marks);
std::string trim     (const char* sz);
std::string lowercase(const char* sz);  // return lower case copy
std::string uppercase(const char* sz);  // return upper case copy
void        makelower(std::string& rs); // convert arg to lower case 
void        makeupper(std::string& rs); // convert arg to upper case 

bool        text2Bool(const char* sz);      // convert text->bool
std::string bool2Text(bool b, int mode=0);  // convert bool->text
std::string trim(const char* sz);           // strip leading/trailing spaces

#endif  // NDG__LOG__INCLUDED
