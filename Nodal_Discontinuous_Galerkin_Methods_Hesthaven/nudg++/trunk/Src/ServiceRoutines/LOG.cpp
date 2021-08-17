// LOG.cpp
//
// 2006/12/12
//---------------------------------------------------------
#include "NDGLib_headers.h"

#include <algorithm>    // for transform

#ifndef WIN32
#include <iterator>     // for back_inserter
#include <locale>
#include <cctype>
#include <string>
// LCW The following is a fix for a broken gcc with the transform functions
struct Toupper {
  Toupper (std::locale const& l) : loc(l) {;}
  char operator() (char c)  { return std::toupper(c,loc); }
  private:
  std::locale const& loc;
};
struct Tolower {
  Tolower (std::locale const& l) : loc(l) {;}
  char operator() (char c)  { return std::tolower(c,loc); }
  private:
  std::locale const& loc;
};
#endif  // !WIN32


//---------------------------------------------------------
// Global log targets
//---------------------------------------------------------

// Redirect messages to file in console mode
FILE* g_LOGFile = NULL;
FILE* g_MSGFile = NULL;

// trace allocation of arrays
FILE* g_TRCFile = NULL;

// Redirect EC printf() to file in console mode
FILE* EC_fout = NULL;
FILE* EC_ferr = NULL;

// Redirect printf() to file
FILE* g_pStdOut = NULL;
FILE* g_pStdErr = NULL;

int g_LOG_FLAG = 5;  // [0:10]
int g_MSG_FLAG = 5;  // [0:10]
int g_TRC_FLAG = 5;  // [0:10]


//---------------------------------------------------------
// Manage display of warnings:
//---------------------------------------------------------

bool g_SimAborted = false;

// Allow this number of warnings before forcing exit
int g_nMax_Warnings = 1000;
void setMaxWarnings (int maxw) { g_nMax_Warnings = maxw; }

// Toggle warnings at run time:
bool g_bShowMessages = true;
bool g_bShowWarnings = true;
void toggleWarnings (bool onoff) { g_bShowWarnings = onoff; }


//---------------------------------------------------------
void umLOG(int n, const char* format_str, ...)
//---------------------------------------------------------
{
  static char buf1[1024];

  if (n <= g_LOG_FLAG) 
  {
    va_list arglist;
    va_start(arglist, format_str);
    int nUsed = -1;
    nUsed = vsnprintf(buf1, 1023, format_str, arglist);
    assert(nUsed>=0);
    va_end(arglist);

//#if (wxUSE_GUI)
//  if (g_pPage1) { g_pPage1->Msg( buf1 ); }
//#endif

    if (g_LOGFile) {
      fprintf(g_LOGFile, "%s", buf1);
      fflush (g_LOGFile);
      printf("%s", buf1);
    }
  }
}

void umLOG(const std::string& msg, int n) 
{
  umLOG(n, msg.c_str());
}


//---------------------------------------------------------
void umMSG(int n, const char* format_str, ...)
//---------------------------------------------------------
{
  static char buf1[1024];

  if (n <= g_MSG_FLAG)
  {
    va_list arglist;
    va_start(arglist, format_str);
    int nUsed = -1;
    nUsed = vsnprintf(buf1, 1023, format_str, arglist);
    assert(nUsed>=0);
    va_end(arglist);

//#if (wxUSE_GUI)
//  if (g_pPage2) { g_pPage2->Msg( buf1 ); }
//#endif

    if (g_MSGFile) {
      fprintf(g_MSGFile, "%s", buf1);
      fflush (g_MSGFile);
      #ifdef _DEBUG
      printf("%s", buf1);   // echo to stdout
      #endif
    }

  }
}

void umMSG(const std::string& msg, int n) 
{
  umMSG(n, msg.c_str());
}


//---------------------------------------------------------
void umTRC(int n, const char* format_str, ...)
//---------------------------------------------------------
{ 
  static char buf1[1024];
  static int once=0;

  if (g_TRCFile)
  {
    if (n <= g_TRC_FLAG)
    {
      va_list arglist;
      va_start(arglist, format_str);
      int nUsed = -1;
      nUsed = vsnprintf(buf1, 1023, format_str, arglist);
      assert(nUsed>=0);
      va_end(arglist);

      fprintf(g_TRCFile, "%s", buf1);
      fflush (g_TRCFile);
    }
  }
  else
  {
    if (!once)
    {
      printf("WARNING: g_TRCFile closed early\n");
      once = 1;
    }
  }
}

void umTRC(const std::string& msg, int n) 
{
  umTRC(n, msg.c_str());
}


//---------------------------------------------------------
void umWARNING (char* function_name, ...)
//---------------------------------------------------------
{
  // only g_nMax_Warnings warnings are allowed:
  static char buf[2048];
  static int nwarnings = 0;
  nwarnings++;
  if (nwarnings > g_nMax_Warnings)
    umERROR("umWARNING", "More than %d calls to the warning functions.", g_nMax_Warnings);

  if (!g_bShowWarnings) 
    return;

  va_list ap;
  char* fmt;
  va_start(ap, function_name);
  fmt = va_arg(ap, char*);
  vsprintf(buf, fmt, ap);
  va_end(ap);

  stringstream ss;
  ss << "\n>>>> WARNING:  << "    << function_name 
     << " reports: (warning no. " << nwarnings 
     << ")\n"                     << buf 
     << "\n" << ends;

  umLOG(1, ss.str().c_str());
}


//---------------------------------------------------------
void umERROR (char* function_name, ...)
//---------------------------------------------------------
{
  static char buf[2048];
  va_list ap;
  char* fmt;
  va_start(ap, function_name);
  fmt = va_arg(ap, char*);
  vsprintf(buf, fmt, ap);
  va_end(ap);

  stringstream ss;
  ss << "\n\n>>>>> Handling an exception: " 
     << function_name << " reports: \n" 
     << buf << "\n\n" << ends;

  umLOG(1, ss.str().c_str());

  // throw exception to try block in user's code
  //throw umException (function_name, ss.str(), false, __FILE__, __LINE__);
  throw ss.str();
}


//---------------------------------------------------------
void umQUIT()
//---------------------------------------------------------
{
  stringstream ss;
  ss << "\n\n >>>>> A fatal error condition has developed <<<<<"
        "\n\nTo find the source of this error, #define _DEBUG"
        "\nand run the application under a debugger.\n\n" << ends;

  g_SimAborted = true;

  umLOG(1, ss.str().c_str());

#if (0)
  DWORD dwExitCode = 3;
  ExitThread(dwExitCode);
#else
  exit(3);
#endif
}


void Clear_AP_Log() { }
void Clear_umLOG()  { }
void Clear_umMSG()  { }



//---------------------------------------------------------
std::string umAFORM (const char* fmt, ...)
//---------------------------------------------------------
{
  // Uses cyclic buffers 

  va_list ap;
  va_start(ap, fmt);
  static int ncalls = 0;
  const  int repeat_cycle = 8;
  static char s [repeat_cycle][999];

  if (++ncalls == 8)
    ncalls = 0; // start with the first buffer again

  vsprintf (s[ncalls], fmt, ap);
  va_end(ap);

  return std::string(s[ncalls]);
}


// Internal formatting of small strings using 
// internal buffer (no alloc calls required)
//---------------------------------------------------------
char* umOFORM (const char* fmt, ...)
//---------------------------------------------------------
{
  va_list ap;
  va_start(ap, fmt);
  static int ncalls = 0;
  const  int repeat_cycle = 8;
  static char s [repeat_cycle][999];

  if (++ncalls == 8)
    ncalls = 0;

  vsprintf (s[ncalls], fmt, ap);
  va_end(ap);

  return s[ncalls];
}


///////////////////////////////////////////////////////////
//
// Some string utilities
//
///////////////////////////////////////////////////////////


// strip blanks, tabs, newlines and EOF 
//---------------------------------------------------------
void umEATWHITE(ifstream& ins) 
//---------------------------------------------------------
{
  while (isspace(ins.peek()))
    ins.ignore();
  if (ins.peek() == EOF)
    ins.ignore();
  return;
}


// Size of buffer used for EC expressions.
// How long can an expression be?
#define eqnBUfSIZE  2000
//---------------------------------------------------------
bool getNextDataLine(ifstream& is, char* buf)
//---------------------------------------------------------
{
  // Skip comment lines
  bool bDataFound = false;
  while (!bDataFound && is.good()) {
    umEATWHITE(is);
  //is.eatwhite(); 
    is.getline(buf, eqnBUfSIZE, '\n');
    if (buf[0] != '#' && strlen(buf)>0) 
    {
      bDataFound = true;
    }
  }
  return (bDataFound && (is.good() || is.eof()));
}


// Return zero-based index of the first character in source 
// that is also in marks. Return -1 if there is no match.
//---------------------------------------------------------
int findOneOf(const char* source, const char* marks)
//---------------------------------------------------------
{
  int len = (int) strlen(source);
  for (int i=0; i<len; ++i){
    if (strchr(marks, source[i]))
      return i;
  }
  return -1;
}


// strip spaces from both ends of a string.
//---------------------------------------------------------
std::string trim(const char* sz)
//---------------------------------------------------------
{
  std::string s(sz), s2;
  if (s.length() == 0)
    return s;
  std::size_t beg = s.find_first_not_of(" \a\b\f\n\r\t\v");
  std::size_t end = s.find_last_not_of(" \a\b\f\n\r\t\v");
  if (beg == std::string::npos) // No non-spaces
    return "";

  s2 = s.substr(beg, end - beg + 1);
  return s2;
}


//---------------------------------------------------------
void makelower(std::string& rs)
//---------------------------------------------------------
{
  /// convert string to lower case
#if defined(WIN32) && !defined(__CYGWIN__)
  std::transform (rs.begin(), rs.end(), rs.begin(), tolower);
#else
  Tolower down(std::locale("C"));
  std::transform (rs.begin(), rs.end(), rs.begin(), down);
#endif
}


//---------------------------------------------------------
void makeupper(std::string& rs)
//---------------------------------------------------------
{
  /// convert string to UPPER case
#if defined(WIN32) && !defined(__CYGWIN__)
  std::transform (rs.begin(), rs.end(), rs.begin(), toupper);
#else
  Toupper up(std::locale("C"));
  std::transform (rs.begin(), rs.end(), rs.begin(), up);
#endif
}


//---------------------------------------------------------
std::string lowercase(const char* sz)
//---------------------------------------------------------
{
  /// return lower case copy
  std::string tmp(sz);
  makelower(tmp);
  return tmp;
}


//---------------------------------------------------------
std::string uppercase(const char* sz)
//---------------------------------------------------------
{
  /// return upper case copy
  std::string tmp(sz);
  makeupper(tmp);
  return tmp;
}


/// Manage conversion between boolean and text values
//---------------------------------------------------------
bool text2Bool(const char* sz)
//---------------------------------------------------------
{
  bool b=false;
  std::string text = lowercase( trim(sz).c_str() );

  if      ("false" == text)  b = false;
  else if ("true"  == text)  b = true;
  else if ("off"   == text)  b = false;
  else if ("on"    == text)  b = true;
  else if ("0"     == text)  b = false;
  else if ("1"     == text)  b = true;
  else {
    umERROR("text2Bool", "Unexpected boolean text, [%s]", text.c_str());
  }

  return b;
}


//---------------------------------------------------------
std::string bool2Text(bool b, int mode)
//---------------------------------------------------------
{
  std::string sz;

  switch (mode)
  {
  case 0 :  sz = (b ? "true" : "false" ); break;
  case 1 :  sz = (b ? "ON"   : "OFF"   ); break;
  case 2 :  sz = (b ? "OK"   : "ERROR" ); break;
  default:  sz = (b ? "true" : "false" ); break;
  }

  return sz;
}


extern "C" {

// alternative to bail_to_command_line() in Gnuplot
//---------------------------------------------------------
void gpTHROW (char* function_name, ...)
//---------------------------------------------------------
{
  static char buf[2048];
  va_list ap;
  char* fmt;
  va_start(ap, function_name);
  fmt = va_arg(ap, char*);
  vsprintf(buf, fmt, ap);
  va_end(ap);

  stringstream ss;
  ss << "\n\n>>>>> Handling an exception from Gnuplot subsystem: " 
     << function_name << " reports: \n" 
     << buf << "\n\n" << ends;

  umMSG(1, "\n%s\n", ss.str().c_str());

  // throw exception to try block in user's code
  //throw "NDG Error";
  umERROR("gpTHROW()","See Gnuplot's bail_to_command_line()");
}


//---------------------------------------------------------
void gpPAUSE(const char* msg)
//---------------------------------------------------------
{
  // do nothing
}

} // extern "C"



//---------------------------------------------------------
void umPAUSE(const char* msg)
//---------------------------------------------------------
{
  // do nothing
}

