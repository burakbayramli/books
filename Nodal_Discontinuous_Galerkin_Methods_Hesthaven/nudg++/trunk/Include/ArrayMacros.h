// ArrayMacros.h
// 
// 2007/09/20
//---------------------------------------------------------
#ifndef NDG__ArrayMacros_H__INCLUDED
#define NDG__ArrayMacros_H__INCLUDED

#define USE_SSE2           0
#define UNROLL_LOOPS       1
#define RGN_BASE_OFFSET   (1)

// select Cholesky solver
// #define NDG_USE_CHOLMOD
#undef NDG_USE_CHOLMOD


#ifndef NDEBUG
#define CHECK_ARRAY_INDEX   1
#else
#define CHECK_ARRAY_INDEX   0
#endif

// trace allocation/release of arrays
#ifndef NDEBUG
# define SHOW_D_ALLOC    1
# define SHOW_SP_ALLOC   1
# define USE_ARRAY_NAMES 1
#else
# define SHOW_D_ALLOC    0
# define SHOW_SP_ALLOC   0
# define USE_ARRAY_NAMES 1
#endif

#include <algorithm>
#include <string>

//---------------------------------------
// OS-dependent items
//---------------------------------------
#if defined(WIN32) && !defined(__CYGWIN__)
  #define snprintf _snprintf
#endif

#include "LOG_funcs.h"


//
// STRUCT: MATLAB binary output data
//
typedef struct {
  long type;      // matrix type
  long m;         // # rows
  long n;         // # cols
  long imag;      // is complex?
  long namlen;    // length of variable name
} umMATLAB;


//
// ENUM: object modes
//
typedef enum {
  OBJ_temp = 0,   // temporary, ownership to be transfered
  OBJ_real = 1    // normal object
} OBJ_mode;


//
// ENUM: sort order
//
typedef enum {
  eDescend = 0,
  eAscend  = 1
} eDir;


//
// ENUM: matrix orientation
//
typedef enum {
  MajorROW = 0,
  MajorCOL = 1
} eMajor;


// select section of matrix
enum {
  sp_LT  = -1,  // lower triangle
  sp_All =  0,  // entire matrix
  sp_UT  =  1   // upper triangle
};


// flags to indicate shape of matrix
enum {
  sp_NONE       = 0,  // arbitrary entries
  sp_LOWER      = 1,  // entries only in lower triangle
  sp_UPPER      = 2,  // entries only in upper triangle
  sp_TRIANGULAR = 4,  // entries only in one triangle
  sp_SYMMETRIC  = 8   // symmetric, but only one triangle stored
};


//
// ENUM: Norm types
//
typedef enum {
  NORM_l1,        // sqrt(     sum_i^n abs(v_i))
  NORM_L1,        // sqrt((1/n)sum_i^n abs(v_i))
  BLAS_l2,        // sqrt(     sum_i^n v_i^2)
  BLAS_L2,        // sqrt((1/n)sum_i^n v_i^2)
  NORM_Linf       // L-infinity norm
} norm_type;


//
// ENUM: factorization mode
//
typedef enum {
  FACT_NONE,      // LU without pivots
  FACT_CHOL,      // Cholesky: LL'
  FACT_CHOL_D,    // Cholesky: LDL'
  FACT_LU,        // LU without pivots
  FACT_LUP,       // LU with    pivots
  FACT_QR,        // QR without pivots
  FACT_QRP        // QR with    pivots
} fact_type;


//
// ENUM: boolean operations
//
typedef enum {
  B_lt,           // <
  B_le,           // <=
  B_gt,           // >
  B_ge,           // >=
  B_and,          // &&
  B_or            // ||
} B_type;


//
// ENUM: orientation (not used)
//
typedef enum {
  MAT_ident = 0,    // identity
  MAT_trans = 1     // transpose
} MAT_mode;


//
// ENUM: data mode for array constructors
//
typedef enum {
  SZ_DATA_VEC      = 0,   // load ASCII data into vector
  SZ_DATA_MAT_ROWS = 1,   // load data into matrix by ROWS
  SZ_DATA_MAT_COLS = 2    // load data into matrix by COLS
} SZ_DATA_mode;


// 
// orientation of array data
// 
class ArrayData {
public:
  ArrayData() : m_mode(SZ_DATA_VEC) {} 
  virtual ~ArrayData() {} 
  SZ_DATA_mode mode() const { return m_mode; }
  SZ_DATA_mode m_mode;
};
class MatRowData : public ArrayData {
public:
  MatRowData() { m_mode = SZ_DATA_MAT_ROWS; } 
  virtual ~MatRowData() {} 
};
class MatColData : public ArrayData {
public:
  MatColData() { m_mode = SZ_DATA_MAT_COLS; } 
  virtual ~MatColData() {} 
};
class MatDimension : public ArrayData {
public:
  MatDimension() { m_mode = SZ_DATA_VEC; } 
  virtual ~MatDimension() {} 
};


extern ArrayData     gVecData;
extern MatRowData    gRowData;
extern MatColData    gColData;
extern MatDimension  All;


// utilities used by struct facedata
inline int* umIVector(int L)               {return (int*)calloc(std::max(L,1),sizeof(int));}
inline int* umIVectorExtend(int *v, int L) {return (int*)realloc(v,sizeof(int)*std::max(L,1));}
inline void umIVectorFree(int*& v)         {if (v) {free(v); v=NULL;} }


//---------------------------------------
inline const char* mode_str(OBJ_mode mode) 
//---------------------------------------
{
  static std::string msg;
  switch (mode) {
    case OBJ_temp: msg = "-tmp"; break;
    case OBJ_real: msg = " reg"; break;
    default: msg = "????"; break;
  }
  return msg.c_str();
}


//---------------------------------------
inline const char* tmp_op_name
(
  const char* s1, 
  const char* op,
  const char* s2,
  std::string& name
) 
//---------------------------------------
{
#ifndef NDEBUG

  static char buf[300]={""};
  static int sID=0;
  ++sID;

  snprintf(buf, (size_t)298, "%s%s%s", s1,op,s2);
//sprintf (buf,              "%s%s%s", s1,op,s2);

#else

  static char buf[3]={"X"};

#endif

  name = buf;
  return buf;
}


//---------------------------------------------------------
// Some systems may not have abs() min(), and max() 
//---------------------------------------------------------
#undef USE_ABS_MIN_MAX
#ifdef USE_ABS_MIN_MAX
{
  inline double abs(double t)           { return ( t > 0 ? t : -t); }
  inline double min(double a, double b) { return (a < b ? a : b); }
  inline double max(double a, double b) { return (a > b ? a : b); }
  inline float  abs(float t) { return   ( t > 0 ? t : -t); }
  inline float  min(float a, float b)   { return (a < b ? a : b); }
  inline float  max(float a, float b)   { return (a > b ? a : b); }
  inline double sign(double a)          { return (a > 0 ? 1.0 : -1.0); }
  inline float  sign(float a)           { return (a > 0.0 ? 1.0f : -1.0f); }
} 
#endif

#endif // NDG__ArrayMacros_H__INCLUDED
