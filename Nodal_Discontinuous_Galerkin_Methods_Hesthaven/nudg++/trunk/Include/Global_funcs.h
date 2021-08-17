// Global_funcs.h
// declare global functions
// 2007/10/15
//---------------------------------------------------------
#ifndef NDG__Global_funcs_H__INCLUDED
#define NDG__Global_funcs_H__INCLUDED


#define wxUSE_GUI 0     // GUI disabled


#include "Constants.h"
#include "Mat_COL.h"
#include "Strings.h"    // define a vector of std::string

class Cub2D;
class Gauss2D;

class Cub3D;
//class Gauss3D;

// flag to hide untranslated blocks
#define THIS_IS_READY 0
#define TESTING_IMAP  1


// Define numeric constants for boundary conditions
// In = 1; Out = 2; Wall = 3; Far = 4; Cyl = 5; 
// Dirichlet = 6; Neuman = 7; Slip = 8;

enum BCTypes {
  BC_None      = 0, 
  BC_In        = 1, 
  BC_Out       = 2, 
  BC_Wall      = 3, 
  BC_Far       = 4, 
  BC_Cyl       = 5, 
  BC_Dirichlet = 6, 
  BC_Neuman    = 7,
  BC_Slip      = 8,
  BC_All       = 99
};


// Define numeric constants for flux types
enum FluxTypes {
  FT_LaxF   = 1,
  FT_Roe    = 2,
  FT_RoeVL  = 3,
  FT_HLL    = 4,
  FT_HLLC   = 5,
  FT_Eigen  = 6
};


// Define numeric constants for filter and limiter types
enum {
  LIMITER_none   = 0,
  FILTER_cutoff  = 1,
  FILTER_exp     = 2,
  LIMITER_LS_ENO = 3,
  LIMITER_WENO   = 4
};


//---------------------------------------------------------
// OS-dependent items
//---------------------------------------------------------
#if defined(WIN32) && !defined(__CYGWIN__)
  #define HAS_ISINF 1
  #include <float.h>
  #define isnan(x)  (_isnan(x))
  #define isinf(x)  (!(_finite(x)) && !(_isnan(x)))
  #define finite(x) (_finite(x))
  // string functions
  #define snprintf _snprintf
#else

  // finite/isnan for Mac
  // Aggressive delve into math.h on OS X
//  #define isinf(x)  inline_isinfd(x)

#endif


//---------------------------------------------------------
// inline utility functions
//---------------------------------------------------------

inline int    umMOD (int n, int m) { return n%m; }
inline int    umDIV (int n, int m) { return ((n-(n%m))/m); }
inline double   SQ  (double x)     { return x*x; }

inline double dSign(double a, double b) { return (b>=0.0 ? std::abs(a) : -std::abs(a)); }
inline double iSign(int    a, int    b) { return (b>=0   ? std::abs(a) : -std::abs(a)); }


void NDG_garbage_collect();

//---------------------------------------------------------
// function prototypes
//---------------------------------------------------------
void    JacobiGQ(double alpha, double beta, int n, DVec& x, DVec& w, bool sort=true);
DVec&   JacobiGL(double alpha, double beta, int N);
DVec&   JacobiP (const DVec& x,double alpha,double beta,int N);
DMat&   Vandermonde2D(int N, const DVec& r, const DVec& s);
DMat&   Vandermonde1D(int N, const DVec& xp);
DVec&   Simplex2DP(const DVec& a, const DVec& b, int i, int j);
DVec&   GradJacobiP(const DVec& z,double alpha,double beta,int N);
void    GradVandermonde2D(int N, const DVec& r, const DVec& s, DMat& V2Dr, DMat& V2Ds);
void    Nodes2D(int N, DVec& x, DVec& y);
void    xytors(const DVec& x, const DVec& y, DVec& r, DVec& s);
void    Dmatrices2D(int N, const DVec& r, const DVec& s, const DMat& V, DMat& Dr, DMat& Ds);
DVec&   TriAreas(const DVec& x1, const DVec& y1, const DVec& x2, const DVec& y2, const DVec& x3, const DVec& y3);


// overloaded global versions: for matrix and vector args
void    GeometricFactors2D(const DMat& x, const DMat& y, const DMat& Dr, const DMat& Ds, DMat& rx, DMat& sx, DMat& ry, DMat& sy, DMat& J);
void    GeometricFactors2D(const DVec& x, const DVec& y, const DMat& Dr, const DMat& Ds, DVec& rx, DVec& sx, DVec& ry, DVec& sy, DVec& J);

// overloaded global version
void    Dmatrices2D(int N, const DVec& r, const DVec& s, const DMat& V, DMat& Dr, DMat& Ds);

void    rstoab(const DVec& r, const DVec& s, DVec& a, DVec& b);
void    GradSimplex2DP(const DVec& a, const DVec& b, int id, int jd, DVec& dmodedr, DVec& dmodeds);
DVec&   Warpfactor(int N, const DVec& rout);
void    Connect2D(const IMat& EToV, IMat& EToE, IMat& EToF);

void    Cubature2D(int Corder, Cub2D& cub);
void    Cubature2D(int Corder, DVec& r, DVec& s, DVec& w, int& Ncub);

void    Cubature3D(int Corder, Cub3D& cub);
void    Cubature3D(int Corder, DVec& r, DVec& s, DVec& t, DVec& w, int& Ncub);

void  tiConnect2D(IMat& EToV, IMat& EToE, IMat& EToF);
void  tiConnect3D(IMat& EToV, IMat& EToE, IMat& EToF);

// 3D
DMat&   Vandermonde3D(int N, const DVec& r, const DVec& s, const DVec& t);
void    Nodes3D(int p, DVec& X, DVec& Y, DVec& Z);
void    EquiNodes3D(int N, DVec& X, DVec& Y, DVec& Z);
void    rsttoabc(const DVec& r, const DVec& s, const DVec& t, DVec& a, DVec& b, DVec& c);
void    xyztorst(const DVec& X, const DVec& Y, const DVec& Z, DVec& r, DVec& s, DVec& t);
DVec&   Simplex3DP(const DVec& a, const DVec& b, const DVec& c, int i, int j, int k);

void    GradVandermonde3D(int N, const DVec& r, const DVec& s, const DVec& t, DMat& V3Dr, DMat& V3Ds, DMat& V3Dt);
void    GradSimplex3DP(const DVec& a, const DVec& b, const DVec& c, int id, int jd, int kd, DVec& V3Dr, DVec& V3Ds, DVec& V3Dt);

// overloaded global versions: for matrix and vector args
void    GeometricFactors3D(const DMat& x, const DMat& y, const DMat& z, const DMat& Dr, const DMat& Ds, const DMat& Dt, DMat& rx, DMat& sx, DMat& tx, DMat& ry, DMat& sy, DMat& ty, DMat& rz, DMat& sz, DMat& tz, DMat& J);
void    GeometricFactors3D(const DVec& x, const DVec& y, const DVec& z, const DMat& Dr, const DMat& Ds, const DMat& Dt, DVec& rx, DVec& sx, DVec& tx, DVec& ry, DVec& sy, DVec& ty, DVec& rz, DVec& sz, DVec& tz, DVec& J);

// overloaded global version
void    Dmatrices3D(int N, const DVec& r, const DVec& s, const DVec& t, const DMat& V, DMat& Dr, DMat& Ds, DMat& Dt);

void    WarpShiftFace3D(int p, double pval, double pval2, const DVec& L1, const DVec& L2, const DVec& L3, const DVec& L4, DVec& warpx, DVec& warpy);
void    evalshift(int p, double pval, const DVec& L1, const DVec& L2, const DVec& L3, DVec& dx, DVec& dy);
DVec&   evalwarp(int p, const DVec& xnodes, const DVec& xout);

IVec&   TopTheta(const DVec& u, double theta, bool do_print=false);


double   gamma(int x);
double  lgamma(const double x);
double   gamma(const double x);
double  factorial(int n);

DVec&   range(int lo, int hi);
DVec&   ones (int N);
DMat&   ones (int M, int N);
DVec&   zeros(int N);
DMat&   zeros(int M, int N);
DMat&   eye  (int N);
DVec&   rand (int N);
DMat&   rand (int M, int N);
IVec&   ceil (const DVec& V);
IVec&   intersect(const IVec& A, const IVec& B);

bool    isInf(const DVec& V);

bool    file_exists(std::string& fname);
bool    match_BC(std::string& name, const char* szbc);


//---------------------------------------------------------
// debug utilites: print arrays to the MSG file
//---------------------------------------------------------

// Numerical {DMat,IMat} and {DVec,IVec}
void dumpDMat(const DMat& M, const char* s);
void dumpIMat(const IMat& M, const char* s);
void dumpDVec(const DVec& V, const char* s);
void dumpIVec(const IVec& V, const char* s);
void dumpIMap(const IMap& V, const char* s);


//---------------------------------------------------------
// special functions (add as required)
//---------------------------------------------------------
DVec& besselj(int N, const DVec& X);

// tex "table" output
void textable(string& capt, FILE* fid, string* titles, DMat& entries, string* form);


///////////////////////////////////////////////////////////
//
// Templated versions (allow non-double data types)
//
///////////////////////////////////////////////////////////


// return an integer range from [start:stop].  
// If (stop<start), then the range decreases.
// templated version allows either DVec or IVec
// See also DVec& range(int lo, int hi);
//---------------------------------------------------------
template <typename T> inline 
Vector<T>& Range(T start, T stop)
//---------------------------------------------------------
{
  int len = 0;
  if (start<=stop)
       len = int(stop-start) + 1;
  else len = int(start-stop) + 1;

  Vector<T> *tmp=new Vector<T>(len, (T)0, OBJ_temp);

  if (stop != start) {
    // load vector with [start:stop], stepping by +/- 1
    T delta=(start<stop)?T(1):T(-1);
    (*tmp)(1) = T(start);                 // set start value
    for (int i=2; i<len; ++i) {
      (*tmp)(i) = (*tmp)(i-1) + T(delta); // delta may be negative
    }
    (*tmp)(len) = T(stop);                // set stop value
  } else {
    (*tmp) = start;  // start and stop are the same
  }

  return (*tmp);
}


// templated version for arbitrary type.
// See also DVec& ones(N);
//---------------------------------------------------------
template <typename T> inline 
Vector<T>& Ones(T N) 
//---------------------------------------------------------
{
  char buf[32]={""}; snprintf(buf,(size_t)30,"ones(%d)",N);
  Vector<T> *tmp=new Vector<T>(N, (T)1, OBJ_temp, buf);
  return (*tmp);
}


// templated version for arbitrary type.
// See also DVec& ones(M,N);
//---------------------------------------------------------
template <typename T> inline 
Mat_COL<T>& Ones(T M, T N) 
//---------------------------------------------------------
{
  char buf[52]={""}; snprintf(buf,(size_t)50,"ones(%d,%d)",M,N);
  Mat_COL<T> *tmp=new Mat_COL<T>(M, N, (T)1, OBJ_temp, buf);
  return (*tmp);
}


// templated version for arbitrary type.
// See also DVec& zeros(N);
//---------------------------------------------------------
template <typename T> inline 
Vector<T>& Zeros(T N) 
//---------------------------------------------------------
{
  char buf[32]={""}; snprintf(buf,(size_t)30,"zeros(%d)",N);
  Vector<T> *tmp=new Vector<T>(N, (T)0, OBJ_temp, buf);
  return (*tmp);
}


// templated version for arbitrary type.
// See also DVec& zeros(M,N);
//---------------------------------------------------------
template <typename T> inline 
Mat_COL<T>& Zeros(T M, T N) 
//---------------------------------------------------------
{
  char buf[52]={""}; snprintf(buf,(size_t)50,"zeros(%d,%d)",M,N);
  Mat_COL<T> *tmp=new Mat_COL<T>(M, N, (T)0, OBJ_temp, buf);
  return (*tmp);
}



// templated version for arbitrary type.
// See also DMat& eye(N);
//---------------------------------------------------------
template <typename T> inline 
Mat_COL<T>& Eye(T N) 
//---------------------------------------------------------
{
  Mat_COL<T> *tmp=new Mat_COL<T>("eye", OBJ_temp);
  tmp->identity(N);
  return (*tmp);
}


//---------------------------------------------------------
// debug utilities: print arrays in Matlab binary format
//---------------------------------------------------------
template <typename T> inline
void dumpMatlab(const Mat_COL<T>& M, const char* sz) {
  FILE* fp = fopen( umOFORM("%s.mat", sz), "wb");
  if (fp) { M.m_save(fp, sz); fclose(fp); }
}

template <typename T> inline
void dumpMatlab(const Vector<T>& V, const char* sz) {
  FILE* fp = fopen( umOFORM("%s.mat", sz), "wb");
  if (fp) { V.m_save_v(fp, sz); fclose(fp); }
}


#endif  // NDG__Global_funcs_H__INCLUDED
