// Vector_Type.h
// 1-based () indexing
// 0-based [] indexing
// 2007/10/15
//---------------------------------------------------------
#ifndef NDG__Vector_Type_H__INCLUDED
#define NDG__Vector_Type_H__INCLUDED


#include <iostream>
#include <iomanip>
#include <sstream>

#include <cstdlib>
#include <cassert>
#include <cfloat>

#include "ArrayMacros.h"
#include "BlasLapack.h"
#include "RAND.h"
#include "Registry_Type.h"
#include <complex>

#include "Region1D.h"

//---------------------------------------
// Mapped regions
//---------------------------------------
#include "MappedRegion1D.h"
#include "MappedRegion2D.h"


typedef double (FuncPtr)(double);
typedef std::complex<double>  dcmplx;

// typedef versions for common data types
template <typename T> class Vector;

typedef Vector<double>  DVec;
typedef Vector<dcmplx>  ZVec;
typedef Vector<int>     IVec;
typedef Vector<long>    LVec;
typedef Vector<bool>    BVec;

// forward declarations
template <typename T> inline Vector<T>& concat(const Vector<T> &A, T x);
template <typename T> inline Vector<T>& concat(const Vector<T> &A, const Vector<T> &B);


//---------------------------------------------------------
template <typename T> 
class Vector
//---------------------------------------------------------
{
  //
  // Member data
  //
protected:

  T*  v_;         // original pointer for 0-based indexing
  T*  vm1_;       // adjusted pointer for 1-based indexing
  int m_Len;      // num elements (i.e. length of array)
  T   ZERO;       // use as T(0);
  T   ONE;        // use as T(1);

  std::string m_name;   // identifier string
  double    m_EqTol;    // allow "fuzzy" comparison
  bool      m_borrowed; // is data borrowed?
  int       m_id;       // identifier number
  OBJ_mode  m_mode;     // real or temporary

  static int            s_count;  // track number of objects
  static umRegistry<T>  s_Reg1;   // manage small arrays (preallocated)
  static umRegistry<T>  s_Reg2;   // manage medium arrays (preallocated)
  static umRegistry<T>  s_Reg3;   // manage general allocations
         umRegistry<T>* m_pReg;   // pointer to registry storing this data
  static bool           s_trace;  // toggle tracing of allocations

public:

  // typedef's for std::compatibility

  typedef     int   size_type;
  typedef       T   data_type;
  typedef       T*  pointer;
  typedef       T*  iterator;
  typedef       T&  reference;
  typedef const T*  const_iterator;
  typedef const T&  const_reference;

  // toggle _DEBUG trace
  static void set_trace(bool b) { s_trace = b; }


  // constructors
  explicit Vector(const char* sz="vec",  OBJ_mode md=OBJ_real);
           Vector(const Vector<T> &B,    OBJ_mode md=OBJ_real, const char* sz="vec");
  explicit Vector(int N, const char* sz, OBJ_mode md=OBJ_real);
  explicit Vector(int N, const T x=T(0), OBJ_mode md=OBJ_real, const char* sz="vec");
  explicit Vector(int N, const T* vdata, OBJ_mode md=OBJ_real, const char* sz="vec");
  explicit Vector(const ArrayData& rAD, int N, const char *sdata, OBJ_mode md=OBJ_real, const char* sz="vec");
  explicit Vector(const T x1, const T x2, const T x3);

  // destructor
  virtual ~Vector();
  virtual void destroy  ();   // mark as available
  virtual void destroy_2();   // free allocation
  virtual void Free();

  // manage allocation
  Vector<T>& borrow(int N, T* p);
  Vector<T>& borrow(const Vector<T>& CV);
  Vector<T>& own  (Vector<T>& B);   // allow lazy deallocation
  Vector<T>& own_2(Vector<T>& B);   // force immediate dealloc

  void initialize(int N, bool bInit=true, T x=T(0));
  bool resize(int N, bool bInit=true, T x=T(0));
  bool resize(const Vector<T>& B, bool bInit=true, T x=T(0));
  void truncate(int N);
  bool extend(int N, bool bInit=true, T x=T(0));
  bool realloc(int N, bool bInit=true, T x=T(0)); // same as extend
  bool switch_Registry(int N, bool bInit=true, T x=T(0));
  bool reg_ok() const;
  void show_registry() const { if (reg_ok()) {m_pReg->show_alloc();}}
  void compact() const;

  // copy/load
  Vector<T>& copy(       const T *vec);
  Vector<T>& copy(int N, const T *vec);
  Vector<T>& copy(int N, const Vector<T>& V);

  void load(int N, const char *sdata);
  void set(const T x1, const T x2);
  void set(const T x1, const T x2, const T x3);
  void set(const T x1, const T x2, const T x3, const T x4);

  // assignment
  Vector<T>& operator=(const T &x);
  Vector<T>& operator=(const Vector<T> &B);
  // allow assignment of 
  Vector<T>& assign(const IVec &B); // enable IVec -> DVec
  Vector<T>& assign(const DVec &B); // enable DVec -> IVec


  Vector<T>& append(const T&  x);
  Vector<T>& append(const Vector<T>& B);

  // utilities
  bool  ok() const                { return ((v_!=NULL) && (m_Len>0)); }
  int   size() const              { return  m_Len; }
  int   length() const            { return  m_Len; }
  int   lbound () const           { return 1; }     // for Region1D
  int   dim(int d=1) const        { return m_Len; } // for Region1D
  void  set_name(const char* sz)  { m_name = sz; }
  const char* name() const        { return m_name.c_str(); }
  bool  is_borrowed() const       { return m_borrowed; }
//bool  is_inf() const;           // check finite
//bool  is_nan() const;           // check NaN
  int   get_id() const            { return m_id; }
  void  set_id(int id)            { m_id = id; }
  int   get_s_count() const       { return s_count; }
  int   get_nnz(T tol=T(0)) const;

  // "mode"
  OBJ_mode get_mode() const       { return m_mode; }
  void set_mode(OBJ_mode mode)    { m_mode = mode; }

  Vector<T>&  zeros(int N=0);
  Vector<T>&  ones(int N=0);
  Vector<T>&  fill(const T &x);
  Vector<T>&  operator!() const;
  Vector<T>&  set_abs();
  Vector<T>&  set_min_val(double dtol=0.0);
  Vector<T>&  SQRT();
  Vector<T>&  SQR();

  T     max_val() const;                // return result only
  T     min_val() const;                //   "
  void  max_val(T& res, int& id) const; // return both result and array id
  void  min_val(T& res, int& id) const; //   "


  T max_val_abs() const;
  T min_val_abs() const;
  T sum() const;
  T sumsquares() const;
  T inner(const Vector<T> &B) const;
  T inner(int N, const T* p) const;

  double mean() const;

  // vector norms
  double norm (norm_type lp = BLAS_L2) const;
  double norm1()    const { return norm(NORM_l1); }
  double norm2()    const { return norm(BLAS_L2); }
  double norm_inf() const { return norm(NORM_Linf); }

  Vector<T>&  normalize();
  Vector<T>&  randomize(double from=0.0, double to=1.0, int N=0);
  Vector<T>&  rand(double from=0.0, double to=1.0, int N=0);
  Vector<T>&  randnorm();
  Vector<T>&  apply(FuncPtr fptr);
  Vector<T>&  exp_val();
  Vector<T>&  log();
  Vector<T>&  chop(double eps = 1e-10);

  // "fuzzy" compare
  double GetEqTol() const { return m_EqTol; }
  void  SetEqTol(double tol) { m_EqTol=tol; }
  bool  same_object(const Vector<T>& B) const;
  bool  operator==(const Vector<T>& B) const;
  bool  operator!=(const Vector<T> &B) const { return (!operator==(B));}

  //-------------------------------------
  // return "Boolean" results
  //-------------------------------------
  Vector<T>& eq    (T val) const;   // r = (BCType==Wall)

  Vector<T>& le    (T val) const;   // r = (u <= val)
  Vector<T>& lt    (T val) const;   // r = (u <  val)
  Vector<T>& lt_abs(T val) const;   // r = (u <  tol)

  Vector<T>& ge    (T val) const;   // r = (u >= val)
  Vector<T>& gt    (T val) const;   // r = (u >  val)
  Vector<T>& gt_abs(T val) const;   // r = (u >  tol)

  bool any(const T& val) const;     // is val in this array?

  //-------------------------------------
  // numerical routines
  //-------------------------------------

  // apply scalar to all elements
  void add_val (const T &x);
  void sub_val (const T &x) { add_val(-x); }
  void mult_val(const T &x);
  void div_val (const T &x);
  void pow_val (const T &x);


  //-------------------------------------
  // Overloaded arithmetic operators.
  // Note: pattern for global operators,
  // tmp(A);  tmp+=x;  return tmp;
  //-------------------------------------
  Vector<T>& operator += (const T &x) { add_val ( x); return (*this); }
  Vector<T>& operator -= (const T &x) { add_val (-x); return (*this); }
  Vector<T>& operator *= (const T &x) { mult_val( x); return (*this); }
  Vector<T>& operator /= (const T &x) { div_val ( x); return (*this); }


  Vector<T>& operator+=(const Vector<T>& B);
  Vector<T>& operator-=(const Vector<T>& B);
  Vector<T>& operator*=(const T* p);
  Vector<T>& operator*=(const Vector<T>& B);
  Vector<T>& operator/=(const Vector<T>& B);

  Vector<T>& dd(const Vector<T> &B) const;    // C = A ./ B
  Vector<T>& dm(const Vector<T> &B) const;    // C = A .* B

  // this += alpha*X
  void axp_y (const T& alpha, const Vector<T>& X);

  // this = alpha*X + y
  void axp_y (const T& alpha, const Vector<T>& X, const Vector<T>& Y);



  //-------------------------------------
  // element access
  //-------------------------------------

  // set/get:  1-based access
        T& operator()(int i)       {Check_index_1(i); return vm1_[i];}
  const T& operator()(int i) const {Check_index_1(i); return vm1_[i];}

  // set/get:  0-based access
        T& operator[](int i)       {Check_index_0(i); return v_[i];}
  const T& operator[](int i) const {Check_index_0(i); return v_[i];}

  // expose data pointer
  const T*    get_v() const     { return v_; }
        T*    get_v()           { return v_; }
        void  set_v(T* p)       { v_ = p; }

  // array access: return T*
  const T* data() const         { return v_; }
        T* data()               { return v_; }

  // return "data()-1" for 1-based access
        T* d_m1()       { return vm1_; } 
  const T* d_m1() const { return vm1_; }

  // array access: return T**
  const T** pdata() const       { return (const T**)(&v_); }
        T** pdata()             { return (&v_); }

  // array access: "stl container"
  const_iterator begin() const  { return v_; }
        iterator begin()        { return v_; }

  // array access: "stl container"
  const_iterator end() const    { return v_ + m_Len; }
        iterator end()          { return v_ + m_Len; }

  // expose address of current registry
  const umRegistry<T>*  get_reg() const   { return m_pReg; }
        umRegistry<T>*  get_reg()         { return m_pReg; }


  //-------------------------------------
  // I/O
  //-------------------------------------

  void print (
    FILE* os = stdout, 
    const char* msg=NULL,
    const char* fmt="lf", // [%d|%e|%lf|%g]
    int  prec =2,       // sig.figs|dec.places
    int  wdth =6,       // output spacing [12]
    bool vert =false,   // vertical/horizontal
    int  nline=5,       // entries per line
    int  nv   =0        // num vals to write (0 --> all)
  ) const;


  void print_STREAM (
    std::ostream& os, 
    const char* msg=NULL,
    char fmt  ='G',     // [%d|%e|%lf|%g]
    int  prec =2,       // sig.figs|dec.places
    int  wdth =6,       // output spacing [12]
    bool vert =false,   // vertical/horizontal
    int  nline=5,       // entries per line
    int  nv   =0        // num vals to write (0 --> all)
  ) const;


  std::string display (
    const char* msg=NULL,
    char fmt  ='D',     // [%d|%e|%lf|%g]
    int  prec =2,       // sig.figs|dec.places
    int  wdth =6,       // output spacing [12]
    bool vert =false,   // vertical/horizontal
    int  nline=5,       // entries per line
    int  nv   =0        // num vals to write (0 --> all)
  ) const;


  // Save vector as (Mx1) array in Matlab ".mat" format (BINARY)
  // Note: For Win32, fp MUST be opened in "wb" mode
  void m_save_v(FILE* fp, const char* name) const;
  void m_load_v(FILE *fp, char **name);


protected:
  //-------------------------------------
  // Optional index checking
  //-------------------------------------
#if (CHECK_ARRAY_INDEX)

  void Check_index_0(int i) const {
    // check for legal 0-based index
    if (i< 0 )    throw 1;
    if (i>=m_Len) throw 2;
  }
  void Check_index_1(int i) const {
    // check for legal 1-based index
    if (i<1 )     throw 1;
    if (i>m_Len)  throw 2;
  }

#else

  // no checking of indices
  void Check_index_0(int i) const throw() {}
  void Check_index_1(int i) const throw() {}

#endif


  //-------------------------------------------------------
  // member functions involving a subset of the array
  //-------------------------------------------------------
public:

  void linspace(T start, T stop, int len=0);
  void range(int start, int stop);

  Vector<T>&  get_map(const IVec& iM) const;
  Vector<T>&  set_map(const IVec& iM, const T &x);
  Vector<T>&  set_map(const IVec& iM, const Vector<T>& X);


  //-------------------------------------------------------
  // operations involving (contiguous) Region1D
  //-------------------------------------------------------
public:
  Vector(const Region1D< Vector<T> > &R, OBJ_mode md=OBJ_real, const char *sz="vec");
  Vector(const_Region1D< Vector<T> > &R, OBJ_mode md=OBJ_real, const char *sz="vec");

  const_Region1D< Vector<T> > operator()(const Index1D &I) const;
        Region1D< Vector<T> > operator()(const Index1D &I);

  Vector<T>&  operator=(const Region1D< Vector<T> > &R);
  Vector<T>&  operator=(const const_Region1D< Vector<T> > &R);
  Vector<T>&  append   (const Region1D< Vector<T> > &R);
  Vector<T>&  mult_element(const Region1D< Vector<T> > &R);
  Vector<T>&  div_element(const Region1D< Vector<T> > &R);
  Vector<T>&  dd(const Region1D< Vector<T> > &R) const;
  Vector<T>&  dm(const Region1D< Vector<T> > &R) const;
  Vector<T>&  operator+=(const Region1D< Vector<T> > &R);
  Vector<T>&  operator-=(const Region1D< Vector<T> > &R);
  //-------------------------------------------------------


  //-------------------------------------------------------
  // operations involving MappedRegion1D
  //-------------------------------------------------------
public:
  Vector(const MappedRegion1D< Vector<T> > &R, OBJ_mode md=OBJ_real, const char *sz="vec");

  // IMAP
  const_MappedRegion1D< Vector<T> > operator()(const IVec &I) const;
        MappedRegion1D< Vector<T> > operator()(const IVec &I);
  // IMAP
  const_MappedRegion1D< Vector<T> > operator()(const const_Region1D< Vector<int> > &Ri) const;
        MappedRegion1D< Vector<T> > operator()(const Region1D< Vector<int> > &Ri);


  Vector<T>& operator=(const MappedRegion1D< Vector<T> > &R);
  Vector<T>& operator=(const const_MappedRegion1D< Vector<T> > &R);
  Vector<T>& mult_element(const MappedRegion1D< Vector<T> >& R);
  Vector<T>& div_element(const MappedRegion1D< Vector<T> >& R);
  Vector<T>& dd(const MappedRegion1D< Vector<T> > &R) const;
  Vector<T>& dm(const MappedRegion1D< Vector<T> > &R) const;
  Vector<T>& operator+=(const MappedRegion1D< Vector<T> > &R);
  Vector<T>& operator-=(const MappedRegion1D< Vector<T> > &R);
  //-------------------------------------------------------

};



///////////////////////////////////////////////////////////
//
// define static member data
//
///////////////////////////////////////////////////////////

// allow toggling of allocation tracing in debug mode
template <typename T> bool  Vector<T>::s_trace=false;
template <typename T> int   Vector<T>::s_count=0;


///////////////////////////////////////////////////////////
//
// Set initial sizes of the 3 registries here here
//
///////////////////////////////////////////////////////////

// Both the smaller registries involve less memory 
// than one large matrix, so we preallocate them:
#if (1)
template <typename T> umRegistry<T> Vector<T>::s_Reg1(500, true, iReg_small); //  16 =  4* 4
template <typename T> umRegistry<T> Vector<T>::s_Reg2(500, true, iReg_med);   // 256 = 16*16
template <typename T> umRegistry<T> Vector<T>::s_Reg3(500, false, 0);    // arbitrary length
#else
template <typename T> umRegistry<T> Vector<T>::s_Reg1(10, true, iReg_small); //  16 =  4* 4
template <typename T> umRegistry<T> Vector<T>::s_Reg2(10, true, iReg_med);   // 256 = 16*16
template <typename T> umRegistry<T> Vector<T>::s_Reg3(10, false, 0);    // arbitrary length
#endif



///////////////////////////////////////////////////////////
//
// constructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(const char* sz, OBJ_mode md)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
}


// not "explicit"; allows construction from return objects
//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(const Vector<T> &B, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
  operator=(B);   // manage copy of real/temp objects
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(int N, const char* sz, OBJ_mode md)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{ 
  ++s_count;
  initialize(N, true, ZERO);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(int N, const T x, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{ 
  ++s_count;
  initialize(N, true, x); 
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(int N, const T* vdata, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
  initialize(N, false); 
  copy(vdata); 
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(const ArrayData& rAD, int N, const char *sdata, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
  initialize(N, false); 
  try {
    // Read ascii data into array
    std::istringstream ins(sdata); 
    for (int i=0; i<N; ++i) { ins >> v_[i]; }
  } catch(...) { 
    umERROR("Vector<%s>(%d, char*)", "istringstream exception", typeid(T).name(), N); 
  }
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(const T x1, const T x2, const T x3)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name("vec"), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(OBJ_real), m_pReg(NULL)
{
  ++s_count;
  initialize(3, false); 
  vm1_[1]=x1; vm1_[2]=x2; vm1_[3]=x3;
}




///////////////////////////////////////////////////////////
//
// destructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Vector<T>::~Vector()
//---------------------------------------------------------
{
  --s_count;

  if (v_) {
    destroy();
  }
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::destroy()
//---------------------------------------------------------
{
  if (v_ && (!m_borrowed)) {
    assert(reg_ok());
    m_pReg->free_alloc(v_, m_id);   // mark allocation as "available"
  }

  v_    = NULL;   // Pointers may be tested, 
  vm1_  = NULL;   // so set both to NULL.
  m_Len = 0;      // no data left
  m_id  = -1;     // no slot in registry
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::destroy_2()
//---------------------------------------------------------
{
  if (v_ && (!m_borrowed)) {
    assert(reg_ok());
    m_pReg->free_alloc_2(v_, m_id); // ***RELEASE*** allocation
  }

  v_    = NULL;   // Pointers may be tested, 
  vm1_  = NULL;   // so set both to NULL.
  m_Len = 0;      // no data left
  m_id  = -1;     // no slot in registry
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::Free()
//---------------------------------------------------------
{ 
//destroy  ();    // mark slot as available
  destroy_2();    // release registry entry
}  



///////////////////////////////////////////////////////////
//
// manage allocation
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::borrow(int N, T* p)
//---------------------------------------------------------
{
  //---------------------------------------------
  // "Borrowing" data allows wrapping external 
  // arrays to exploit vector algorithms while 
  // avoiding any overhead of copying data.
  //
  // NB: do not tangle external arrays with Vector
  // registries.  Since this object does not "own" 
  // the external data, it must not delete it.
  //
  // return (*this) to enable syntax such as: 
  //
  //    y = A * Vj.borrow(n,V.pCol(j));
  //---------------------------------------------

  destroy();          // reset all members
  assert(N >= 0);     // must be non-negative
  if (!p) { umERROR("Array<T>::borrow(%d)", "pointer was NULL", N); }
  m_borrowed = true;  // do not delete the external data
  if (N > 0) {
    v_    = p;        // point to the external array
    vm1_  = v_ - 1;   // adjust 1-offset pointer
    m_Len = N;        // update length
    m_id  = -1;       // no slot in registry
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::borrow(const Vector<T>& CV)
//---------------------------------------------------------
{
  Vector<T>& B = const_cast<Vector<T>&>(CV);
  int N = B.length();
  T* p = B.data();
  this->borrow(N, p);
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::own(Vector<T>& B)
//---------------------------------------------------------
{
  // "owning" another vector transfers ownership 
  // of the allocation, avoiding any overhead of 
  // copying data, e.g. from a temporary object.

  destroy();              // reset all members

  if (!B.ok()) { 
    return (*this);       // both arrays are empty
  }

  // Take ownership of B's allocation:
  m_pReg= B.m_pReg;       // point to B's registry
  m_id  = B.m_id;         // take B's slot in registry
  v_    = B.v_;           // point to B's array
  vm1_  = v_ - 1;         // adjust 1-offset pointer
  m_Len = B.m_Len;        // update length

  m_name    = B.m_name;     // copy B's name
  m_EqTol   = B.m_EqTol;    // copy B's equality tolerance
  m_borrowed= B.m_borrowed; // copy B's borrowed status
  m_mode    = B.m_mode;     // copy B's mode

  // invalidate B:
  B.m_id  = -1;     // invalidate B's slot
  B.v_    = NULL;   // invalidate B's array
  B.vm1_  = NULL;   // invalidate B's array
  B.m_Len = 0;      // invalidate B's length

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::own_2(Vector<T>& B)
//---------------------------------------------------------
{
  // call this version when an array is very large,
  // and you want to force immediate release of the 
  // allocation from the registry

  this->destroy_2();  // release (large) allocation
  this->own(B);       // take over B's allocation
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::initialize(int N, bool bInit, T x)
//---------------------------------------------------------
{
  // Allocate (or reuse) data, adjust pointers
  //   v_[] is 0-offset for 0-based indexing
  // vm1_[] is 1-offset for 1-based indexing

  assert( N >= 0 );   // must be non-negative

  if ((N>0) && (m_Len!=N)) 
  {
    // About to allocate, expect v_ to be NULL
    assert( NULL == v_ );

    // Select storage location:
    if      (N <= iReg_small) { m_pReg = &s_Reg1; }
    else if (N <= iReg_med)   { m_pReg = &s_Reg2; }
    else                      { m_pReg = &s_Reg3; }
    
    v_ = m_pReg->get_alloc(N, m_id);
    
    assert(m_pReg->check_alloc(v_, m_id, N));
    if (!v_) { umERROR("Vector<T>::initialize(%d)", "alloc failed (%0.2lf MB)", N, double(N*(int)sizeof(T))/(1024.*1024.)); }
    
    vm1_  = v_ - 1;   // make 1-offset
  }
  else if ((N>0) && (m_Len==N))
  {
    assert( v_ );     // reusing allocation
    vm1_ =  v_ - 1;   // make 1-offset
  }
  else 
  {
    assert(0==N);
    v_ = vm1_ = NULL;
  }

  m_Len   = N;        // update length
  m_EqTol = 0.0;      // adjust for "fuzzy" comparison

  if (N>0 && bInit) {
    fill(x);          // fill array with given value
  }
}


//---------------------------------------------------------
template <typename T> inline
bool Vector<T>::resize(int N, bool bInit, T x)
//---------------------------------------------------------
{
  // Resize existing object, optionally setting 
  // the entire array to some given inital value.
  // Return value indicates whether size has changed.

  assert(!m_borrowed);  // "borrowed" allocations must not be changed
  assert( N >= 0 );     // must be non-negative

  bool bResized=false;

  // TODO: (N < m_Len) is an alternate test. 
  // If new array fits in current allocation,
  // just update some housekeeping details.

  if (N != m_Len) 
  {
    bResized=true;
    this->destroy();      // clear allocation, zero members
    if (N > 0) 
    {
      initialize(N, bInit, x);
    }
    m_Len = N;
  }
  else if (bInit && m_Len>0)
  {
    // no resize, but fill existing array
    fill(x);
  }

  return bResized;
}


//---------------------------------------------------------
template <typename T> inline
bool Vector<T>::resize(const Vector<T>& B, bool bInit, T x)
//---------------------------------------------------------
{
  // Resize existing object to match size of vector B.
  // Optionally set entire array to given inital value.
  // Return value indicates whether size has changed.
  int Nlen=B.size();
  bool bResized = this->resize(Nlen, bInit, x);
  return bResized;
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::truncate(int N)
{
  extend(N, false, 0);            // alias for extend
}


template <typename T> inline
bool Vector<T>::realloc(int N, bool bInit, T x)
{
  return extend(N, bInit, x);     // alias for extend
}


template <typename T> inline
bool Vector<T>::extend(int N, bool bInit, T x)
//---------------------------------------------------------
{
  // resize a Vector, retaining existing values

  if (N<1) { destroy(); return true; }

  assert(!m_borrowed);  // "borrowed" data not held in registry
  assert( N >= 0 );     // must be non-negative
  int Nold = m_Len;     // store current size

  if (!ok()) 
  {
    return resize(N, bInit, x);
  } 
  else if (m_Len == N) 
  {
    return true;    // i.e. all's OK
  //return false;   // i.e. not resized
  } 
  else 
  {
    // a. If new size "fits"    in current registry, just resize
    // b. If new size "belongs" in current registry, just resize

    if      (                    (N <= iReg_small) && (&s_Reg1==m_pReg)) {v_ = s_Reg1.resize_alloc(v_, N, m_id);}
    else if ((N > iReg_small) && (N <= iReg_med  ) && (&s_Reg2==m_pReg)) {v_ = s_Reg2.resize_alloc(v_, N, m_id);}
    else if ((N > iReg_med)                        && (&s_Reg3==m_pReg)) {v_ = s_Reg3.resize_alloc(v_, N, m_id);}
    else {
      switch_Registry(N, bInit, x);
    }

    if (!v_) {
      umWARNING("Vector::extend()", "Call to s_Reg.resize_alloc(%d) failed", N);
      destroy();
      return false;
    } 
    else 
    {
      vm1_ = v_ - 1;    // update 1-offset pointer
      m_Len=N;
      if (N>0 && bInit) 
      {
        // fill new elements with given value
        for (int i=Nold+1; i<=N; ++i) {
          vm1_[i] = x;
        }
      }
    }
    return true;
  }
}


//---------------------------------------------------------
template <typename T> inline
bool Vector<T>::switch_Registry(int N, bool bInit, T x)
//---------------------------------------------------------
{
  // "borrowed" data is not held in registry
  assert(!m_borrowed);

  // non-destructive resizing of existing arrays.
  // keeps resized arrays in appropriate registry.

  // compare new with old registry--did we really switch?
  umRegistry<T>* pRegOld = m_pReg;

#if defined(_DEBUG) || defined(DEBUG)
  //#####################################################
  bool bSwitched = false;
  if (&s_Reg1 == m_pReg)
  { 
    // currently in s_Reg1: fixed (small)
    if (N <= iReg_small) {
      umTRC(1, "switch_Registry: small    (no change)\n");
    } else if (N <= iReg_med) {
      umTRC(1, "switch_Registry: small    --> medium\n");
      bSwitched = true;
    } else {
      umTRC(1, "switch_Registry: small    --> dynamic\n");
      bSwitched = true;
    }
  }
  else if (&s_Reg2 == m_pReg)
  {
    // currently in s_Reg2: fixed (medium)
    if (N <= iReg_small) {
      umTRC(1, "switch_Registry: medium   --> small\n");
      bSwitched = true;
    } else if (N <= iReg_med) {
      umTRC(1, "switch_Registry: medium   (no change)\n");
    } else {
      umTRC(1, "switch_Registry: medium   --> dynamic\n");
      bSwitched = true;
    }
  }
  else
  {
    // currently in s_Reg2: dynamic
    if (N <= iReg_small) {
      umTRC(1, "switch_Registry: dynamic  --> small\n");
      bSwitched = true;
    } else if (N <= iReg_med) {
      umTRC(1, "switch_Registry: dynamic  --> medium\n");
      bSwitched = true;
    } else {
      umTRC(1, "switch_Registry: dynamic  (no change)\n");
    }
  }
  //#####################################################
#endif

  // constructor selects correct registry
  Vector<T> *tmp = new Vector<T>(N, ZERO, OBJ_temp, "switching");

  int L=std::min(size(), N);  // how much data to copy?
  tmp->copy(L, data());       // copy existing data
  (*this) = (*tmp);           // switch ownership, delete tmp

  assert(reg_ok());
  return (pRegOld == m_pReg) ? false : true;
}


//---------------------------------------------------------
template <typename T> inline
bool Vector<T>::reg_ok() const
//---------------------------------------------------------
{
  // if (m_borrowed) { return false; }
  if (!m_pReg) { return false; }
  if (!v_    ) { return false; }

  if ((m_pReg != &s_Reg1) && 
      (m_pReg != &s_Reg2) && 
      (m_pReg != &s_Reg3)) { return false; }

  bool bOK = m_pReg->check_alloc(v_, m_id, m_Len);
  return bOK;
}




//---------------------------------------------------------
template <typename T> inline
void Vector<T>::compact() const
//---------------------------------------------------------
{
//s_Reg1.compact();  // no change to fixed-size registry
//s_Reg2.compact();  // no change to fixed-size registry
  s_Reg3.compact();  // release all unused allocations
}



///////////////////////////////////////////////////////////
//
// copy/load
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <> inline  // specialization for T=double
DVec& DVec::copy(const double *vec)
{
  // Overwrite entire vector (assume vec has enough values).
  // call BLAS dcopy()
  COPY(m_Len, (const double*)vec, 1, (double*)v_, 1);
  return (*this);
}

template <> inline  // specialization for T=double
DVec& DVec::copy(int N, const double *vec)
//---------------------------------------------------------
{
  if (N > m_Len) { this->resize(N, false); }

  // Overwrite 1st N entries (assume vec has >= N values).
  // call BLAS dcopy()
  COPY(N, (const double*)vec, 1, (double*)v_, 1);
  return (*this);
}



//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::copy(const T *vec)
//---------------------------------------------------------
{
  // Assumes vec has enough values. TODO: memcpy() ?
  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i]=vec[i]; v_[i+1]=vec[i+1]; v_[i+2]=vec[i+2]; v_[i+3]=vec[i+3];
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) { v_[i] = vec[i]; }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::copy(int N, const T *vec)
//---------------------------------------------------------
{
  // Assumes vec has enough values.

  if (N > m_Len) { this->resize(N, false); }

  if (N == m_Len) {
    // overwrite entire vector
    return copy(vec);
  } else {
    // overwrite first N entries (of zero-based data)
    for (int i=1; i<=N; ++i) { 
      (*this)(i)=vec[i-1]; 
    }
    return (*this);
  }
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::copy(int N, const Vector<T>& V)
//---------------------------------------------------------
{
  // assume arg has enough values.
  assert(N <= V.size());

  if (N >= m_Len) {
    // replace vector
    (*this) = V;
  } else {
    // overwrite first N entries
    copy(N, V.data());
    // clean up temporaries
    if (V.get_mode() == OBJ_temp) { delete (&V); }
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::load(int N, const char *sdata)
//---------------------------------------------------------
{
  // load ASCII data
  resize(N);
  try {
    std::istringstream ins(sdata);
    for (int i=0; i<N; ++i)
      ins >> v_[i];
  } catch(...) { 
    umERROR("Vector<T>::load(N, char *s)", "problem parsing data.");
  }
}


template <typename T> inline
void Vector<T>::set(const T x1, const T x2)
{
  resize(2);  vm1_[1]=x1; vm1_[2]=x2;
}

template <typename T> inline
void Vector<T>::set(const T x1, const T x2, const T x3)
{
  resize(3);  vm1_[1]=x1; vm1_[2]=x2; vm1_[3]=x3;
}


template <typename T> inline
void Vector<T>::set(const T x1, const T x2, const T x3, const T x4)
{
  resize(4);  vm1_[1]=x1; vm1_[2]=x2; vm1_[3]=x3; vm1_[4]=x4;
}



///////////////////////////////////////////////////////////
//
// assignment
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator=(const T &x)
//---------------------------------------------------------
{ 
  fill(x);  
  return (*this);
}


//---------------------------------------------------------
// enable assignment of IVec to DVec
template <typename T> inline
Vector<T>& Vector<T>::assign(const IVec &B)
//---------------------------------------------------------
{
  int len=B.size();
  if (this->m_name=="vec" || this->m_name.empty()) 
    this->m_name=B.name();

  this->resize(len);          // resize array of T
  const int* p = B.data();    // load int data as T

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    this->v_[i  ]=T(p[i  ]); this->v_[i+1]=T(p[i+1]);
    this->v_[i+2]=T(p[i+2]); this->v_[i+3]=T(p[i+3]);
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) { this->v_[i] = T(p[i]); }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*this);
}


//---------------------------------------------------------
// enable assignment of DVec to IVec
template <typename T> inline
Vector<T>& Vector<T>::assign(const DVec &B)
//---------------------------------------------------------
{
  int len=B.size();
  if (this->m_name=="vec" || this->m_name.empty()) 
    this->m_name=B.name();

  this->resize(len);            // resize array of T
  const double* p = B.data();   // load double data as T

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    this->v_[i  ]=T(p[i  ]); this->v_[i+1]=T(p[i+1]);
    this->v_[i+2]=T(p[i+2]); this->v_[i+3]=T(p[i+3]);
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) { this->v_[i] = T(p[i]); }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator=(const Vector<T> &B)
//---------------------------------------------------------
{
  if (this->v_ == B.v_) {
    // if both vectors are empty, then check for 
    // NULL to avoid leaking empty OBJ_temp's
    if (NULL != this->v_) {
      return (*this);
    }
  }

  int N = B.size();
  if (m_name=="vec" || m_name=="mat" || m_name.empty()) 
    m_name=B.name();

  if (N<1) {
    // B is empty.  Delete if OBJ_temp
    if (OBJ_temp == B.get_mode()) { 
      delete (&B); 
    }
    this->destroy();    // reset to empty array
  }

  else if (this->m_borrowed)
  {
    // This Vector wraps an external array.  If lengths
    // match, deep copy the data to the external array:
    if (m_Len == B.m_Len) {
      copy(B.v_);   // deep copy into external array
    } else {
      umERROR("Vector::operator=(Vector&)", 
        "When assigning an array to a borrowed  \n"
        "array, lengths must match exactly:     \n"
        "here the borrowed array has length:  %d\n"
        " the array being copied has length:  %d", m_Len, B.m_Len);
    }

    // If B was a temporary object, delete it.
    if (B.get_mode() == OBJ_temp) {delete &B;}
  }

  else if (OBJ_temp != B.get_mode())
  {
    // Deep copy operation
    if (m_Len == B.m_Len) {
      copy(B.v_);                 // re-use currrent structure 
    } else {
      destroy();
      initialize(B.m_Len, false); // re-use, with housekeeping 
      copy(B.v_);
    }
  }
  else
  {
    // B is a "temporary" object: transfer ownership

    Vector<T> &Bref = const_cast<Vector<T>& >(B);

    if (reg_ok()) 
    {
      // free current allocation
      assert(m_pReg->check_alloc(v_, m_id, m_Len));
      m_pReg->free_alloc(v_, m_id); // free current allocation
    }

    m_Len  = Bref.size();   // copy length
    m_id   = Bref.get_id(); // take B's slot in the registry
    v_     = Bref.get_v();  // take B's array
    vm1_   = v_ - 1;        // adjust 1-based pointer

    if (B.is_borrowed()) {
      umWARNING("Vector<T>::operator=(B)", "check transfer of borrowed allocation");
      this->m_borrowed = true;
    } else {
      m_pReg = Bref.get_reg();  // copy address of B's registry
      assert(m_pReg->check_alloc(v_, m_id, m_Len));
    }

    Bref.set_v(NULL);     // detach B from its array
    Bref.set_id(-1);      // mark as unregistered
    Bref.m_Len = 0;       // mark as empty (for debug trace)
    delete &B;            // delete temporary object
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::append(const T& x)
//---------------------------------------------------------
{
  // append a single value, retaining existing values

  assert(!m_borrowed);  // avoid changing "borrowed" structure
  if (!ok()) {
    resize(1); v_[0]=x;
  } else {
    OBJ_mode old_md = this->get_mode(); // (*this) may be OBJ_temp
    this->set_mode(OBJ_real);           // stop concat deleting (*this).
    (*this) = concat((*this), x);       // call global concat(A,x)
    this->set_mode(old_md);             // restore original mode
  }
  return (*this);
}



//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::append(const Vector<T>& B)
//---------------------------------------------------------
{
  // append a second vector, retaining existing values

  assert(!m_borrowed);  // avoid changing "borrowed" structure
  if (!ok()) {
    (*this)=B;
  } else {
    OBJ_mode old_md = this->get_mode(); // (*this) may be OBJ_temp
    this->set_mode(OBJ_real);           // stop concat deleting (*this).
    (*this) = concat((*this), B);       // call global concat(A,B)
    this->set_mode(old_md);             // restore original mode
  }
  return (*this);
}



///////////////////////////////////////////////////////////
//
// utilities
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
int Vector<T>::get_nnz(T tol) const
//---------------------------------------------------------
{
  int nnz = 0;
  for (int i=0; i<m_Len; ++i) {
    if (std::abs(v_[i]) > tol) { ++nnz; }
  }
  return nnz;
} 



//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::zeros(int N)
//---------------------------------------------------------
{ 
  if (N>0) {resize(N, true, ZERO);}  // resize, then fill
  else     {fill(ZERO);}
  return (*this); 
} 


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::ones(int N)
//---------------------------------------------------------
{ 
  if (N>0) {resize(N, true, ONE);}    // resize, then fill
  else     {fill(ONE);}
  return (*this); 
} 


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::fill(const T &x)
//---------------------------------------------------------
{
  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] = x;
    v_[i+1] = x;
    v_[i+2] = x;
    v_[i+3] = x; 
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] = x;
  }
  return (*this); 
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator!() const
//---------------------------------------------------------
{
  // returns a "Boolean" result: all non-zeros in 
  // this are "toggled" to zero, and vice-versa.

  // initialize result to ZERO, then toggle...
  Vector<T>* tmp = new Vector<T>(m_Len, ZERO, OBJ_temp);
  for (int i=0; i<m_Len; ++i) {
    if (ZERO == this->v_[i]) { 
      tmp->v_[i] = ONE; 
    }
  }
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::set_abs()
//---------------------------------------------------------
{
  for (int i=0; i<m_Len; ++i) {v_[i] = std::abs(v_[i]);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::set_min_val(double dtol)
//---------------------------------------------------------
{
  for (int i=0; i<m_Len; ++i) {
    if (v_[i] < dtol) {
      v_[i] = dtol;
    }
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::SQRT()
//---------------------------------------------------------
{
  // apply(::sqrt);
  for (int i=0; i<m_Len; ++i) {
    if (v_[i]>0) {
      v_[i] = (T) sqrt(double(v_[i]));
    } else {
      v_[i] = ZERO;
    }
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::SQR()
//---------------------------------------------------------
{
  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // umroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] *= v_[i  ];
    v_[i+1] *= v_[i+1];
    v_[i+2] *= v_[i+2];
    v_[i+3] *= v_[i+3];
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] *= v_[i];
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::max_val() const
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  T res = v_[0];
  for (int i=1; i<m_Len; ++i) { res = std::max(res, v_[i]); }
  if (OBJ_temp == this->m_mode) {delete this;}
  return res;
}

//---------------------------------------------------------
template <typename T> inline
T Vector<T>::min_val() const
//---------------------------------------------------------
{ 
  if (m_Len<1) return ZERO;
  T res = v_[0];
  for (int i=1; i<m_Len; ++i) { res = std::min(res, v_[i]); }
  if (OBJ_temp == this->m_mode) {delete this;}
  return res;
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::max_val(T& res, int& id) const
//---------------------------------------------------------
{ 
  if (m_Len<1) { res = 0.0; id = -1; return; }

  // work on local vars
  T tres = vm1_[1]; int tid=1;
  for (int i=2; i<=m_Len; ++i) {
    if (vm1_[i] > tres) { tres = vm1_[i];  tid = i; }
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  res=tres; id=tid;   // update user args
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::min_val(T& res, int& id) const
//---------------------------------------------------------
{ 
  if (m_Len<1) { res = 0.0; id = -1; return; }

  // work on local vars
  T tres = vm1_[1]; int tid=1;
  for (int i=2; i<=m_Len; ++i) {
    if (vm1_[i] < tres) { tres = vm1_[i];  tid = i; }
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  res=tres; id=tid;   // update user args
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::max_val_abs() const 
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  T res = std::abs(v_[0]);
  for (int i=1; i<m_Len; ++i) { res = std::max(res, std::abs(v_[i])); }
  if (OBJ_temp == this->m_mode) {delete this;}
  return res;
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::min_val_abs() const 
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  T res = std::abs(v_[0]);
  for (int i=1; i<m_Len; ++i) { res = std::min(res, std::abs(v_[i])); }
  if (OBJ_temp == this->m_mode) {delete this;}
  return res;
}



//---------------------------------------------------------
template <> inline  // specialization for T=double
double Vector<double>::sum() const
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  double one = 1.0;
  // call BLAS ddot()
  double val = DOT(m_Len, &one, 0, (const double*)v_, 1);
  if (OBJ_temp == this->m_mode) {delete this;}
  return val;
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::sum() const
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;

  // accumulate
  T r0=ZERO, r1=ZERO, r2=ZERO, r3=ZERO;
  for (i=0; i<N4; i+=4) {
    r0 += v_[i  ];
    r1 += v_[i+1];
    r2 += v_[i+2];
    r3 += v_[i+3];
  }
  r0 += (r1+r2+r3);
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    r0 += v_[i];
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return r0;
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::sumsquares() const
//---------------------------------------------------------
{
  // Return the sum of squares of all elements.
  T ret = inner(*this);
  return ret;
}


//---------------------------------------------------------
template <> inline  // specialization for T=double
double Vector<double>::inner(const Vector<double> &B) const
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  assert(B.size() >= m_Len);
  // call BLAS ddot()
  double ret = DOT(m_Len, this->v_, 1, B.data(), 1);
  if (OBJ_temp == B.get_mode() ) {delete (&B);}
  if (OBJ_temp == this->m_mode) {delete this;}
  return ret;
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::inner(const Vector<T> &B) const
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  int N = B.size();
  assert(N >= m_Len);
  // pass a pointer into B's data
  T ret = inner(N, B.data());
  if (OBJ_temp == B.getmode() ) {delete (&B);}
  return ret;
}


//---------------------------------------------------------
template <typename T> inline
T Vector<T>::inner(int N, const T* p) const
//---------------------------------------------------------
{
  if (m_Len<1) return ZERO;
  assert(N >= m_Len);

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;

  // accumulate
  T r0=ZERO, r1=ZERO, r2=ZERO, r3=ZERO;
  for (i=0; i<N4; i+=4) {
    r0 += ( v_[i  ] * p[i  ] );
    r1 += ( v_[i+1] * p[i+1] );
    r2 += ( v_[i+2] * p[i+2] );
    r3 += ( v_[i+3] * p[i+3] );
  }
  r0 += (r1+r2+r3);
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    r0 += ( v_[i] * p[i] );
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return r0;
}


//---------------------------------------------------------
template <typename T> inline
double Vector<T>::mean() const
//---------------------------------------------------------
{
  // Return mean (average) of all elements.
  if (m_Len<1) return 0.0;
  double ret = ((double)sum()) / (double)m_Len;
  if (OBJ_temp == this->m_mode) {delete this;}
  return ret;
}



///////////////////////////////////////////////////////////
//
// vector norms
//
///////////////////////////////////////////////////////////


template <typename T> inline
double Vector<T>::norm(norm_type lp) const
{
  double nrx = 0.0;
  if (lp == NORM_L1 || lp == NORM_l1) 
  {
    for (int i=0; i<m_Len; ++i) {nrx += std::abs(this->v_[i]);}
    if (lp == NORM_L1) nrx /= double(m_Len);
  } 
  else if (lp == BLAS_L2 || lp == BLAS_l2) 
  {
  //if (sizeof(T) == sizeof(double))
  //nrx = NRM2(m_Len, this->v_, 1);
    nrx = sqrt(double(sumsquares()));
  } 
  else // if (lp == NORM_Linf) 
  {
    double tmp=0.0, tmax = -1.0;
    for (int i=0; i<m_Len; ++i) {
      tmp = std::abs(v_[i]);
      if (tmp>tmax) tmax = tmp;
    }
    nrx = tmax;
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return nrx;
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::normalize()
//---------------------------------------------------------
{
  div_val(norm(NORM_Linf));   // divide by |max| element
//div_val(norm(BLAS_L2));     // divide by Euclidean length
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::randomize(double from, double to, int N)
//---------------------------------------------------------
{
  if ((N>0) && (N!=m_Len)) {
    // allow (optional) simultaneous resize 
    resize(N, false);
  }

  // random numbers clamped within 
  // user-defined range:
  for (int i=0; i<m_Len; ++i) {
    v_[i] = umRAND::get_rand(from, to);
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::rand(double from, double to, int N)
//---------------------------------------------------------
{
  return randomize(from, to, N);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::randnorm()
//---------------------------------------------------------
{
  randomize(0.0, 1.0);
  normalize();
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::apply(FuncPtr fptr)
//---------------------------------------------------------
{
  // apply single arg function to all elements (e.g. sin, exp)
  for (int i=0; i<m_Len; ++i) {
    v_[i] = fptr( v_[i] );
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::exp_val()
//---------------------------------------------------------
{
  apply(::exp);
  return (*this); 
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::log()
//---------------------------------------------------------
{
  if (this->min_val()<=0.0) {
    umWARNING("Vector<T>::log()", "negative elements passed to log()");
  } 
  apply(::log);
  return (*this); 
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::chop(double eps)
//---------------------------------------------------------
{
  // set "small" elements to 0.0;
  for (int i=0; i<m_Len; ++i) {
    if (std::abs(v_[i]) < eps) {
      v_[i] = ZERO;
    }
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
bool Vector<T>::same_object(const Vector<T>& B) const
//---------------------------------------------------------
{
  if (this != &B) { 
    return false; 
  } else {
    return true; 
  }
}


//---------------------------------------------------------
template <typename T> inline
bool Vector<T>::operator==(const Vector<T>& B) const
//---------------------------------------------------------
{
  // same vector?
  if (v_ == B.v_) { return true; } // NB: NULL==NULL ?
  int len = B.size();
  if (len != m_Len) { return false; }
  for (int i=0; i<len; ++i) {
    if ( std::abs( v_[i] - B[i] ) > m_EqTol ) {
      // different value
      return false;
    }
  }
  return true;
}




///////////////////////////////////////////////////////////
//
// member Boolean operations
//
///////////////////////////////////////////////////////////


template <typename T> inline
Vector<T>& Vector<T>::eq(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = (v(i) == val) ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(x==val)");
  for (int i=1; i<=m_Len; ++i) { if (this->vm1_[i] == val) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::le(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = (v(i) <= val) ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(x<=val)");
  for (int i=1; i<=m_Len; ++i) { if (this->vm1_[i] <= val) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::lt(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = (v(i) < val) ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(x<val)");
  for (int i=1; i<=m_Len; ++i) { if (this->vm1_[i] < val) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::lt_abs(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = |v(i)| < val ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(|x|<tol)");
  T fval = std::abs(val);
  for (int i=1; i<=m_Len; ++i) { if (std::abs(vm1_[i]) < fval) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::ge(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = (v(i) >= val) ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(x>=val)");
  for (int i=1; i<=m_Len; ++i) { if (this->vm1_[i] >= val) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::gt(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = (v(i) > val) ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(x>val)");
  for (int i=1; i<=m_Len; ++i) { if (this->vm1_[i] > val) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::gt_abs(T val) const
{
  // return a vector of [0/1] values:
  //   r(i) = |v(i)| > val ? 1:0

  // initialize with zeros.
  Vector<T> *tmp=new Vector<T>(m_Len, ZERO, OBJ_temp, "(|x|>tol)");
  T fval = std::abs(val);
  for (int i=1; i<=m_Len; ++i) { if (std::abs(vm1_[i]) > fval) { tmp->vm1_[i] = this->ONE; } }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}



template <typename T> inline
bool Vector<T>::any(const T& val) const
{
  // return true if any instance of the 
  // given value is found in the array

  for (int i=1; i<=m_Len; ++i) { 
    if (val == this->vm1_[i]) { 
      return true; 
    } 
  }
  return false;
}


/////////////////////////////////////////////////////////
//
// numerical routines.
//
// Default routines are provided to accommodate all 
// data types, plus calls to BLAS for T=<double>.
//
/////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::add_val(const T &x)
//---------------------------------------------------------
{
  // Add a value to all elements.

  if (ZERO==x) { return; }

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] += x;
    v_[i+1] += x;
    v_[i+2] += x;
    v_[i+3] += x;
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] += x;
  }
}


//---------------------------------------------------------
template <> inline  // specialization for T=double
void Vector<double>::mult_val(const double &x)
//---------------------------------------------------------
{
  // handle special cases
  if (ZERO==x) {fill(ZERO); return;}
  if (ONE ==x) { return; }

  // call BLAS dscal()
  SCAL(m_Len, x, v_, 1);
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::mult_val(const T &x)
//---------------------------------------------------------
{
  // Multiply all elements by a value.

  // handle special cases
  if (ZERO==x) {fill(ZERO); return;}
  if (ONE ==x) { return; }

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] *= x;
    v_[i+1] *= x;
    v_[i+2] *= x;
    v_[i+3] *= x;
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] *= x;
  }
}


//---------------------------------------------------------
template <> inline  // specialization for T=double
void Vector<double>::div_val(const double &x)
//---------------------------------------------------------
{
  // Divide all elements by a value.

  if (1.0==x) { return; }
  if (0.0==x) throw "division by zero";

  double recip = 1.0/x;   // convert fp divisions 
  mult_val(recip);        // ... to multiplications
} 


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::div_val(const T &x)
//---------------------------------------------------------
{
  // Divide all elements by a value.

  // handle special cases
  if (ONE==x) { return; }
  if (T(0)==x) throw "division by zero";

  // handle integer division
  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] /= x;
    v_[i+1] /= x;
    v_[i+2] /= x;
    v_[i+3] /= x;
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] /= x;
  }
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::pow_val(const T &x)
//---------------------------------------------------------
{
  // Raise all elements to a power.
  
  // handle special cases
  if (ZERO==x) {fill(ONE); return;} // all elements become 1
  if (ONE ==x) { return; }          // array remains the same

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] = pow(v_[i  ], x);
    v_[i+1] = pow(v_[i+1], x);
    v_[i+2] = pow(v_[i+2], x);
    v_[i+3] = pow(v_[i+3], x);
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] = pow(v_[i], x);
  }
}


//---------------------------------------------------------
template <> inline  // specialization for T=double
Vector<double>& Vector<double>::operator+=(const Vector<double>& B)
//---------------------------------------------------------
{
  assert(B.size() >= m_Len);    // B may be longer than A
  const double* p = B.data();   // operate on the base array
  // call BLAS daxpy()
  AXPY(m_Len, 1.0, (double*)p, 1, v_, 1);

  // if B is temporary, delete it.
  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator+=(const Vector<T>& B)
//---------------------------------------------------------
{
  // element-by-element --> A .+ B

  assert(B.size() >= m_Len);    // B may be longer than A
  const T* p = B.data();        // operate on the base array

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] += p[i  ];
    v_[i+1] += p[i+1];
    v_[i+2] += p[i+2];
    v_[i+3] += p[i+3];
  }

  for (i=N4; i<m_Len; ++i)
    v_[i] += p[i];

  // if B is temporary, delete it.
  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*this);
}


//---------------------------------------------------------
template <> inline  // specialization for T=double
Vector<double>& Vector<double>::operator-=(const Vector<double>& B)
//---------------------------------------------------------
{
  assert(B.size() >= m_Len);    // B may be longer than A
  const double *p = B.data();   // operate on the base array
  // call BLAS daxpy()
  AXPY(m_Len, -1.0, (double*)p, 1, v_, 1);

  // if B is temporary, delete it.
  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator-=(const Vector<T>& B)
//---------------------------------------------------------
{
  // element-by-element --> A .- B

  assert(B.size() >= m_Len);   // B may be longer than A
  const T* p = B.data();       // operate on the base array

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] -= p[i  ];
    v_[i+1] -= p[i+1];
    v_[i+2] -= p[i+2];
    v_[i+3] -= p[i+3];
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] -= p[i];
  }

  // if B is temporary, delete it.
  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator*=(const T* p)
//---------------------------------------------------------
{
  // element-by-element --> A .* data

  int Nmod4 = m_Len & 3;
  int N4 = m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    v_[i  ] *= p[i  ];
    v_[i+1] *= p[i+1];
    v_[i+2] *= p[i+2];
    v_[i+3] *= p[i+3];
  }
  // cleanup
  for (i=N4; i<m_Len; ++i) {
    v_[i] *= p[i];
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator*=(const Vector<T>& B)
//---------------------------------------------------------
{
  // element-by-element --> A .* B

  assert(B.size() >= m_Len);  // B may be longer than A
  const T* p = B.data();      // operate on the base array
  (*this) *= (p);

  // if B is temporary, delete it.
  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::operator/=(const Vector<T>& B)
//---------------------------------------------------------
{
  // element-by-element --> A ./ B

  if (m_Len>0) 
  {
    assert(B.size() >= m_Len);      // B may be longer than A
    assert(B.min_val_abs()>0.0);    // DEBUG check for zero divisor

    const T* p = B.data();          // operate on the base array

    int Nmod4 = m_Len & 3;
    int N4 = m_Len - Nmod4, i=0;
    // unroll
    for (i=0; i<N4; i+=4) {
      v_[i  ] /= p[i  ];
      v_[i+1] /= p[i+1];
      v_[i+2] /= p[i+2];
      v_[i+3] /= p[i+3];
    }
    // cleanup
    for (i=N4; i<m_Len; ++i) {
      v_[i] /= p[i];
    }
  }

  // if B is temporary, delete it.
  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::dd(const Vector<T> &B) const
//---------------------------------------------------------
{
  // (*this) is not changed: C = A ./ B

#ifndef NDEBUG
  // FIXME: tmp ctor may delete (*this)
  // if (OBJ_temp == m_mode) {umWARNING("Vector<T>::dd(B)", "check for side-effects");}
#endif

  std::string sz; tmp_op_name(name(), "./", B.name(), sz);
  Vector<T> *tmp=new Vector<T>((*this), OBJ_temp, sz.c_str());
  (*tmp)/=(B);
  return (*tmp);
}
  

//---------------------------------------------------------
template <typename T> inline
Vector<T>& Vector<T>::dm(const Vector<T> &B) const
//---------------------------------------------------------
{
  // (*this) is not changed: C = A .* B

#ifndef NDEBUG
  // FIXME: tmp ctor may delete (*this)
  //if (OBJ_temp == m_mode) {umWARNING("Vector<T>::dm(B)", "check for side-effects");}
#endif

  std::string sz; tmp_op_name(name(), ".*", B.name(), sz);
  Vector<T> *tmp=new Vector<T>((*this), OBJ_temp, sz.c_str());
  (*tmp)*=(B);
  return (*tmp);
}


//---------------------------------------------------------
template <> inline  // specialization for T=double
void Vector<double>::axp_y(const double& alpha, const Vector<double>& X)
//---------------------------------------------------------
{
  // BLAS example:  this += alpha*X
  assert(this->size() == X.size());
  AXPY(m_Len, alpha, (double*)X.data(), 1, this->v_, 1);

  // if X is temporary, delete it.
  if (X.get_mode() == OBJ_temp) {delete (&X);}
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::axp_y(const T& alpha, const Vector<T>& X)
//---------------------------------------------------------
{
  (*this) += (alpha*X);
}



//---------------------------------------------------------
template <> inline  // specialization for T=double
void Vector<double>::axp_y
(
  const double& alpha, 
  const Vector<double>& X,
  const Vector<double>& Y
)
//---------------------------------------------------------
{
  // BLAS example:  this = alpha*X + y
  assert(Y.size() == X.size());

  (*this) = Y;    // copy Y, then add alpha*X,
  AXPY(m_Len, alpha, (double*)X.data(), 1, this->v_, 1);

  // operator=() above deletes temporary Y
  // if X is temporary, delete it here.
  if (X.get_mode() == OBJ_temp) {delete (&X);}
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::axp_y
(
  const T& alpha, 
  const Vector<T>& X, 
  const Vector<T>& Y
)
//---------------------------------------------------------
{
  assert(Y.size() == X.size());
  (*this) = Y;            // copy Y, 
  (*this) += (alpha*X);   // then add alpha*X
}




///////////////////////////////////////////////////////////
//
// I/O: vector input/output
//
///////////////////////////////////////////////////////////


// Write a format that can be read by Vector<T>
template <typename T> 
std::ostream& operator<<(std::ostream &s, const Vector<T> &A)
{
  int N = A.size();
  s << N << "\n";
  for (int i=0; i<N; i++) { s << A[i] << " \n"; }
  s << std::endl;
  return s;
}


template <typename T> 
std::istream & operator>>(std::istream &s, Vector<T> &A)
{
  int N = 0;
  s >> N;
  if (A.size() != N) { A.resize(N); }
  for (int i=0; i<N; i++) { s >> A[i]; }
  return s;
}


//---------------------------------------------------------
template <typename T>
void Vector<T>::print
(
  FILE* os, 
  const char* msg,
  const char* fmt,  // [%d|%e|%lf|%g]
  int  prec,        // sig.figs|dec.places
  int  wdth,        // output spacing [12]
  bool vert,        // vertical/horizontal
  int  nline,       // entries per line
  int  nv           // num vals to write (0 --> all)
) const
//---------------------------------------------------------
{
  static char buf[20] = {""};

  // write min(nv,len) vals
  int len = this->size();
  if (nv > 0) len = (nv<=len ? nv : len);

  if (msg) { fprintf(os, "%s\n", msg); }
  fprintf(os, "(%d)\n", len);

  if (1==nline || vert) {

    // handle integer data types
    if (sizeof(T) == sizeof(double))
         sprintf(buf, "%c%d.%d%s\n", '%',wdth, prec,fmt);
    else sprintf(buf, "%c%dd\n", '%',wdth);

    for (int i=0; i<len; ++i) {
      fprintf(os, buf, T(v_[i]));
    }
  } else {

    // handle integer data types
    if (sizeof(T) == sizeof(double))
         sprintf(buf, "%c%d.%d%s ", '%',wdth, prec,fmt);
    else sprintf(buf, "%c%dd ", '%',wdth);

    for (int i=0; i<len; ++i) 
    {
      fprintf(os, buf, T(v_[i]));
      if (i && (0 == ((i+1)%nline)))
        fprintf(os, "\n");
    }
  }
  fprintf(os, "\n");
  fflush(os);
}


//---------------------------------------------------------
template <typename T>
void Vector<T>::print_STREAM 
(
  std::ostream& os, 
  const char* msg,
  char fmt,         // [%d|%e|%lf|%g]
  int  prec,        // sig.figs|dec.places
  int  wdth,        // output spacing [12]
  bool vert,        // vertical/horizontal
  int  nline,       // entries per line
  int  nv           // num vals to write (0 --> all)
) const
//---------------------------------------------------------
{
  // save stream settings
  std::ios_base::fmtflags flgs = os.flags();
  if ('E' == toupper(fmt)) 
  { os << std::setiosflags(std::ios::scientific); } 
  else if ('F' == toupper(fmt)) 
    //{ os << std::setiosflags(std::ios::fixed); }
  { os << std::setiosflags(std::ios::fixed|std::ios::showpoint); }
  else { ; } // general format
  //os << std::setiosflags();} 
  os << std::setprecision(prec);

  // write min(nv,len) vals
  int len = this->size();
  if (nv > 0) len = (nv<=len ? nv : len);

  if (msg) { os << msg << "\n"; }
  os << "(" << len << ")\n";

  if (1==nline || vert) {
    for (int i=0; i<len; ++i) {os<<std::setw(wdth)<< T(v_[i]) <<"\n"; }
  } else {
    for (int i=0; i<len; ++i) {
      os  << std::setw(wdth) << T(v_[i]) << " "; 
      if (i && (0 == ((i+1)%nline)))
        os << "\n";
    }
  }
  os << std::endl;
  os.setf(flgs);  // restore stream settings
}


//---------------------------------------------------------
template <typename T>
std::string Vector<T>::display
(
  const char* msg,
  char fmt,         // [%d|%e|%lf|%g]
  int  prec,        // sig.figs|dec.places
  int  wdth,        // output spacing [12]
  bool vert,        // vertical/horizontal
  int  nline,       // entries per line
  int  nv           // num vals to write (0 --> all)
) const
//---------------------------------------------------------
{
  std::stringstream ss;

  if ('E' == toupper(fmt)) 
  { ss << std::setiosflags(std::ios::scientific); } 
  else if ('F' == toupper(fmt)) 
  { ss << std::setiosflags(std::ios::fixed|std::ios::showpoint); }
  else { ; } // general format
  ss << std::setprecision(prec);

  // write min(nv,len) vals
  int len = this->size();
  if (nv > 0) len = (nv<=len ? nv : len);

  if (msg &&  vert) { ss << msg << "\n"; }
  else if (msg)     { ss << msg << "  "; }

  ss << "(" << len << "):  ";
  if (vert) ss << '\n';

  if (1==nline || vert) {
    for (int i=0; i<len; ++i) {ss<<std::setw(wdth)<< T(v_[i]) <<"\n"; }
  } else {
    for (int i=0; i<len; ++i) {
      ss  << std::setw(wdth) << T(v_[i]) << ","; 
      if (i && (0 == ((i+1)%nline)))
        ss << "\n";
    }
  }
  ss << std::ends;
  return ss.str();
}


// I/O: Matlab (binary) .mat format
//#######################################
// Define Matlab constants
//#######################################
#define COL_ORDER   0   // col-major data
#define ROW_ORDER   1   // row-major data
#define DOUBLE_PREC 0   // double precision
#define SINGLE_PREC 1   // single precision

#define MACH_ID     0   // 80x87 format   (small-endian?)
//#define MACH_ID   1   // 6888x format   (big-endian?)
#define ORDER       COL_ORDER
#define PRECISION   DOUBLE_PREC
//#######################################

//---------------------------------------------------------
template <typename T>
void Vector<T>::m_save_v(FILE* fp, const char* name) const
//---------------------------------------------------------
{
  // Save vector as (M,1) array in Matlab ".mat" format (BINARY)
  // Note: For Win32, fp MUST be opened in "wb" mode

  if (!this->ok()) { 
    umWARNING("Vector<T>::m_save_v","Empty vector"); 
    return; 
  }

  int M = this->size();

  umMATLAB mat;
  mat.type = 1000*MACH_ID + 100*ORDER + 10*PRECISION + 0;
  mat.m = M;    // save as Mx1 array
  mat.n = 1;    // one column
  mat.imag = 0; // FALSE;
  mat.namlen = (name? (long)(strlen(name)+1) : (long)1);

  // write header
  fwrite(&mat,sizeof(umMATLAB),1,fp);
  // write name
  if ( name == (char *)NULL )
    fwrite("",sizeof(char),1,fp);
  else
    fwrite(name,sizeof(char),(int)(mat.namlen),fp);

  // write actual data
  if (sizeof(T) == sizeof(double)) {
    fwrite(v_,sizeof(double),M,fp);
  } else {
    for (int i=0; i<M; ++i) {
      fwrite((double*)(&v_[i]), sizeof(double),1,fp);
    }
  }
}


//---------------------------------------------------------
template <typename T>
void Vector<T>::m_load_v(FILE *fp, char **name)
//---------------------------------------------------------
{
  // loads a ".mat" file variable (MATLAB format)
  // imaginary parts ignored 

  umMATLAB mat;

  if (fread(&mat,sizeof(umMATLAB),1,fp) != 1)
    umERROR("E_FORMAT","m_load_v");
  if (mat.type >= 10000)          // don't load a sparse matrix
    umERROR("E_FORMAT","m_load_v");

  int m_flag = (mat.type/1000) % 10;
  int o_flag = (mat.type/100) % 10;
  int p_flag = (mat.type/10) % 10;
  int t_flag = (mat.type) % 10;
  if (m_flag != MACH_ID)          umERROR("E_FORMAT","m_load_v");
  if ( t_flag != 0 )              umERROR("E_FORMAT","m_load_v");
  if ( p_flag != DOUBLE_PREC && 
       p_flag != SINGLE_PREC )    umERROR("E_FORMAT","m_load_v");

  *name = (char *)malloc((unsigned)(mat.namlen)+1);
  if ( fread(*name,sizeof(char),(unsigned)(mat.namlen),fp) == 0 )
    umERROR("E_FORMAT","m_load_v");

  int M=mat.m, N=mat.n;
  this->resize(M*N);

  float  f_temp=0.0f;
  double d_temp=0.0;
  int i=0, iR=0, jC=0;

  if ((p_flag == DOUBLE_PREC) && (sizeof(T) == sizeof(double))) 
  {
    // read data directly into allocation
    fread(this->v_, sizeof(double),M*N,fp);
  }
  else
  {
    // load one element at a time
    if (p_flag == DOUBLE_PREC) {
      for (i=0; i<M*N; ++i) {
        fread(&d_temp,sizeof(double),1,fp);
        this->v_[i] = (T)(d_temp);          // cast double to <T>
      }
    } else {
      for (i=0; i<M*N; ++i) {
        fread(&f_temp,sizeof(float),1,fp);
        this->v_[i] = (T)(f_temp);          // cast float to <T>
      }
    }
  }

  // skip imaginary part
  if (mat.imag) 
  {
    if (p_flag == DOUBLE_PREC) {
      for (i=0; i<M*N; ++i) {fread(&d_temp,sizeof(double),1,fp);}
    } else {
      for (i=0; i<M*N; ++i) {fread(&f_temp,sizeof(float),1,fp);}
    }
  }
}

//#######################################
// Undefine Matlab constants
//#######################################
#undef COL_ORDER
#undef ROW_ORDER
#undef DOUBLE_PREC
#undef SINGLE_PREC

#undef MACH_ID
#undef ORDER
#undef PRECISION
//#######################################



///////////////////////////////////////////////////////////
//
// Globals: numeric operator overloads
//
///////////////////////////////////////////////////////////


// C = A + B
// C = A - B

template <typename T> inline 
Vector<T>& operator+(const Vector<T> &A, const Vector<T> &B) 
{
  std::string sz; tmp_op_name(A.name(),"+",B.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator-(const Vector<T> &A, const Vector<T> &B) 
{
  std::string sz; tmp_op_name(A.name(),"-",B.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}


// C = A + x
// C = x + A
// C = A - x
// C = x - A  (make vector X from scalar x)

template <typename T> inline 
Vector<T>& operator+(const Vector<T> &A, const T &x) 
{
  std::string sz; tmp_op_name(A.name(),"+","x", sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator+(const T &x, const Vector<T> &A) 
{
  std::string sz; tmp_op_name("x","+",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator-(const Vector<T> &A, const T &x) 
{
  std::string sz; tmp_op_name(A.name(),"-","x", sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= x;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator-(const T &x, const Vector<T> &A)
{ 
  // Create vec X filled with scalar x, then call X - A

  std::string sz; tmp_op_name("x","-",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(A.size(), x, OBJ_temp, sz.c_str());
  (*tmp) -= A;
  return (*tmp);
}



// C = A * x
// C = x * A
// C = A .* B   Note: element-by-element
// C = A ./ B   Note: element-by-element
// C = A / x
// C = x / A

template <typename T> inline 
Vector<T>& operator*(const Vector<T> &A, const T &x) 
{
  std::string sz; tmp_op_name(A.name(),"*","x", sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator*(const T &x, const Vector<T> &A) 
{
  std::string sz; tmp_op_name("x","*",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


/*
//---------------------------------------------------------
// specialization for T=double
inline DVec& operator*(const DVec& A, const DVec& B) 
//---------------------------------------------------------
{
  int n = A.size();
  assert(A.size() == n);
  // element-by-element --> A .* B
  // using BLAS2 symmetric banded mat*vec
  std::string sz; tmp_op_name(A.name(),"*",B.name(), sz);
//DVec *tmp=new DVec(A, OBJ_temp, sz.c_str());
  DVec *tmp=new DVec(n, sz.c_str(), OBJ_temp);
//dsbmv(uplo, n, kd, alpha, double *a,  lda, double *x, int incx, double beta, double *y, int incy);
  SBMV ('U',  n, 0,   1.0,  A.data(),     1,  B.data(),    1,        0.0,   tmp->data(),     1);
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}
*/

template <typename T> inline 
Vector<T>& operator*(const Vector<T> &A, const Vector<T> &B) 
{
  // element-by-element --> A .* B
  std::string sz; tmp_op_name(A.name(),"*",B.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= B;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator/(const Vector<T> &A, const Vector<T> &B) 
{ 
  // element-by-element --> A ./ B
  std::string sz; tmp_op_name(A.name(),"/",B.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= B;
  return (*tmp);
}


template <typename T> inline 
Vector<T>& operator/(const Vector<T> &A, const T &x) 
{
  std::string sz; tmp_op_name(A.name(),"/","x", sz);
  assert(std::abs(x) > (T)0);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= x;
  return (*tmp);
}


template <typename T> inline
Vector<T>& operator/(const T &x, const Vector<T> &A) 
{ 
  // create a vector filled with scalar x;  tmp = tmp./A
  std::string sz; tmp_op_name("x","/",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(A.size(), x, OBJ_temp, sz.c_str());
  (*tmp) /= A;
  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// Globals: double precision norms
//
///////////////////////////////////////////////////////////


inline double norm1(const DVec& V) 
{
  double nval = V.norm(NORM_l1);
//double nval = V.norm(NORM_L1); // scaled
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return nval;
}

inline double norm2(const DVec& V) 
{
  double nval = V.norm(BLAS_L2);
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return nval;
}

inline double norm_inf(const DVec& V) 
{
  double nval = V.norm(NORM_Linf);
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return nval;
}

inline double norm(const DVec& V) 
{
  return norm2(V);
}



///////////////////////////////////////////////////////////
//
// Globals: miscellaneous operations
//
///////////////////////////////////////////////////////////


// C = -A     : unary negation
// C = !A     : boolean not
// x = A.B    : inner product
// Z = aX+Y   : [*]axpy
//
// Y = apply(f, X);


// negation (unary operator)
template <typename T> inline 
Vector<T>& operator- (const Vector<T> &A) 
{
  std::string sz; tmp_op_name(" ","-",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= T(-1);
  return (*tmp);
}


// boolean (unary operator)
template <typename T> inline 
Vector<T>& operator! (const Vector<T> &A)
{
  std::string sz; tmp_op_name(" ","!",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  tmp->operator!();
  return (*tmp);
}


// x = A.B
template <typename T> inline 
T inner(const Vector<T> &A, const Vector<T> &B)
{
  T retval = A.inner(B);

  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }

  return retval;
}


// daxpy :  Z = aX+Y
template <typename T> inline 
Vector<T>& axp_y(const double alpha, const Vector<T> &X, const Vector<T> &Y)
{
  std::string sz; tmp_op_name(X.name(), "*a+", Y.name(), sz);

  Vector<T> *tmp=new Vector<T>(Y, OBJ_temp, sz.c_str());
  tmp->axp_y(alpha, X);
  return (*tmp);
}


// apply :  Y = = sin(X) = apply(sin, X);
template <typename T> inline 
Vector<T>& apply(FuncPtr fptr, const Vector<T> &X)
{
  char buf[100]={""};
  snprintf(buf, (size_t)90, "func(%s)", X.name());

  Vector<T> *tmp=new Vector<T>(X, OBJ_temp, buf);
  tmp->apply(fptr);
  return (*tmp);
}



/*
// overloads for integer arg
template <typename T> inline 
Vector<T>& operator/(const Vector<T> &A, int i) { return A / ((T)i); }
template <typename T> inline 
Vector<T>& operator/(int i, const Vector<T> &A) { return ((T)i) / A; }

template <typename T> inline 
Vector<T>& operator*(const Vector<T> &A, int i) { return A * ((T)i); }
template <typename T> inline 
Vector<T>& operator*(int i, const Vector<T> &A) { return ((T)i) * A; }
*/


///////////////////////////////////////////////////////////
//
// Globals: Matlab "find" operations
//
///////////////////////////////////////////////////////////


template <typename T> inline 
IVec& find(const Vector<T> &V, char op, T val)
{
  std::string sz;
  if (sizeof(T) == sizeof(double)) {
    sz=umOFORM("find(%s %c %g)", V.name(), op, val);
  } else {
    sz=umOFORM("find(%s %c %d)", V.name(), op, val);
  }


  int len = V.size();
  IVec mask(len);

  int count=0;

  switch (op) {

  case '<':   // find ids of elements less than val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (V(i) < val) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;

  case '=':   // find ids of elements that equal val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (V(i) == val) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;

  case '>':   // find ids of elements greater than val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (V(i) > val) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;

  case '!':   // find ids of elements != val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (V(i) != val) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;


  }

  int sk=0;
  IVec *tmp = new IVec(count, sz.c_str(), OBJ_temp);
  for (int i=1; i<=len; ++i) {
    if (mask(i) >= 1) {
      (*tmp)(++sk) = mask(i);  // element mask(i) satisfied condition
    }
  }

#if (0) // defined(_DEBUG) || defined(DEBUG)
  umMSG(1, "find(Vec) : found %d matching elements\n", count);
  if ((count>0) && (count<=20))
    umMSG(1, "%s\n", tmp->display(" ", 0,3,false,20).c_str());
#endif

  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return (*tmp);
}



template <typename T> inline 
IVec& find(const Vector<T> &A, char op, const Vector<T> &B)
{
  std::string sz;
  sz=umOFORM("find(%s %c %s)", A.name(), op, B.name());

  int len = A.size(), blen=B.size();
  
  // for '&' operation, lengths need not match
  if (op != '&') {
    assert(blen == len); 
  }

  IVec mask(len);

  int count=0;

  switch (op) {

  case '<':   // find ids of elements less than val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (A(i) < B(i)) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;

  case '=':   // find ids of elements that equal val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (A(i) == B(i)) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;

  case '>':   // find ids of elements greater than val
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      if (A(i) > B(i)) {
        mask(i) = i;  // ith element satisfies condition
        count++;
      }
    }
    break;

  case '&':   // return elements that are in both sets
    //-----------------------------------------------------
    for (int i=1; i<=len; ++i) {
      for (int j=1; j<=blen; ++j) {
        if (A(i) == B(j)) {
          mask(i) = i;  // ith element satisfies condition
          count++;
          break;        // add value A(i), move to next i
        }
      }
    }
    break;

  }

  int sk=0;
  IVec *tmp = new IVec(count, sz.c_str(), OBJ_temp);

  if (op != '&') {
    for (int i=1; i<=len; ++i) {
      if (mask(i) >= 1) {
        (*tmp)(++sk) = mask(i);  // element mask(i) satisfied condition
      }
    }
  } else {
    for (int i=1; i<=len; ++i) {
      if (mask(i) >= 1) 
      {
        // in this case we return the actual values that 
        // were in both sets.  The indices into A where 
        // these were found are are stored in mask, so
        // return that actual value, not just the index.

        // element A(mask(i)) was in both "sets"
        (*tmp)(++sk) = (int) A(mask(i));
      }
    }
  }



#if (0) // defined(_DEBUG) || defined(DEBUG)
  umMSG(1, "find(Vec%cVec) : found %d matching elements\n", op,count);
  if ((count>0) && (count<=20))
    umMSG(1, "%s\n", tmp->display(" ", 0,3,false,20).c_str());
#endif

  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }

  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// Globals: Matlab "setdiff" operation
//
///////////////////////////////////////////////////////////

#include <set>

template <typename T> inline 
Vector<T>& setdiff(const Vector<T> &A, const Vector<T> &B)
{
  // return values in A that are not in B, sorting result

  Vector<T> *tmp = new Vector<T>("setdiff(A,B)", OBJ_temp);

  int Na=A.size(), Nb=B.size(), i=0;
  if (Nb<1) {
    (*tmp) = sort(A, false);  // NOT deleting duplicates
  }
  else
  {
    typedef typename std::set<T>::iterator SetIt;

    // Note: std::set sorts/removes duplicates from A
    std::set<T> us;
    for (i=1; i<=Na; ++i) {us.insert(A(i));}
    for (i=1; i<=Nb; ++i) {us.erase (B(i));}

    int len = (int) us.size(), sk=0;
    tmp->resize(len);  SetIt it;
    for (it=us.begin(), sk=1; it!=us.end(); it++, sk++) {
      (*tmp)(sk) = (*it);
    }
  }

  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }

  return (*tmp);
}


template <typename T> inline 
Vector<T>& setdiff(const Vector<T> &A, const T& x)
{
  // return values in A that are not in B, sorting result

  Vector<T> *tmp = new Vector<T>("setdiff(A,x)", OBJ_temp);

  int Na=A.size(), i=0;

  typedef typename std::set<T>::iterator SetIt;

  // Note: std::set sorts/removes duplicates from A
  std::set<T> us;
  for (i=1; i<=Na; ++i) { us.insert(A(i)); }

  // if x is in the set, remove it.
  us.erase(x);

  int len = (int) us.size(), sk=0;
  tmp->resize(len);  SetIt it;
  for (it=us.begin(), sk=1; it!=us.end(); it++, sk++) {
    (*tmp)(sk) = (*it);
  }

  if (A.get_mode() == OBJ_temp) { delete (&A); }

  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// Globals: Matlab "unique" operation
//
///////////////////////////////////////////////////////////

template <typename T> inline 
Vector<T>& unique(const Vector<T> &A)
{
  // return sorted values of A, removing duplicates
  Vector<T> *tmp = new Vector<T>("unique(A)", OBJ_temp);

  int Na=A.size(), i=0;
  if (Na<=1) {
    // degenerate cases Na = {0,1}
    tmp->resize(Na); for(i=1;i<=Na;++i) {(*tmp)(i)=A(i);}
  }
  else
  {
    typedef typename std::set<T>::iterator SetIt;

    // Note: std::set sorts/removes duplicates from A
    std::set<T> us;
    for (i=1; i<=Na; ++i) {us.insert(A(i));}
    int len = (int) us.size(), sk=0;
    tmp->resize(len);  SetIt it;
    for (it=us.begin(), sk=1; it!=us.end(); it++, sk++) {
      (*tmp)(sk) = (*it);
    }
  }
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  return (*tmp);
}


///////////////////////////////////////////////////////////
//
// Globals: Matlab "concat" operations
//
///////////////////////////////////////////////////////////


// concatenate 2 vectors
template <typename T> inline 
Vector<T>& concat(const Vector<T> &A, const Vector<T> &B)
{
  std::string sz("[A,B]");
  int len1 = A.size(), len2 = B.size();
  int total = len1+len2;
  int i=0, sk=0;

  Vector<T> *tmp = new Vector<T>(total, sz.c_str(), OBJ_temp);
  for (i=1;            i<=len1; ++i      ) {(*tmp)(i )=A(i);}
  for (i=1, sk=len1+1; i<=len2; ++i, ++sk) {(*tmp)(sk)=B(i);}

  // delete temporaries
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }

  return (*tmp);
}


// concatenate 3 vectors
template <typename T> inline 
Vector<T>& concat(const Vector<T> &A, const Vector<T> &B, const Vector<T> &C)
{
  std::string sz("[A,B,C]");
  int len1=A.size(), len2=B.size(), len3=C.size();
  int total=len1+len2+len3, i=0,sk=0;

  Vector<T> *tmp = new Vector<T>(total, sz.c_str(), OBJ_temp);
  for (i=1;                 i<=len1; ++i      ) {(*tmp)(i )=A(i);}
  for (i=1, sk=len1+1;      i<=len2; ++i, ++sk) {(*tmp)(sk)=B(i);}
  for (i=1, sk=len1+len2+1; i<=len3; ++i, ++sk) {(*tmp)(sk)=C(i);}

  // delete temporaries
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  if (C.get_mode() == OBJ_temp) { delete (&C); }

  return (*tmp);
}


// concatenate 4 vectors
template <typename T> inline 
Vector<T>& concat
(
  Vector<T> &A, Vector<T> &B, 
  Vector<T> &C, Vector<T> &D
)
{
  std::string sz("[A,B,C,D]");
  int len1=A.size(),len2=B.size(),len3=C.size(),len4=D.size();
  int total = len1+len2+len3+len4, i=0, sk=0;

  Vector<T> *tmp = new Vector<T>(total, sz.c_str(), OBJ_temp);
  for (i=1;                      i<=len1; ++i      ) {(*tmp)(i )=A(i);}
  for (i=1, sk=len1+1;           i<=len2; ++i, ++sk) {(*tmp)(sk)=B(i);}
  for (i=1, sk=len1+len2+1;      i<=len3; ++i, ++sk) {(*tmp)(sk)=C(i);}
  for (i=1, sk=len1+len2+len3+1; i<=len4; ++i, ++sk) {(*tmp)(sk)=D(i);}

  // delete temporaries
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  if (C.get_mode() == OBJ_temp) { delete (&C); }
  if (D.get_mode() == OBJ_temp) { delete (&D); }

  return (*tmp);
}


// concatenate [vector; scalar]
template <typename T> inline 
Vector<T>& concat(const Vector<T> &A, T x)
{
  std::string sz("[A,x]");
  int len1 = A.size(); int total=len1+1;
  Vector<T> *tmp = new Vector<T>(total, sz.c_str(), OBJ_temp);
  tmp->copy(len1, A.data()); (*tmp)(total)=x;

  // delete temporaries
  if (A.get_mode() == OBJ_temp) { delete (&A); }

  return (*tmp);
}


// concatenate [scalar; vector]
template <typename T> inline 
Vector<T>& concat(T x, const Vector<T> &A)
{
  std::string sz("[x,A]");
  // create as OBJ_real to avoid premature deletion in append()
  Vector<T> *tmp = new Vector<T>(1, sz.c_str(), OBJ_real);
  tmp->append(A);           // calls global concat()
  tmp->set_mode(OBJ_temp);  // restore OBJ_temp mode

  // if A is OBJ_temp, then A is deleted in append(A) 

  return (*tmp);
}


///////////////////////////////////////////////////////////
//
// Matlab "intersect" operation
//
///////////////////////////////////////////////////////////

// implemented for int arrays in file Global_funcs.cpp
IVec& intersect(const IVec& A, const IVec& B);



///////////////////////////////////////////////////////////
//
// Matlab "floor", "mod" operations
//
///////////////////////////////////////////////////////////


template <typename T> inline 
Vector<T>& floor(const Vector<T> &V)
{
  std::string sz=umOFORM("floor(%s)", V.name());

  int len = V.size();
  Vector<T> *tmp = new Vector<T>(len, sz.c_str(), OBJ_temp);
  for (int i=1; i<=len; ++i) {
    (*tmp)(i) = (T) (floor(double(V(i))));
  }

  if (V.get_mode() == OBJ_temp) {delete (&V);}
  return (*tmp);
}


inline   // specialization for <T=int>
IVec& mod(const IVec& V, int d)
{
  std::string sz=umOFORM("mod(%s)", V.name());
  int len = V.size();
  IVec *tmp = new IVec(len, sz.c_str(), OBJ_temp);
  for (int i=1; i<=len; ++i) { 
    (*tmp)(i) = V(i) % d; 
  }
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return (*tmp);
}


// mod: general <T> version
template <typename T> inline 
Vector<T>& mod(const Vector<T> &V, T d)
{
  std::string sz=umOFORM("mod(%s)", V.name());
  int len = V.size();
  Vector<T> *tmp = new Vector<T>(len, sz.c_str(), OBJ_temp);
  for (int i=1; i<=len; ++i) { (*tmp)(i) = (T)fmod(double(V(i)), double(d)); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return (*tmp);
}


///////////////////////////////////////////////////////////
//
// Boolean "&&", "||"
//
///////////////////////////////////////////////////////////


// C = A&B
template <typename T> inline 
Vector<T>& operator &&(const Vector<T> &A, const Vector<T> &B)
{
  int len=A.size();
  assert(B.size()==len);  // assume matching dimension
  Vector<T> *tmp=new Vector<T>(len, T(0), OBJ_temp, "(a&b)");
  const T *a=A.data(); const T *b=B.data(); T *p=tmp->data();
  for (int i=0; i<len; ++i) {
    if ((a[i] != T(0)) && (b[i] != T(0))) { p[i] = T(1); }
  }
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


// C = A|B
template <typename T> inline 
Vector<T>& operator ||(const Vector<T> &A, const Vector<T> &B)
{
  int len=A.size();
  assert(B.size()==len);  // assume matching dimension
  Vector<T> *tmp=new Vector<T>(len, T(0), OBJ_temp, "(a|b)");
  const T *a=A.data(); const T *b=B.data(); T *p=tmp->data();
  for (int i=0; i<len; ++i) {
    if ((a[i] != T(0)) || (b[i] != T(0))) { p[i] = T(1); }
  }
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


///////////////////////////////////////////////////////////
//
// Matlab "cumsum" operation
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Vector<T>& cumsum(const Vector<T>& A)
//---------------------------------------------------------
{
  int N=A.size();
  Vector<T> *tmp=new Vector<T>(N, "cumsum(A)", OBJ_temp);

  T tmp_sum = T(0);
  for (int i=1; i<=N; ++i) {
    tmp_sum += A(i);
    (*tmp)(i) = tmp_sum;
  }
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  return (*tmp);
}


///////////////////////////////////////////////////////////
//
// Matlab "sub2ind" operation
//
///////////////////////////////////////////////////////////


inline 
IVec& sub2ind(int Nr, int Nc, const IVec &ri, const IVec &cj)
{
  std::string sz=umOFORM("sub2ind(%d,%d)", Nr, Nc);

  int len = ri.size(); assert(cj.size() == len);
  IVec *tmp = new IVec(len, sz.c_str(), OBJ_temp);
  for (int i=1; i<=len; ++i) {
    assert(cj(i)>=1 && cj(i)<=Nc);
    assert(ri(i)>=1 && ri(i)<=Nr);
    (*tmp)(i) = (cj(i)-1)*Nr + ri(i);
  }

  // delete temp objects
  if (ri.get_mode() == OBJ_temp) { delete (&ri); }
  if (cj.get_mode() == OBJ_temp) { delete (&cj); }

  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// member functions involving a subset of the array
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::linspace(T start, T stop, int len)
//---------------------------------------------------------
{
  // Partition segment into equal intervals. If len is 0, 
  // the keep the current size, else resize the array
  assert(len>=0);
  
  if (len != 0)    {resize(len);}   // user resizing
  else { if(!ok()) {resize(2); }}   // catch empty vectors

  if (stop != start) {
    T delta=(stop-start) / (T)(m_Len-1);
    vm1_[1 ] = start;               // set exact start value
    for (int i=2; i<m_Len; ++i) {
      vm1_[i] = vm1_[i-1] + delta;  // delta may be negative
    }
    vm1_[m_Len] = stop;             // impose exact stop value
  } else {
    fill(start);                    // start and stop are the same
  }
}


//---------------------------------------------------------
template <typename T> inline
void Vector<T>::range(int start, int stop)
//---------------------------------------------------------
{
  // Set contents to be integer range from [start:stop].  
  // If (stop<start), range decreases.

  int len = 0;
  if (start<=stop) {
    len = (stop-start)+1;   // increasing range
  } else {
    len = (start-stop)+1;   // decreasing range
  }

  resize(len); 

  if (stop != start) {
    int delta=(start<stop)?1:-1;
    vm1_[1] = T(start);             // set start value
    for (int i=2; i<m_Len; ++i) {
      vm1_[i]=vm1_[i-1]+T(delta);   // delta may be negative
    }
    vm1_[m_Len] = T(stop);          // set stop value
  } else {
    fill(start);     // start and stop are the same
  }
}


template <typename T> inline
Vector<T>& Vector<T>::get_map(const IVec& iM) const
{
  // Return subset of elements, indexed by map
  // Enables the following syntax:
  //
  //  v = q.map(um->vmapR) - q.map(um->vmapL);

  static char buf[100]={""};
  snprintf(buf, (size_t)90, "vmap(%s)", this->name());
  int idx=0, len=iM.size();
  Vector<T> *tmp=new Vector<T>(len, ZERO, OBJ_temp, buf);
  if (len > 0) {
    // Load elements indexed by iM into New vector
    for (int k=1; k<=len; ++k) {
      idx = iM(k);
      Check_index_1(idx);       // make sure index is valid
      tmp->vm1_[k] = vm1_[idx]; // copy mapped element to New vec
    }
  }
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::set_map(const IVec& iM, const T &x)
{
  // Set elements indicated by map to given value

  int len = iM.size();
  if (len < 1) { return (*this); } // zero-length map
  int idx = 0;
  for (int k=1; k<=len; ++k) {
    idx = iM(k);
    if (idx>0 && idx <=m_Len) {
      vm1_[idx] = x;  // set mapped element to x
    } else {          // skip elements out of range
      umTRC(3, "set_map: index %d (%d) out of range [1:%d]\n", k, idx, m_Len);
    }
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::set_map(const IVec& iM, const Vector<T>& X)
{
  // Set elements indicated by map to corresponding elements in X
  int len = iM.size(), xlen = X.size();
  if (len < 1) { return (*this); }  // zero-length map
  if (len > m_Len) { umERROR("Vector<T>::set_map()", "too many entries in map"); }  
  int idx = 0;
  this->fill(ZERO);                 // zero all entries
  for (int k=1; k<=len; ++k) {
    idx = iM(k);                    // index into array of X values
    if (idx>=1 && idx <= xlen) {    // check index is in range
      vm1_[k] = X.vm1_[idx];        // set mapped element
    } else {                        // skip elements out of range
      umTRC(1, "set_map: index %d (%d) out of range [1:%d]\n", k, idx, m_Len);
    }
  }

  // if X is temporary, delete it.
  if (X.get_mode() == OBJ_temp) {delete (&X);}
  if (iM.get_mode()== OBJ_temp) {delete (&iM);}
  return (*this);
}



///////////////////////////////////////////////////////////
//
// member functions involving (contiguous) Region1D
//
///////////////////////////////////////////////////////////


// construct from Region1D
template <typename T> inline
Vector<T>::Vector(const Region1D< Vector<T> > &R, OBJ_mode md, const char *sz)
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
  (*this)=R;  // vector = region
}


// construct from const_Region1D
template <typename T> inline
Vector<T>::Vector(const_Region1D< Vector<T> > &R, OBJ_mode md, const char *sz)
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
  (*this)=R;  // vector = region
}



template <typename T> inline
const_Region1D< Vector<T> >
Vector<T>::operator()(const Index1D &I) const
{
  // return a const region of (*this)
  return const_Region1D< Vector<T> >(*this, I);
}


template <typename T> inline
Region1D< Vector<T> >
Vector<T>::operator()(const Index1D &I)
{
  // return a region of (*this)
  return Region1D< Vector<T> >(*this, I);
}


template <typename T> inline
Vector<T>& Vector<T>::operator=(const Region1D< Vector<T> > &R)
{
  // load from Region1D< Vec<> >
  int N = R.size(); resize(N);
  for (int i=1; i<=N; ++i) {this->vm1_[i]=R(i);} // loads R(offset+i)
  return (*this);
}

template <typename T> inline
Vector<T>& Vector<T>::operator=(const const_Region1D< Vector<T> > &R)
{
  // load from Region1D< Vec<> >
  int N = R.size(); resize(N);
  for (int i=1; i<=N; ++i) {this->vm1_[i]=R(i);} // loads R(offset+i)
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::append(const Region1D< Vector<T> > &R)
{
  Vector<T> br(R);  // load vector from Region1D
  append(br);       // append
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::mult_element(const Region1D< Vector<T> > &R)
{
  assert(R.size() >= m_Len);
  for (int i=1; i<=m_Len; ++i) {
    vm1_[i] *= R(i);
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::div_element(const Region1D< Vector<T> > &R)
{
  assert(R.size() >= m_Len);
  for (int i=1; i<=m_Len; ++i) {
    assert(R(i) != ZERO);
    vm1_[i] /= R(i);
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::dd(const Region1D< Vector<T> > &R) const
{
  // (*this) is not changed: C = A ./ reg(R)
  std::string sz; tmp_op_name(name(), "./", R.name(), sz);
  Vector<T> *tmp=new Vector<T>((*this), OBJ_temp, sz.c_str());
  Vector<T> bv(R);
  (*tmp) /= bv;
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::dm(const Region1D< Vector<T> > &R) const
{
  // (*this) is not changed: C = A .* reg(R)
  std::string sz; tmp_op_name(name(), ".*", R.name(), sz);
  Vector<T> *tmp=new Vector<T>((*this), OBJ_temp, sz.c_str());
  Vector<T> bv(R);
  (*tmp) *= bv;
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::operator+=(const Region1D< Vector<T> > &R)
{
  int N1 = this->size(), N2 = R.size();

  // Allow addition of blocks of different sizes.
  // if arg is too long, extend this to match

  if (N2 > N1) { this->extend(N2); }
  for (int i=1; i<=N2; ++i) {
    this->vm1_[i] += R(i);      // adds R(offset+i)
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::operator-=(const Region1D< Vector<T> > &R)
{
  int N1 = this->size(), N2 = R.size();

  // Allow subtraction of blocks of different sizes.
  // if arg is too long, extend this to match

  if (N2 > N1) { this->extend(N2); }
  for (int i=1; i<=N2; ++i) {
    this->vm1_[i] -= R(i);      // subtracts R(offset+i)
  }
  return (*this);
}



///////////////////////////////////////////////////////////
//
// global functions involving (contiguous) Region1D
//
///////////////////////////////////////////////////////////
//
// vector + region
// vector - region
// vector * region
//
// region + region
// region - region
//
// region * scalar
// scalar * region
// region / scalar
// scalar / region
//
// inner (vector,region)
// inner (region,vector)
///////////////////////////////////////////////////////////

// vector + region
template <typename T> inline 
Vector<T>& operator+(const Vector<T> &A, const Region1D< Vector<T> > &B)
{
  std::string sz("A+reg(B)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}


// vector - region
template <typename T> inline 
Vector<T>& operator-(const Vector<T> &A, const Region1D< Vector<T> > &B)
{
  std::string sz("A-reg(B)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}


// A = vector * region
template <typename T> inline 
Vector<T>& operator*(const Vector<T> &A, const Region1D< Vector<T> > &B)
{
  std::string sz("A*reg(B)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= B;
  return (*tmp);
}


// region + region
template <typename T> inline 
Vector<T>& operator+(const Region1D< Vector<T> > &A, const Region1D< Vector<T> > &B)
{
  std::string sz("reg(A) + reg(B)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}


// region - region
template <typename T> inline 
Vector<T>& operator-(const Region1D< Vector<T> > &A, const Region1D< Vector<T> > &B)
{
  std::string sz("reg(A) - reg(B)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}


// region * scalar
template <typename T> inline 
Vector<T>& operator*(const Region1D< Vector<T> > &A, T x)
{
  std::string sz("reg(A) * x");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// scalar * region
template <typename T> inline 
Vector<T>& operator*(T x, const Region1D< Vector<T> > &A)
{
  std::string sz("x * reg(A)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// region / scalar
template <typename T> inline 
Vector<T>& operator/(const Region1D< Vector<T> > &A, T x)
{
  std::string sz("reg(A) / x");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= x;
  return (*tmp);
}


// scalar / region
template <typename T> inline 
Vector<T>& operator/(T x, const Region1D< Vector<T> > &A)
{
  // create a vector filled with scalar x;  tmp = tmp./A

  std::string sz("x / reg(A)");

  Vector<T> *tmp =new Vector<T>(A.size(), x, OBJ_temp, sz.c_str());
  Vector<T> B(A, OBJ_temp);
  tmp /= B;
  return (*tmp);
}


// inner (vector,region)
template <typename T> inline
T inner(const Vector<T>& V, const Region1D< Vector<T> > &R)
{
  T retval=T(0); int len=V.size(); assert(R.size()<=len);
  for (int i=1; i<=len; ++i) { retval += V(i)*R(i); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return retval;
}

// inner (region,vector)
template <typename T> inline
T inner(const Region1D< Vector<T> > &R, const Vector<T>& V)
{
  T retval=T(0); int len=R.size(); assert(V.size()<=len);
  for (int i=1; i<=len; ++i) { retval += R(i)*V(i); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return retval;
}



///////////////////////////////////////////////////////////
//
// member functions involving MappedRegion1D
//
///////////////////////////////////////////////////////////


// construct from MappedRegion1D
//---------------------------------------------------------
template <typename T> inline
Vector<T>::Vector(const MappedRegion1D< Vector<T> > &R, OBJ_mode md, const char *sz)
//---------------------------------------------------------
: v_(0), vm1_(0), m_Len(0), ZERO(0), ONE(1),
  m_name(sz), m_EqTol(0.0), m_borrowed(false),
  m_id(-1), m_mode(md), m_pReg(NULL)
{
  ++s_count;
  (*this)=R;  // vector = mapped region
}


template <typename T> inline
MappedRegion1D< Vector<T> > 
Vector<T>::operator()(const IVec &I)
{
  // return a mapped region of (*this)    IMAP
  IMap im(I.size(), I.data());
  if (I.get_mode()==OBJ_temp) {delete (&I);}
  return MappedRegion1D< Vector<T> >(*this, im);
}


template <typename T> inline
const_MappedRegion1D< Vector<T> > 
Vector<T>::operator()(const IVec &I) const
{
  // return a const mapped region of (*this)    IMAP
  IMap im(I.size(), I.data());
  if (I.get_mode()==OBJ_temp) {delete (&I);}
  return const_MappedRegion1D< Vector<T> >(*this, im);
}


template <typename T> inline
MappedRegion1D< Vector<T> > 
Vector<T>::operator()(const Region1D< Vector<int> > &Ri)
{
  //int lo=Ri.lbound(), hi=Ri.size(), ofs=Ri.offset();
  //Index1D I(lo+ofs,hi+ofs);
  // return a mapped region of (*this)    IMAP
  Vector<int> I(Ri);
  IMap im(I.size(), I.data());
  return MappedRegion1D< Vector<T> >(*this, im);
}


template <typename T> inline
const_MappedRegion1D< Vector<T> > 
Vector<T>::operator()(const const_Region1D< Vector<int> > &Ri) const
{
  //int lo=Ri.lbound(), hi=Ri.size();
  //Index1D I(lo,hi);
  // return a const mapped region of (*this)    IMAP
  Vector<int> I(Ri);
  IMap im(I.size(), I.data());
  return const_MappedRegion1D< Vector<T> >(*this, im);
}


template <typename T> inline
Vector<T>& Vector<T>::operator=(const MappedRegion1D< Vector<T> > &R)
{
  // load from a MappedRegion1D< Vec<> >
  int N = R.size(); resize(N);
  for (int i=1; i<=N; ++i) {this->vm1_[i] = R(i);} // loads R(map(i))
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::operator=(const const_MappedRegion1D< Vector<T> > &R)
{
  // load from a const_MappedRegion1D< Vec<> >
  int N = R.size(); resize(N);
  for (int i=1; i<=N; ++i) {this->vm1_[i] = R(i);} // loads R(map(i))
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::mult_element(const MappedRegion1D< Vector<T> >& R)
{
  assert(R.size() >= m_Len);
  for (int i=1; i<=m_Len; ++i) {
    vm1_[i] *= R(i);
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::div_element(const MappedRegion1D< Vector<T> >& R)
{
  assert(R.size() >= m_Len);
  for (int i=1; i<=m_Len; ++i) {
    assert(R(i) != ZERO);
    vm1_[i] /= R(i);
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::dd(const MappedRegion1D< Vector<T> > &R) const
{
  // (*this) is not changed: C = A ./ R(map)
  std::string sz; tmp_op_name(name(), "./", R.name(), sz);
  Vector<T> *tmp=new Vector<T>((*this), OBJ_temp, sz.c_str());
  Vector<T> bv(R);
  (*tmp) /= bv;
  return (*tmp);
}
  

template <typename T> inline
Vector<T>& Vector<T>::dm(const MappedRegion1D< Vector<T> > &R) const
{
  // (*this) is not changed: C = A .* R(map)
  std::string sz; tmp_op_name(name(), ".*", R.name(), sz);
  Vector<T> *tmp=new Vector<T>((*this), OBJ_temp, sz.c_str());
  Vector<T> bv(R);
  (*tmp) *= bv;
  return (*tmp);
}


template <typename T> inline
Vector<T>& Vector<T>::operator+=(const MappedRegion1D< Vector<T> > &R)
{
  int N1 = this->size(), N2 = R.size();

  // Allow addition of blocks of different sizes.
  // if arg is too long, extend this to match

  if (N2 > N1) { this->extend(N2); }
  for (int i=1; i<=N2; ++i) {
    this->vm1_[i] += R(i);      // adds R(map(i))
  }
  return (*this);
}


template <typename T> inline
Vector<T>& Vector<T>::operator-=(const MappedRegion1D< Vector<T> > &R)
{
  int N1 = this->size(), N2 = R.size();

  // Allow subtraction of blocks of different sizes.
  // if arg is too long, extend this to match

  if (N2 > N1) { this->extend(N2); }
  for (int i=1; i<=N2; ++i) {
    this->vm1_[i] -= R(i);      // subtracts R(map(i))
  }
  return (*this);
}


///////////////////////////////////////////////////////////
//
// global functions involving MappedRegion1D
//
///////////////////////////////////////////////////////////
//
//        vector + mapped region
//        vector - mapped region
//
// mapped region + mapped region
// mapped region - mapped region
//
// mapped region * scalar
//        scalar * mapped region
// mapped region / scalar
//        scalar / mapped region
// mapped region + scalar
//        scalar + mapped region
// mapped region - scalar
//        scalar - mapped region
//
///////////////////////////////////////////////////////////


// vector + mapped region
template <typename T> inline 
Vector<T>& operator+(const Vector<T> &A, const MappedRegion1D< Vector<T> > &B)
{
  std::string sz("A+B(map)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}


// vector - mapped region
template <typename T> inline 
Vector<T>& operator-(const Vector<T> &A, const MappedRegion1D< Vector<T> > &B)
{
  std::string sz("A-B(map)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}


// mapped region + mapped region
template <typename T> inline 
Vector<T>& operator+(const MappedRegion1D< Vector<T> > &A, const MappedRegion1D< Vector<T> > &B)
{
  std::string sz("A(map) + B(map)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}


// mapped region - mapped region
template <typename T> inline 
Vector<T>& operator-(const MappedRegion1D< Vector<T> > &A, const MappedRegion1D< Vector<T> > &B)
{
  std::string sz("A(map) - B(map)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}


// mapped region * scalar
template <typename T> inline 
Vector<T>& operator*(const MappedRegion1D< Vector<T> > &A, T x)
{
  std::string sz("A(map)*x");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// scalar * mapped region
template <typename T> inline 
Vector<T>& operator*(T x, const MappedRegion1D< Vector<T> > &A)
{
  std::string sz("x*A(map)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// mapped region / scalar
template <typename T> inline 
Vector<T>& operator/(const MappedRegion1D< Vector<T> > &A, T x)
{
  std::string sz("A(map)/x");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= x;
  return (*tmp);
}


// scalar / mapped region
template <typename T> inline 
Vector<T>& operator/(T x, const MappedRegion1D< Vector<T> > &A)
{
  // create a vector filled with scalar x;  tmp = tmp./A

  std::string sz("x/A(map)");

  Vector<T> *tmp =new Vector<T>(A.size(), x, OBJ_temp, sz.c_str());
  Vector<T> B(A, OBJ_temp);
  tmp /= B;
  return (*tmp);
}


// mapped region + scalar
template <typename T> inline 
Vector<T>& operator+(const MappedRegion1D< Vector<T> > &A, T x)
{
  std::string sz("A(map)+x");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


// scalar + mapped region
template <typename T> inline 
Vector<T>& operator+(T x, const MappedRegion1D< Vector<T> > &A)
{
  std::string sz("x+A(map)");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


// mapped region - scalar
template <typename T> inline 
Vector<T>& operator-(const MappedRegion1D< Vector<T> > &A, T x)
{
  std::string sz("A(map)-x");

  Vector<T> *tmp=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= x;
  return (*tmp);
}


// scalar - mapped region
template <typename T> inline 
Vector<T>& operator-(T x, const MappedRegion1D< Vector<T> > &A)
{
  std::string sz("x-A(map)");

  Vector<T> *tmp=new Vector<T>(A.size(), x, OBJ_temp, sz.c_str());
  (*tmp) -= A;
  return (*tmp);
}

#endif  // NDG__Vector_Type_H__INCLUDED

