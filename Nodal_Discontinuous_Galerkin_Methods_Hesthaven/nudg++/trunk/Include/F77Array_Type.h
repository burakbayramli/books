// F77Array_Type.h
//
// 2007/02/15
//---------------------------------------------------------
#ifndef NDG__F77Array_Type_H__INCLUDED
#define NDG__F77Array_Type_H__INCLUDED

#include "Vec_Type.h"


// forward declarations:
template <typename T> class F77Vec;

typedef F77Vec<double>  F77DVec;
typedef F77Vec<long>    F77LVec;
typedef F77Vec<int>     F77IVec;


//---------------------------------------------------------
template <typename T>
class F77Vec : public virtual Vector<T>
//---------------------------------------------------------
{
  // Specialization of Vector<T> allowing arbitrary
  // base index, c.f. F77 dimension (-3:N+3)

  //
  // Member data
  //
protected:

  int base;     // arbitrary base index for array
  T*  vmb_;     // b-offset pointer into array

public:

  //
  // constructors
  //
  F77Vec(                const char* sz="F77Vec",  OBJ_mode md=OBJ_real);
  F77Vec(int N,           const char* sz="F77Vec",  OBJ_mode md=OBJ_real);
  F77Vec(int lo, int hi,   const char* sz="F77Vec",  OBJ_mode md=OBJ_real);
  F77Vec(const Index1D& II, const char* sz="F77Vec",  OBJ_mode md=OBJ_real);
  F77Vec(const F77Vec<T>& B, const char* sz="F77Vec",  OBJ_mode md=OBJ_real);

  // destructor
  virtual ~F77Vec ();

  // resize/realloc
  bool resize(int N,              bool binit=true, T x=T(0));
  bool resize(int lo, int hi,     bool binit=true, T x=T(0));
  bool resize(const Index1D& II,  bool binit=true, T x=T(0));

  // set/get arbitrary base for indexing into the array
  void setBase(int b1)  { base = b1; }
  int  getBase() const  { return base; }

  // element access
  const T& operator()(int i) const  { Check_index_b(i);  return this->vmb_[i]; }
        T& operator()(int i)        { Check_index_b(i);  return this->vmb_[i]; }

  // assignment
  F77Vec<T>& operator=(const F77Vec<T>& B)
  {
    Vector<T>::operator=(B)
    base = B.base;
    vmb_ = v_ - base;   // set b-offset pointer
    return *this;
  }

  // assignment
  F77Vec<T>& operator=(const Vector<T>& B)
  {
    Vector<T>::operator=(B)
    base = 1;
    vmb_ = v_ - base;   // set b-offset pointer
    return *this;
  }

protected:
  //-------------------------------------
  // Optional index checking
  //-------------------------------------
#if (CHECK_ARRAY_INDEX)

  void Check_index_b(int i) const {
    // check for legal b-based index
    if (i-base <  0 )     throw 1;
    if (i-base >= m_Len)  throw 2;
  }

#else

  // no checking of indices
  void Check_index_b(int i) const throw() {}

#endif

};



///////////////////////////////////////////////////////////
//
// constructors
//
///////////////////////////////////////////////////////////

template <typename T> inline
F77Vec<T>::F77Vec(const char* sz, OBJ_mode md)
: Vector<T>(sz, md), base(1), vmb_(NULL)
{}


template <typename T> inline
F77Vec<T>::F77Vec(int N, const char* sz,  OBJ_mode md)
: Vector<T>(N, sz, md), base(1), vmb_(NULL)
{}


template <typename T> inline
F77Vec<T>::F77Vec(int lo, int hi, const char* sz,  OBJ_mode md)
: Vector<T>(sz, md), base(1), vmb_(NULL)
{
  resize(lo,hi);
}


template <typename T> inline
F77Vec<T>::F77Vec(const Index1D& II, const char* sz,  OBJ_mode md)
: Vector<T>(sz, md), base(1), vmb_(NULL)
{
  resize(II);
}


template <typename T> inline
F77Vec<T>::F77Vec(const F77Vec<T>& B, const char* sz,  OBJ_mode md)
: Vector<T>(sz, md), base(1), vmb_(NULL)
{
  this->operator=(B);
}


///////////////////////////////////////////////////////////
//
// destructor
//
///////////////////////////////////////////////////////////

template <typename T> inline
F77Vec<T>::~F77Vec () 
{}




///////////////////////////////////////////////////////////
//
// resize/realloc
//
///////////////////////////////////////////////////////////

template <typename T> inline
bool F77Vec<T>::resize(int N, bool binit, T x)
{
  base = 1;  bool ret = false;
  ret  = Vector<T>::resize(N, binit, x);
  vmb_ = v_ - base;   // set b-offset pointer
  return ret;
}


template <typename T> inline
bool F77Vec<T>::resize(int lo, int hi, bool binit, T x)
{
  assert(lo<=hi);
  base = lo;  bool ret = false;
  ret  = Vector<T>::resize( (hi-lo)+1, binit, x);
  vmb_ = v_ - base;   // set b-offset pointer
  return ret;
}


template <typename T> inline
bool F77Vec<T>::resize(const Index1D& II, bool binit, T x)
{
  int lo=II.lo(), hi=II.hi();  assert(lo<=hi);
  base = lo;  bool ret = false;
  ret  = Vector<T>::resize( (hi-lo)+1, binit, x);
  vmb_ = v_ - base;   // set b-offset pointer
  return ret;
}




//#########################################################
#if 1 // (THIS_IS_READY)
//#########################################################

#include "Mat_Col.h"

// forward declarations:
template <typename T> class F77Mat;

typedef F77Mat<double>  F77DMat;
typedef F77Mat<long>    F77LMat;
typedef F77Mat<int>     F77IMat;


//---------------------------------------------------------
template <typename T>
class F77Mat : public virtual Mat_COL<T>
//---------------------------------------------------------
{
  // Specialization of Mat_COL<T> allowing arbitrary
  // base indices, c.f. F77 dimension (-3:N+3, 5:10)

  //
  // Member data
  //
protected:

  int bm;     // arbitrary base for row index
  int bn;     // arbitrary base for col index

  T** bcol;   // b-based data pointers, adjusted 
              // to enable b-based (i,j) indexing

public:

  //
  // constructors
  //
  explicit F77Mat(                                      const char* sz="F77Mat",  OBJ_mode md=OBJ_real);
  explicit F77Mat(           int Nr,            int Nc, const char* sz="F77Mat",  OBJ_mode md=OBJ_real);
  explicit F77Mat(const Index1D& II, const Index1D& JJ, const char* sz="F77Mat",  OBJ_mode md=OBJ_real);
  explicit F77Mat(const Index1D& II,            int Nc, const char* sz="F77Mat",  OBJ_mode md=OBJ_real);
  explicit F77Mat(           int Nr, const Index1D& JJ, const char* sz="F77Mat",  OBJ_mode md=OBJ_real);
  explicit F77Mat(const F77Mat<T>& B,                   const char* sz="F77Mat",  OBJ_mode md=OBJ_real);

  // destructor
  virtual ~F77Mat();
  virtual void destroy();
  void set_bpointers(int M, int N);

  // resize/realloc
  bool resize(           int Nr,            int Nc, bool binit=true, T x=T(0));
  bool resize(const Index1D& II, const Index1D& JJ, bool binit=true, T x=T(0));
  bool resize(const Index1D& II,            int Nc, bool binit=true, T x=T(0));
  bool resize(           int Nr, const Index1D& JJ, bool binit=true, T x=T(0));


  // set/get arbitrary row and column index base
  void setBaseM(int b)    { bm = b; }
  int  getBaseM() const   { return bm; }

  void setBaseN(int b)    { bn = b; }
  int  getBaseN() const   { return bn; }

  // element access
  const T& operator()(int i, int j) const
  {
    CheckIdx_IJ_b(i,j);
    return this->bcol[j][i];
  }

  T& operator()(int i, int j)
  { 
    CheckIdx_IJ_b(i,j);
    return this->bcol[j][i];
  }


  // assignment
  F77Mat<T>& operator=(const F77Mat<T>& B);
  F77Mat<T>& operator=(const Mat_COL<T>& B);


protected:
  //-------------------------------------
  // Optional index checking
  //-------------------------------------
#if (CHECK_ARRAY_INDEX)

  void CheckIdx_IJ_b(int i, int j) const { 
    // check for legal b-based indices
    if (i < bm) throw 1;  if (i >= (m_M+bm)) throw 2;
    if (j < bn) throw 3;  if (j >= (m_N+bn)) throw 4;
  }

#else

  // no checking of indices
  void CheckIdx_IJ_b(int i, int j) const throw() {}

#endif



};



///////////////////////////////////////////////////////////
//
// constructors
//
///////////////////////////////////////////////////////////
template <typename T> inline
F77Mat<T>::F77Mat(const char* sz,  OBJ_mode md)
: Mat_COL<T>(sz,md),
  bm(1), bn(1), bcol(NULL)
{
}


template <typename T> inline
F77Mat<T>::F77Mat(int Nr, int Nc, const char* sz,  OBJ_mode md)
: Mat_COL<T>(Nr, Nc, sz, md),
  bm(1), bn(1), bcol(NULL)
{
  // A(1:Nr, 1:Nc)
  bm = 1;
  bn = 1;
  set_bpointers(Nr, Nc);
}


template <typename T> inline
F77Mat<T>::F77Mat(const Index1D& II, const Index1D& JJ, const char* sz,  OBJ_mode md)
: Mat_COL<T>(II.N(), JJ.N(), sz, md),
  bm(1), bn(1), bcol(NULL)
{
  // A(ia:ib, ja:jb)
  bm = II.lo();
  bn = JJ.lo();
  set_bpointers(II.N(), JJ.N());
}


template <typename T> inline
F77Mat<T>::F77Mat(const Index1D& II, int Nc, const char* sz,  OBJ_mode md)
: Mat_COL<T>(II.N(), Nc, sz, md),
  bm(1), bn(1), bcol(NULL)
{
  // A(ia:ib, 1:Nc)
  bm = II.lo();
  bn = 1;
  set_bpointers(II.N(), Nc);
}


template <typename T> inline
F77Mat<T>::F77Mat(int Nr, const Index1D& JJ, const char* sz,  OBJ_mode md)
: Mat_COL<T>(Nr, JJ.N(), sz, md),
  bm(1), bn(1), bcol(NULL)
{
  // A(1:Nr, ja:jb)
  bm = 1;
  bn = JJ.lo();
  set_bpointers(Nr, JJ.N());
}


template <typename T> inline
F77Mat<T>::F77Mat(const F77Mat<T>& B, const char* sz,  OBJ_mode md)
: Mat_COL<T>(sz,md),
  bm(0), bn(0), bcol(NULL)
{
  this->operator=(B);
}


///////////////////////////////////////////////////////////
//
// destructors
//
///////////////////////////////////////////////////////////


template <typename T> inline
F77Mat<T>::~F77Mat()
{
  this->destroy();
}


template <typename T> inline
void F77Mat<T>::destroy()
{
  // Mat/Vec base classes manage deallocation of data,
  // This class manages only its b-based bcol pointers.
  Mat_COL<T>::destroy();

  // Note: restore "bcol" to 0-offset
//if (col_) {col_ ++;   ::free(col_); col_=NULL;}
  if (bcol) {bcol +=bn; ::free(bcol); bcol=NULL;}

  bm = bn = 0;
}


// The internal contiguous (0-offset) array v_[M*N] is 
// allocated in base class Vector.  Here we create an 
// internal array of column pointers to enable b-based, 
// column-major indexing into an (M,N) matrix.
//---------------------------------------------------------
template <typename T> inline
void F77Mat<T>::set_bpointers(int M, int N)
//---------------------------------------------------------
{
  // Assumes base class version has been called
  // to allow use as a Mat_COL<T> object

  // Mat_COL<T>::set_pointers(M,N);

  // clear the old set of b-col pointers
  // Note: restore "bcol" to 0-offset
  if (bcol) {bcol += bn; ::free(bcol); bcol=NULL;}

  // allocate a New set of col pointers
  bcol = (T **) calloc((size_t)N, sizeof(T*) );
  assert(bcol);

  // adjust pointers for b-based indexing

  T* p = this->v_ - bm;   // bm <= row offset

  for (int i=0; i<N; ++i)
  {
    bcol[i] = p;
    p += M;
  }

  bcol -= bn;   // adjust for b-based indexing
}



///////////////////////////////////////////////////////////
//
// resize/realloc
//
///////////////////////////////////////////////////////////


template <typename T> inline
bool F77Mat<T>::resize(int Nr, int Nc, bool binit, T x)
{
  bool rb=Mat_COL<T>::resize(Nr, Nc, binit, x);

  // A(1:Nr, 1:Nc)
  bm = 1;
  bn = 1;
  set_bpointers(Nr, Nc);
  return rb;
}


template <typename T> inline
bool F77Mat<T>::resize(const Index1D& II, const Index1D& JJ, bool binit, T x)
{
  bool rb=Mat_COL<T>::resize(II.N(), JJ.N(), binit, x);

  // A(ia:ib, ja:jb)
  bm = II.lo();
  bn = JJ.lo();
  set_bpointers(II.N(), JJ.N());
  return rb;
}


template <typename T> inline
bool F77Mat<T>::resize(const Index1D& II, int Nc, bool binit, T x)
{
  bool rb=Mat_COL<T>::resize(II.N(), Nc, binit, x);

  // A(ia:ib, 1:Nc)
  bm = II.lo();
  bn = 1;
  set_bpointers(II.N(), Nc);
  return rb;
}


template <typename T> inline
bool F77Mat<T>::resize(int Nr, const Index1D& JJ, bool binit, T x)
{
  bool rb=Mat_COL<T>::resize(Nr, JJ.N(), binit, x);

  // A(1:Nr, ja:jb)
  bm = 1;
  bn = JJ.lo();
  set_bpointers(Nr, JJ.N());
  return rb;
}


///////////////////////////////////////////////////////////
//
// assignment
//
///////////////////////////////////////////////////////////


template <typename T> inline
F77Mat<T>& F77Mat<T>::operator=(const F77Mat<T>& B)
{
  if (this->v_ == B.v_)
    return (*this);

  // set up base class structure
  Mat_COL<T>::operator=(B);

  // set up b-based indexing
  this->bm = B.getBaseM();
  this->bn = B.getBaseN();
  set_bpointers(m_M, m_N);

  return *this;
}


template <typename T> inline
F77Mat<T>& F77Mat<T>::operator=(const Mat_COL<T>& B)
{
  if (this->v_ == B.v_)
    return (*this);

  // set up base class structure
  Mat_COL<T>::operator=(B);

  // set up b-based indexing
  this->bm = 1;
  this->bn = 1;
  set_bpointers(m_M, m_N);

  return *this;
}


//#########################################################
#endif  // (THIS_IS_READY)
//#########################################################


#endif  // NDG__F77Array_Type_H__INCLUDED
