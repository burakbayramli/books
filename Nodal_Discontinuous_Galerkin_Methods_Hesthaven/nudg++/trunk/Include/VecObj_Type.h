// VecObj_Type.h: simple 1-D array of objects
// 1-based () and 0-based [] indexing
// 2007/10/18
//---------------------------------------------------------
#ifndef NDG__VecObj_Type_H__INCLUDED
#define NDG__VecObj_Type_H__INCLUDED

#include <cstdlib>
#include <cassert>
#include "ArrayMacros.h"


// typedef versions for common data types
template <typename T> class VecObj;

typedef VecObj<double>  DV;
typedef VecObj<int>     IV;

// for debug print
void dumpDV(const DV& V, const char* s);
void dumpIV(const IV& V, const char* s);


//---------------------------------------------------------
template <typename T>
class VecObj
//---------------------------------------------------------
{

protected:
  T*    v_;     // pointer to calloc'd alloc.
  T*    vm1_;   // pointer for 1-based access
  int   n_;     // length of allocation

  void initialize(int len);
  void copy(const T* p);

public:

  // constructors
  VecObj()                  : v_(0),vm1_(0),n_(0) {;}
  VecObj(const VecObj<T> &B): v_(0),vm1_(0),n_(0) {copy(B);}
  VecObj(int N)             : v_(0),vm1_(0),n_(0) {initialize(N);}
  VecObj(int N, T* p)       : v_(0),vm1_(0),n_(0) { 
    initialize(N);
    for (int i=0; i<N; ++i) { v_[i] = p[i]; }
  }

  // destructor
  ~VecObj() { destroy(); }
  void destroy();

  bool  ok() const      { return (v_ && n_>0); }
  int   size() const    { return n_; }
  int   length() const  { return n_; }
  bool  resize(int N);

  void  truncate(int N);
  bool  extend(int N);
  bool  realloc(int N);   // same as extend

  VecObj<T>& append(const T&  x);
  VecObj<T>& append(const VecObj<T>& B);

  // access to data
  const T* data() const   { return v_; }
        T* data()         { return v_; }

  // 1-based (i) access
  const T& operator()(int i) const {Check_index_1(i);return vm1_[i];}
        T& operator()(int i)       {Check_index_1(i);return vm1_[i];}
  
  // 0-based [i] access
  const T& operator[](int i) const {Check_index_0(i);return v_[i];}
        T& operator[](int i)       {Check_index_0(i);return v_[i];}

  // assignment
  VecObj<T>& operator=(const T& x) { fill(x); return (*this); }
  VecObj<T>& operator=(const VecObj<T> &B);

  void copy(const VecObj<T>& rA) { (*this) =   rA ; }
  void copy(const VecObj<T>* pA) { (*this) = (*pA); }

  // optimize certain assignments (assumes length is ok)
  void set(T& x1, T& x2)        { v_[0]=x1; v_[1]=x2; }
  void set(T& x1, T& x2, T& x3) { v_[0]=x1; v_[1]=x2; v_[2]=x3; }

  void fill(const T &x);

protected:

  //-------------------------------------
  // Optional index checking
  //-------------------------------------
#if (CHECK_ARRAY_INDEX)
  void Check_index_0(int i) const {
    // check for legal 0-based index
    if (i< 0 ) throw 1;
    if (i>=n_) throw 2;
  }
  void Check_index_1(int i) const {
    // check for legal 1-based index
    if (i<1 )  throw 1;
    if (i>n_)  throw 2;
  }
#else
  void Check_index_0(int i) const throw() {}
  void Check_index_1(int i) const throw() {}
#endif

};



//---------------------------------------------------------
// define member functions
//---------------------------------------------------------

template <typename T> inline
void VecObj<T>::initialize(int len)
{
  // create the array, adjusting 1-based pointers
  // about to allocate, so expect v_ to be NULL
  assert(NULL == v_);

  // Please note: use new (not calloc) to ensure 
  // that each object's constructor is called:

  v_   = new T[len];    // array of objects
  assert(v_ );
  vm1_ = v_-1;
  n_ = len;
}


template <typename T> inline
void VecObj<T>::destroy() 
{
  if (v_) { delete [] v_; }
  n_=0; v_=NULL; vm1_=NULL;
}


template <typename T> inline
bool VecObj<T>::resize(int N)
{
  // Resize existing object, optionally setting 
  // the entire array to some given inital value.
  // Return value indicates whether size has changed.

  assert( N >= 0 );       // must be non-negative
  bool bResized=false;
  if (N != n_) {
    bResized=true;
    this->destroy();      // clear allocation, zero members
    if (N > 0) {
      initialize(N);
    }
  }

  return bResized;
}


//---------------------------------------------------------
template <typename T> inline
void VecObj<T>::truncate(int N)
{
  extend(N);            // alias for extend
}


template <typename T> inline
bool VecObj<T>::realloc(int N)
{
  return extend(N);     // alias for extend
}


template <typename T> inline
bool VecObj<T>::extend(int N)
//---------------------------------------------------------
{
  // resize a VecObj, retaining existing values
  if (N<1) { destroy(); return true; }

  assert( N >= 0 );     // must be non-negative
  int Nold = n_;        // store current size

  if (!ok()) 
  {
    return resize(N);   // new allocation
  } 
  else if (n_ == N) 
  {
    return true;        // i.e. all's OK
  //return false;       // i.e. not resized
  } 
  else 
  {
    // Note: using new/delete, so not using realloc
    // For now, simply re-allocate and copy...

    T* v2 = new T[N];     // allocate new array of default objects
    if (!v2) { 
      destroy();          // attempt cleanup
      umERROR("VecObj::extend()", "realloc from %d to %d objects failed", Nold, N); 
      return false;
    }

    int tocopy = std::min(Nold, N);
    for (int i=0; i<tocopy; ++i) {
      v2[i] = v_[i];      // copy appropiate set of old objects
    }
    destroy();            // release old array
    this->v_ = v2;        // take ownership of new array
    vm1_ = v_ - 1;        // update 1-offset pointer
    n_ = N;
  }
  return true;
}


// append 1 <T> to an existing VecObj<T>
template <typename T> inline 
VecObj<T>& VecObj<T>::append(const T& x)
{
  if (!ok()) {
    resize(1); v_[0]=x;     // "appending" first element
  } else {
    int len1 = this->size();
    this->extend(len1+1);
    this->v_[len1] = x;
  }
  return (*this);
}


// append a VecObj to an existing VecObj
template <typename T> inline 
VecObj<T>& VecObj<T>::append(const VecObj<T> &B)
{
  if (!ok()) {
    (*this)=B;    // "appending" to empty array
  } else {
    int len1 = this->size(), len2 = B.size();
    int total = len1+len2;
    this->extend(total);
    // append elements of B to tail of this extended array
    for (int sk=len1, i=0; i<total; ++sk, ++i) {
      this->v_[sk]=B[i];
    }
  }
  return (*this);
}


template <typename T> inline
VecObj<T>& VecObj<T>::operator=(const VecObj<T> &B)
{
  if (&B == this) { return (*this); }
  int N = B.size();
  if (n_ == N) {
    copy(B.data());   // no re-alloc, just deep copy
  } else {
    destroy();        // release allocation
    initialize(N);    // rebuild
    copy(B.data());   // deep copy
  }
  return (*this);
}


template <typename T> inline
void VecObj<T>::fill(const T &x) 
{
  // fill array: unroll into blocks of 4, then fill tail
  int N = this->n_, i=0;
  int Nmod4 = N & 3;  int N4 = N - Nmod4;
  for (i=0; i<N4; i+=4) { 
    v_[i  ]=x; v_[i+1]=x; 
    v_[i+2]=x; v_[i+3]=x; 
  }
  for (i=N4; i<N; ++i)  { v_[i] = x; }
}


template <typename T> inline
void VecObj<T>::copy(const T* p) 
{
  // Note:
  // - assumes length of source and destination match.
  // - assumes object <T> defines operator=()

  int N = this->n_, i=0; 
  int Nmod4 = N & 3; int N4 = N - Nmod4;
  for (i=0; i<N4; i+=4) {
    v_[i  ]=p[i  ]; v_[i+1]=p[i+1];
    v_[i+2]=p[i+2]; v_[i+3]=p[i+3];
  }
  for (i=N4; i<N; ++i)  {v_[i]=p[i];}
}


//---------------------------------------------------------
//                  I/O
//---------------------------------------------------------
// template <typename T> inline std::ostream& operator<<(std::ostream &s, const VecObj<T> &A) { return s; }
// template <typename T> inline std::istream& operator>>(std::istream &s,       VecObj<T> &A) { return s; }

#endif  // NDG__VecObj_Type_H__INCLUDED
