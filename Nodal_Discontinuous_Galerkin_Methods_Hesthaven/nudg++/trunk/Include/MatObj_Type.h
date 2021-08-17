// MatObj_Type.h: simple 2-D array of objects
// column-wise, 1-based (i,j) indexing
// 2007/10/18
//---------------------------------------------------------
#ifndef NDG__MatObj_Type_H__INCLUDED
#define NDG__MatObj_Type_H__INCLUDED

#include <cstdlib>
#include <cassert>
#include "ArrayMacros.h"


// typedef versions for common data types
template <typename T> class MatObj;

typedef MatObj<double>  DM;
typedef MatObj<int>     IM;

// for debug print
void dumpDM(const DM& M, const char* s);
void dumpIM(const IM& M, const char* s);


//---------------------------------------------------------
template <typename T>
class MatObj
//---------------------------------------------------------
{

protected:

  T*    v_;       // pointer to calloc'd alloc.
  T**   col_;     // pointers to 1-offset columns
  int   m_, n_;   // num_rows, num_cols

  void initialize(int M, int N);
  void copy(const T* p);

public:

  // constructors
  MatObj()                   : v_(0),col_(0),m_(0),n_(0) {;}
  MatObj(const MatObj<T> &B) : v_(0),col_(0),m_(0),n_(0) {copy(B);}
  MatObj(int M, int N)       : v_(0),col_(0),m_(0),n_(0) {initialize(M,N);}
  MatObj(int M, int N, T* p) : v_(0),col_(0),m_(0),n_(0) {
    initialize(M,N); int len = M*N;
    for (int i=0; i<len; ++i) { v_[i] = p[i]; }
  }


  // destructor
  ~MatObj() { destroy(); }
  void destroy();

  // status
  bool  ok()        const { return (v_ && col_ && m_>0 && n_>0); }
  int   size()      const { return (m_*n_); }
  int   length()    const { return (m_*n_); }
  int   num_rows()  const { return  m_; }
  int   num_cols()  const { return  n_; }
  bool  resize(int M, int N);

  // access to data
  const T* data()   const { return v_; }
        T* data()         { return v_; }

  // return pointer to data in col j
        T* pCol(int j)       {Check_idx_Col(j); return col_[j]+1;}
  const T* pCol(int j) const {Check_idx_Col(j); return col_[j]+1;}


  // 1-based element access
  const T& operator()(int i, int j) const {Check_index_IJ(i,j); return col_[j][i];}
        T& operator()(int i, int j)       {Check_index_IJ(i,j); return col_[j][i];}

  // assignment
  MatObj<T>& operator=(const T& x) { fill(x); return (*this); }
  MatObj<T>& operator=(const MatObj<T> &B);

  void copy(const MatObj<T>& rB) { (*this) =   rB ; }
  void copy(const MatObj<T>* pB) { (*this) = (*pB); }
  void fill(const T &x);

  void append_col(int len, T* p);


protected:

  //-------------------------------------
  // Optional index checking
  //-------------------------------------
#if (CHECK_ARRAY_INDEX)
  // 1-based indices
  void Check_index_IJ (int i, int j) const { 
    if (i<1) throw 1;   if (i> m_) throw 2;
    if (j<1) throw 3;   if (j> n_) throw 4;
  }
  void Check_idx_Row(int i) const {if (i<1) throw 1; if (i>m_)      throw 2;}
  void Check_idx_Col(int j) const {if (j<1) throw 1; if (j>n_)      throw 2;}
  void Check_idx_Len(int k) const {if (k<1) throw 1; if (k>(m_*n_)) throw 2;}
#else
  // 1-based indices
  void Check_index_IJ(int i, int j) const {}
  void Check_idx_Row(int i) const {}
  void Check_idx_Col(int j) const {}
  void Check_idx_Len(int k) const {}
#endif

};



//---------------------------------------------------------
// define member functions
//---------------------------------------------------------

template <typename T> inline
void MatObj<T>::initialize(int M, int N)
{
  // allocate the data array, and build the 
  // array of 1-based column pointers.

  assert(NULL == v_  );   // should be empty
  assert(NULL == col_);   // should be empty
  assert(0==m_&&0==n_);   // should be empty
  assert(M>=0 && N>=0);

  if (M<1 || N<1) {
    // allow empty initialization
    return; 
  }

  // Please note: use new (not calloc) to ensure 
  // that each object's constructor is called!

  v_   = new T[M*N];  // array of objects
  col_ = new T* [N];  // column pointers

  assert(v_ && col_);

  // set matrix dimensions (m,n)
  m_ = M; n_ = N;

  // adjust pointers for 1-based indexing
  T* p = v_ - 1;
  for (int i=0; i<N; ++i) {
    col_[i] = p;
    p += M;
  }

  col_ -- ;   // adjust for 1-based indexing
}


template <typename T> inline
void MatObj<T>::destroy()
{
  if (v_)   {        delete [] v_;  } // delete objects
  if (col_) {col_++; delete [] col_;} // delete col pointers
  v_=NULL; col_=NULL; m_=0; n_=0;     // zero in case reused
}


template <typename T> inline
bool MatObj<T>::resize(int M, int N) 
{
  // resize allocation
  if (M==m_ && N==n_) { return false; }
  destroy();
  if (M>0 && N>0) { initialize(M,N); }
  return true;
}


template <typename T> inline
MatObj<T>& MatObj<T>::operator=(const MatObj<T> &B)
{
  if (&B == this) { return (*this); }
  int Nr = B.num_rows(), Nc = B.num_cols();
  if ((Nr==m_) && (Nc==n_)) {
    copy(B.data());     // no re-alloc, just deep copy
  } else {
    destroy();          // release allocation
    initialize(Nr,Nc);  // rebuild
    copy(B.data());     // deep copy
  }
  return (*this);
}


template <typename T> inline
void MatObj<T>::fill(const T &x) 
{
  // fill array: unroll into blocks of 4, then fill tail
  int N = m_*n_, i=0;
  int Nmod4 = N & 3;  int N4 = N - Nmod4;
  for (i=0; i<N4; i+=4) { 
    v_[i  ]=x; v_[i+1]=x; 
    v_[i+2]=x; v_[i+3]=x; 
  }
  for (i=N4; i<N; ++i)  { v_[i]=x; }
}


template <typename T> inline
void MatObj<T>::copy(const T* p) 
{
  // Note:
  // - assumes length of source and destination match.
  // - assumes object <T> defines operator=()
  int N = m_*n_, i=0;
  int Nmod4 = N & 3; int N4 = N - Nmod4;
  for (i=0; i<N4; i+=4) {
    v_[i  ]=p[i  ]; v_[i+1]=p[i+1];
    v_[i+2]=p[i+2]; v_[i+3]=p[i+3];
  }
  for (i=N4; i<N; ++i)  {v_[i]=p[i];}
}


template <typename T> inline
void MatObj<T>::append_col(int len, T* p)
{
  // To append a column, simply append new data to tail 
  // of existing array, then adjust logical dimensions.
  // Note: handle case of appending 1st col to empty mat

  // check for nothing to do
  if (len<1 || !p) 
    return;  

  int i=0;
  if (!ok()) {
    resize(len, 1);           // appending "1st" col to empty mat
    for (i=1; i<=len; ++i) {
      col_[1][i] = p[i-1];    // load data into new column 
    }
  } 
  else 
  {
    assert(len >= m_);  // assume enough data for new column
    int Ni = size();    // store length of current data array
    int Nr = m_;        // store current row count
    int Nc = n_ + 1;    // set new column count
    T* tpv = v_;        // store existing data

    v_ = NULL;          // unlink pointer from data
    destroy();          // reset members
    initialize(Nr,Nc);  // re-alloc expanded object

    // reload old data
    for (i=0; i<Ni; ++i) { v_[i] = tpv[i]; }

    // load data for new column
    for (i=1; i<=Nr; ++i) { col_[Nc][i] = p[i-1];  }

    // finally, delete old data
    delete [] tpv;
  }
}


//---------------------------------------------------------
//                  I/O
//---------------------------------------------------------
// template <typename T> inline std::ostream& operator<<(std::ostream &s, const MatObj<T> &A) { return s; }
// template <typename T> inline std::istream& operator>>(std::istream &s,       MatObj<T> &A) { return s; }

#endif  // NDG__MatObj_Type_H__INCLUDED
