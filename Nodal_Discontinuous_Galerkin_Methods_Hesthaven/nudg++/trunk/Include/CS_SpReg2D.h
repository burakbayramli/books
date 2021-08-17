// CS_SpReg2D.h
// 2D sub-regions of sparse matrices
// 2007/09/28
//---------------------------------------------------------
#ifndef NDG__CS_SpReg2D_H__INCLUDED
#define NDG__CS_SpReg2D_H__INCLUDED

//#include <iostream>
//#include <cassert>

#include "ArrayMacros.h"
#include "Index.h"


//---------------------------------------------------------
template <class SpMat> 
class SpReg2D
//---------------------------------------------------------
{
protected:

  SpMat &  A_;
  int offset_[2];   // 0-based
  int dim_[2];

public:

  // typedef's for std::compatibility

  // data type of referenced array
  typedef typename SpMat::m_data_type T;

//typedef         T   value_type;
  typedef         T   element_type;
  typedef         T*  pointer;
  typedef         T*  iterator;
  typedef         T&  reference;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;

  T m_ZERO;

        SpMat & array()         { return A_; }
  const SpMat & array()  const  { return A_; }

  int lbound() const    { return 1; }
  int size() const      { return A_.size(); }
  int num_rows() const  { return dim_[0]; }
  int num_cols() const  { return dim_[1]; }
  const char* name() const { return A_.name(); }

  int offset(int i) const     // 1-offset
  {
#if (CHECK_ARRAY_INDEX)
    assert( A_.lbound() <= i);
    assert( i<= dim_[0] + A_.lbound()-1);
#endif
    return offset_[i-A_.lbound()];
  }


  SpReg2D(SpMat &A, int i1, int i2, 
                    int j1, int j2) 
    : A_(A), m_ZERO(T(0))
  {
#if (CHECK_ARRAY_INDEX)
    assert( i1 <= i2 );
    assert( j1 <= j2);
    assert( A.lbound() <= i1);
    assert( i2<= A.dim(A.lbound()) + A.lbound()-1);
    assert( A.lbound() <= j1);
    assert( j2<= A.dim(A.lbound()+1) + A.lbound()-1 );
#endif

    offset_[0] = i1-A.lbound();
    offset_[1] = j1-A.lbound();
    dim_[0] = i2-i1+1;
    dim_[1] = j2-j1+1;
  }

  SpReg2D(SpMat &A, const Index1D &I, const int j) 
    : A_(A), m_ZERO(T(0))
  {
    // This region will be part of a sparse column
    // m_mode = eCol;

#if (CHECK_ARRAY_INDEX)
    assert( I.lbound() <= I.ubound() );
    assert( A.lbound() <= I.lbound() );
    assert( I.ubound() <= A.dim(A.lbound()) + A.lbound()-1);
    assert( A.lbound() <= j );
    assert( j          <= A.dim(A.lbound()+1) + A.lbound()-1 );
#endif

    offset_[0] = I.lbound() - A.lbound();
    offset_[1] = j          - A.lbound();
       dim_[0] = I.ubound() - I.lbound() + 1;
       dim_[1] = 1;
  }


  SpReg2D(SpMat &A, const int i, const Index1D &J) 
    : A_(A), m_ZERO(T(0))
  {
    // This region will be part of a sparse row
    // m_mode = eRow;

#if (CHECK_ARRAY_INDEX)
    assert( J.lbound() <= J.ubound() );
    assert( A.lbound() <= i );
    assert( i          <= A.dim(A.lbound()) + A.lbound()-1);
    assert( A.lbound() <= J.lbound() );
    assert( J.ubound() <= A.dim(A.lbound()+1) + A.lbound()-1 );
#endif

    offset_[0] = i          - A.lbound();
    offset_[1] = J.lbound() - A.lbound();
       dim_[0] = 1;
       dim_[1] = J.ubound() - J.lbound() + 1;
  }



  SpReg2D(SpMat &A, const Index1D &I, 
                    const Index1D &J) 
    : A_(A), m_ZERO(T(0))
  {
#if (CHECK_ARRAY_INDEX)
    assert( I.lbound() <= I.ubound() );
    assert( J.lbound() <= J.ubound() );
    assert( A.lbound() <= I.lbound() );
    assert( I.ubound() <= A.dim(A.lbound()) + A.lbound()-1);
    assert( A.lbound() <= J.lbound() );
    assert( J.ubound() <= A.dim(A.lbound()+1) + A.lbound()-1 );
#endif

    offset_[0] = I.lbound()-A.lbound();
    offset_[1] = J.lbound()-A.lbound();
    dim_[0] = I.ubound() - I.lbound() + 1;
    dim_[1] = J.ubound() - J.lbound() + 1;
  }


  SpReg2D(SpReg2D<SpMat> &A, int i1, int i2, 
                             int j1, int j2)
    : A_(A.A_), m_ZERO(T(0))
  {
#if (CHECK_ARRAY_INDEX)
    assert( i1 <= i2 );
    assert( j1 <= j2);
    assert( A.lbound() <= i1);
    assert( i2 <= A.dim(A.lbound()) + A.lbound()-1);
    assert( A.lbound() <= j1);
    assert( j2 <= A.dim(A.lbound()+1) + A.lbound()-1 );
#endif

    offset_[0] = (i1 - A.lbound()) + A.offset_[0];
    offset_[1] = (j1 - A.lbound()) + A.offset_[1];
    dim_[0] = i2-i1 + 1;
    dim_[1] = j2-j1 + 1;
  }

  SpReg2D<SpMat> operator()(int i1, int i2,
                            int j1, int j2)
  {
#if (CHECK_ARRAY_INDEX)
    assert( i1 <= i2 );
    assert( j1 <= j2);
    assert( A_.lbound() <= i1);
    assert( i2 <= dim_[0] + A_.lbound()-1);
    assert( A_.lbound() <= j1);
    assert( j2 <= dim_[1] + A_.lbound()-1 );
#endif

    return SpReg2D<SpMat>(A_, i1+offset_[0], offset_[0] + i2, 
                              j1+offset_[1], offset_[1] + j2);
  }


  SpReg2D<SpMat> operator()(const Index1D &I, const Index1D &J)
  {
#if (CHECK_ARRAY_INDEX)
    assert( I.lbound() <= I.ubound() );
    assert( J.lbound() <= J.ubound() );
    assert( A_.lbound()<= I.lbound());
    assert( I.ubound() <= dim_[0] + A_.lbound()-1);
    assert( A_.lbound()<= J.lbound());
    assert( J.ubound() <= dim_[1] + A_.lbound()-1 );
#endif

    return SpReg2D<SpMat>(A_, I.lbound()+offset_[0],
      offset_[0] + I.ubound(), offset_[1]+J.lbound(),
      offset_[1] + J.ubound());
  }


  inline T& operator()(int i, int j)
  {
#if (CHECK_ARRAY_INDEX)
    assert( A_.lbound() <= i);
    assert( i <= dim_[0] + A_.lbound()-1);
    assert( A_.lbound() <= j);
    assert( j <= dim_[1] + A_.lbound()-1 );
#endif

#if (THIS_IS_READY)
    return A_(i+offset_[0], j+offset_[1]);
#else
    return m_ZERO;
#endif
  }

  inline const T& operator() (int i, int j) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( A_.lbound() <= i);
    assert( i<= dim_[0] + A_.lbound()-1);
    assert( A_.lbound() <= j);
    assert( j<= dim_[1] + A_.lbound()-1 );
#endif

#if (THIS_IS_READY)
    return A_(i+offset_[0], j+offset_[1]);
#else
    return m_ZERO;
#endif
  }


  SpReg2D<SpMat>& operator=(const SpReg2D<SpMat> &R)
  {
    int M = num_rows(); 
    int N = num_cols();

    // make sure both sides conform
    assert(M == R.num_rows());
    assert(N == R.num_cols());

    int start = R.lbound();
    int Mend =  start + M - 1;
    int Nend =  start + N - 1;

#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = R(i,j);
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }


  SpReg2D<SpMat>& operator=(const SpMat &R)
  {
    int M = num_rows(); 
    int N = num_cols();

    // make sure both sides conform
    assert(M == R.num_rows());
    assert(N == R.num_cols());

    int start = R.lbound();
    int Mend =  start + M - 1;
    int Nend =  start + N - 1;

#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = R(i,j);
    //###################################
#endif // (THIS_IS_READY)

    // if R is temporary, delete it.
    if (R.get_mode() == OBJ_temp)
      delete (&R);

    return *this;
  }

  SpReg2D<SpMat>& operator=(const Vector<T> &V)
  {
    int M=num_rows(), N=num_cols(), len=V.length();
    bool load_col = true; int ir=0,jc=0,i=0,j=0;

    // load a dense vector into a region of 
    // one column or row of a sparse matrix

    if (1==N) {
      // load a region of one column
      load_col = true;
      if (len!=M) { umERROR("Sparse region = Vector", "vector has length %d, expected %d", len, M); }
    } else if (1==M) {
      // load a region of one row
      load_col = false; 
      if (len!=N) { umERROR("Sparse region = Vector", "vector has length %d, expected %d", len, M); }
    } else {
      umERROR("Sparse region = Vector", "region is (%d,%d), expected 1d region", M, N);
    }

    if (load_col) {
      j=1;  // jc = 1+offset_[1];      
      for (i=1; i<=len; ++i) {  // load region of a sparse column
        ir = i+offset_[0];
        jc = j+offset_[1];
        A_.set1(ir, jc, V(i));
      }
    } else {
      i=1;  // ir = 1+offset_[0];
      for (j=1; j<=len; ++j) {  // load region of a sparse row
        ir = i+offset_[0];
        jc = j+offset_[1];
        A_.set1(ir, jc, V(i));
      }
    }

    if (V.get_mode() == OBJ_temp) delete (&V);  // cleanup

    return *this;
  }


  //-------------------------------------
  // Apply scalar to each mapped element
  //-------------------------------------

  SpReg2D<SpMat>& operator=(const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = t;
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }

  SpReg2D<SpMat>& operator *= (const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) *= t;
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }

  SpReg2D<SpMat>& operator /= (const T& t)
  {
    assert (T(0) != t);
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) /= t;
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }

  SpReg2D<SpMat>& operator += (const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) += t;
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }

  SpReg2D<SpMat>& operator -= (const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
#if (THIS_IS_READY)
    //###################################
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) -= t;
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }


  //#######################################################
  // arithmetic operators +,-,*,/ 
  //#######################################################
  SpReg2D<SpMat> & operator+=(const SpReg2D<SpMat> &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

#if (THIS_IS_READY)
    //###################################
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) += R(i,j);
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }

  SpReg2D<SpMat> & operator-=(const SpReg2D<SpMat> &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

#if (THIS_IS_READY)
    //###################################
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) -= R(i,j);
    //###################################
#endif // (THIS_IS_READY)

    return *this;
  }

};


//---------------------------------------------------------
// std::ostream algorithms
//---------------------------------------------------------


template <class SpMat>
std::ostream& operator<<(std::ostream &s, const SpReg2D<SpMat> &A)
{
#if (THIS_IS_READY)
  //###################################

  int start = A.lbound();
  int Mend=A.lbound()+ A.num_rows() - 1;
  int Nend=A.lbound() + A.num_cols() - 1;

  s << A.num_rows() << "  " << A.num_cols() << "\n";
  for (int i=start; i<=Mend; ++i) {
    for (int j=start; j<=Nend; ++j) {
      s << A(i,j) << " ";
    }
    s << "\n";
  }

  //###################################
#endif // (THIS_IS_READY)

  s.flush();
  return s;
}

#endif  // NDG__CS_SpReg2D_H__INCLUDED
