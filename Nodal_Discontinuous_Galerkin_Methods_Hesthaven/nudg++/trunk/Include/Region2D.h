// Region2D.h
// 2D sub-regions of matrices
// 2007/10/16
//---------------------------------------------------------
#ifndef NDG__Region_22D_H__INCLUDED
#define NDG__Region_22D_H__INCLUDED

#include <iostream>
#include <cassert>

#include "ArrayMacros.h"
#include "Index.h"


// declare "const" version
template <class Array2D> class const_Region2D;

//---------------------------------------------------------
template <class Array2D> class Region2D
//---------------------------------------------------------
{
protected:

  Array2D &  A_;
  int offset_[2];   // 0-based
  int dim_[2];

public:

  // typedef's for std::compatibility

  // data type of referenced array
  typedef typename Array2D::m_data_type T;

  typedef         T   value_type;
  typedef         T   element_type;
  typedef         T*  pointer;
  typedef         T*  iterator;
  typedef         T&  reference;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;

        Array2D & array()         { return A_; }
  const Array2D & array()  const  { return A_; }

  int lbound() const    { return A_.lbound(); }
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

  int dim(int i) const
  {
    umWARNING("FIXME: Region2D<T>""dim(%d)", "checking who calls this code?", i);

#if (1)
    return dim_[i-A_.lbound()];
#else
    return (1==i)?dim_[0]:dim_[1];
#endif
  }


  Region2D(Array2D &A, int i1, int i2, 
                       int j1, int j2) 
    : A_(A)
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

  Region2D(Array2D &A, const Index1D &I, 
                       const Index1D &J) 
    : A_(A)
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


  Region2D(Region2D<Array2D> &A, int i1, int i2, 
                                 int j1, int j2)
    : A_(A.A_)
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

  Region2D<Array2D> operator()(int i1, int i2,
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

    return Region2D<Array2D>(A_, i1+offset_[0], offset_[0] + i2, 
                                 j1+offset_[1], offset_[1] + j2);
  }


  Region2D<Array2D> operator()(const Index1D &I, const Index1D &J)
  {
#if (CHECK_ARRAY_INDEX)
    assert( I.lbound() <= I.ubound() );
    assert( J.lbound() <= J.ubound() );
    assert( A_.lbound()<= I.lbound());
    assert( I.ubound() <= dim_[0] + A_.lbound()-1);
    assert( A_.lbound()<= J.lbound());
    assert( J.ubound() <= dim_[1] + A_.lbound()-1 );
#endif

    return Region2D<Array2D>(A_, I.lbound()+offset_[0],
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

    return A_(i+offset_[0], j+offset_[1]);
  }

  inline const T& operator() (int i, int j) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( A_.lbound() <= i);
    assert( i<= dim_[0] + A_.lbound()-1);
    assert( A_.lbound() <= j);
    assert( j<= dim_[1] + A_.lbound()-1 );
#endif

    return A_(i+offset_[0], j+offset_[1]);
  }


  Region2D<Array2D>& operator=(const Region2D<Array2D> &R)
  {
    int M = num_rows(); 
    int N = num_cols();

    // make sure both sides conform
    assert(M == R.num_rows());
    assert(N == R.num_cols());

    int start = R.lbound();
    int Mend =  start + M - 1;
    int Nend =  start + N - 1;

    for (int j=start; j<=Nend; ++j)
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = R(i,j);

    return *this;
  }

  Region2D<Array2D>& operator=(const const_Region2D<Array2D> &R)
  {
    int M = num_rows(); 
    int N = num_cols();

    // make sure both sides conform
    assert(M == R.num_rows());
    assert(N == R.num_cols());

    int start = R.lbound();
    int Mend =  start + M - 1;
    int Nend =  start + N - 1;

    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = R(i,j);

    return *this;
  }

  Region2D<Array2D>& operator=(const Array2D &R)
  {
    int M = num_rows(); 
    int N = num_cols();

    // make sure both sides conform
    assert(M == R.num_rows());
    assert(N == R.num_cols());

    int start = R.lbound();
    int Mend =  start + M - 1;
    int Nend =  start + N - 1;

    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = R(i,j);

    // if R is temporary, delete it.
    if (R.get_mode() == OBJ_temp)
      delete (&R);

    return *this;
  }

  Region2D<Array2D>& operator=(const Vector<T> &V)
  {
    // this Region2D maps one "row" of a matrix
    // load a vector into this row:

    int M=num_rows(), N=num_cols();

    // make sure both sides conform
    assert(       1 == M);
    assert(V.size() == N);

    for (int j=1; j<=N; ++j)
      (*this)(1,j) = V(j);

    // if V is temporary, delete it.
    if (V.get_mode() == OBJ_temp) {delete (&V);}
    return *this;
  }


  //-------------------------------------
  // Apply scalar to each mapped element
  //-------------------------------------

  Region2D<Array2D>& operator=(const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) = t;

    return *this;
  }

  Region2D<Array2D>& operator += (const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) += t;

    return *this;
  }

  Region2D<Array2D>& operator -= (const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) -= t;

    return *this;
  }

  Region2D<Array2D>& operator *= (const T& t)
  {
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) *= t;

    return *this;
  }

  Region2D<Array2D>& operator /= (const T& t)
  {
    assert (T(0) != t);
    int start = lbound();
    int Mend = lbound() + num_rows() - 1;
    int Nend = lbound() + num_cols() - 1;
    
    for (int j=start; j<=Nend; ++j)     // assume col-major
      for (int i=start; i<=Mend; ++i)
        (*this)(i,j) /= t;

    return *this;
  }


  //-------------------------------------
  // arithmetic operators += , -=
  //-------------------------------------

  Region2D<Array2D> & operator += (const Array2D &B)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == B.num_rows());  // assume both sides conform
    assert(N == B.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) += B(i,j);

    if (B.get_mode() == OBJ_temp) {delete (&B);} // clean up temps
    return *this;
  }

  Region2D<Array2D> & operator -= (const Array2D &B)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == B.num_rows());  // assume both sides conform
    assert(N == B.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) -= B(i,j);

    if (B.get_mode() == OBJ_temp) {delete (&B);} // clean up temps
    return *this;
  }

  Region2D<Array2D> & operator += (const Region2D<Array2D> &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) += R(i,j);

    return *this;
  }

  Region2D<Array2D> & operator -= (const Region2D<Array2D> &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) -= R(i,j);

    return *this;
  }

};



//---------------------------------------------------------
template <class Array2D> 
class const_Region2D
//---------------------------------------------------------
{
protected:

  const Array2D &  A_;
  int offset_[2];       // 0-based
  int dim_[2];

public:
  typedef typename Array2D::m_data_type T;

  typedef         T   value_type;
  typedef         T   element_type;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;

  const Array2D & array() const { return A_; }
  int lbound() const   { return A_.lbound(); }
  int size() const     { return A_.size(); }
  int num_rows() const { return dim_[0]; }
  int num_cols() const { return dim_[1]; }
  const char* name() const { return A_.name(); }

  int offset(int i) const                 // 1-offset
  {
#if (CHECK_ARRAY_INDEX)
    assert( RGN_BASE_OFFSET <= i);
    assert( i<= dim_[0] + RGN_BASE_OFFSET-1);
#endif
    return offset_[i-RGN_BASE_OFFSET];
  }


  int dim(int i) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( RGN_BASE_OFFSET <= i);
    assert( i<= dim_[0] + RGN_BASE_OFFSET-1);
#endif
    return dim_[i-RGN_BASE_OFFSET];
  }


  const_Region2D(const Array2D &A, int i1, int i2, 
                                   int j1, int j2) : A_(A)
  {
#if (CHECK_ARRAY_INDEX)
    assert( i1 <= i2 );
    assert( j1 <= j2);
    assert( RGN_BASE_OFFSET <= i1);
    assert( i2<= A.dim(RGN_BASE_OFFSET) + RGN_BASE_OFFSET-1);
    assert( RGN_BASE_OFFSET <= j1);
    assert( j2<= A.dim(RGN_BASE_OFFSET+1) + RGN_BASE_OFFSET-1 );
#endif

    offset_[0] = i1-RGN_BASE_OFFSET;
    offset_[1] = j1-RGN_BASE_OFFSET;
    dim_[0] = i2-i1+1;
    dim_[1] = j2-j1+1;
  }

  const_Region2D(const Array2D &A, const Index1D &I, const Index1D &J) : A_(A)
  {
#if (CHECK_ARRAY_INDEX)
    assert( I.lbound() <= I.ubound() );
    assert( J.lbound() <= J.ubound() );
    assert( RGN_BASE_OFFSET <= I.lbound());
    assert( I.ubound()<= A.dim(RGN_BASE_OFFSET) + RGN_BASE_OFFSET-1);
    assert( RGN_BASE_OFFSET <= J.lbound());
    assert( J.ubound() <= A.dim(RGN_BASE_OFFSET+1) + RGN_BASE_OFFSET-1 );
#endif

    offset_[0] = I.lbound()-RGN_BASE_OFFSET;
    offset_[1] = J.lbound()-RGN_BASE_OFFSET;
    dim_[0] = I.ubound() - I.lbound() + 1;
    dim_[1] = J.ubound() - J.lbound() + 1;
  }


  const_Region2D(const_Region2D<Array2D> &A, int i1, int i2, 
                                             int j1, int j2) : A_(A.A_)
  {
#if (CHECK_ARRAY_INDEX)
    assert( i1 <= i2 );
    assert( j1 <= j2);
    assert( RGN_BASE_OFFSET <= i1);
    assert( i2<= A.dim(RGN_BASE_OFFSET) + RGN_BASE_OFFSET-1);
    assert( RGN_BASE_OFFSET <= j1);
    assert( j2<= A.dim(RGN_BASE_OFFSET+1) + RGN_BASE_OFFSET-1 );
#endif

    offset_[0] = (i1 - RGN_BASE_OFFSET) + A.offset_[0];
    offset_[1] = (j1 - RGN_BASE_OFFSET) + A.offset_[1];
    dim_[0] = i2-i1 + 1;
    dim_[1] = j2-j1+1;
  }

  const_Region2D<Array2D> operator()(int i1, int i2, int j1, int j2)
  {
#if (CHECK_ARRAY_INDEX)
    assert( i1 <= i2 );
    assert( j1 <= j2);
    assert( RGN_BASE_OFFSET <= i1);
    assert( i2<= dim_[0] + RGN_BASE_OFFSET-1);
    assert( RGN_BASE_OFFSET <= j1);
    assert( j2<= dim_[0] + RGN_BASE_OFFSET-1 );
#endif

    return const_Region2D<Array2D>(A_, i1+offset_[0], offset_[0] + i2, 
                                       j1+offset_[1], offset_[1] + j2);
  }

  const_Region2D<Array2D> operator()(const Index1D &I, const Index1D &J)
  {
#if (CHECK_ARRAY_INDEX)
    assert( I.lbound() <= I.ubound() );
    assert( J.lbound() <= J.ubound() );
    assert( RGN_BASE_OFFSET <= I.lbound());
    assert( I.ubound()<= dim_[0] + RGN_BASE_OFFSET-1);
    assert( RGN_BASE_OFFSET <= J.lbound());
    assert( J.ubound() <= dim_[1] + RGN_BASE_OFFSET-1 );
#endif

    return const_Region2D<Array2D>(A_, I.lbound()+offset_[0], offset_[0] + I.ubound(), 
                                       offset_[1]+J.lbound(), offset_[1] + J.ubound());
  }

  inline const T & operator() (int i, int j) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( RGN_BASE_OFFSET <= i);
    assert( i<= dim_[0] + RGN_BASE_OFFSET-1);
    assert( RGN_BASE_OFFSET <= j);
    assert( j<= dim_[1] + RGN_BASE_OFFSET-1 );
#endif
    return A_(i+offset_[0], j+offset_[1]);
  }

};


//---------------------------------------------------------
// std::ostream algorithms
//---------------------------------------------------------

template <class Array2D>
std::ostream& operator<<(std::ostream &s, const const_Region2D<Array2D> &A)
{
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
  s.flush();
  return s;
}


template <class Array2D>
std::ostream& operator<<(std::ostream &s, const Region2D<Array2D> &A)
{
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
  s.flush();
  return s;
}

#endif  // NDG__Region_22D_H__INCLUDED
