// Region1D.h
// 1D sub-regions of vectors
// 2007/10/16
//---------------------------------------------------------
#ifndef NDG__Region1D_H__INCLUDED
#define NDG__Region1D_H__INCLUDED

#include <iostream>
#include <cassert>

#include "ArrayMacros.h"
#include "Index.h"


#define USE_BLAS_REGION 0

// declare "const" version
template <class Array1D> class const_Region1D;

//---------------------------------------------------------
template <class Array1D> 
class Region1D
//---------------------------------------------------------
{
protected:

  Array1D &  A_;
  int offset_;    // 0-based
  int dim_;

public:

  // typedef's for std::compatibility

  // data type of referenced array
  typedef typename Array1D::data_type   T;

  typedef         T   value_type;
  typedef         T   element_type;
  typedef         T*  pointer;
  typedef         T*  iterator;
  typedef         T&  reference;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;


  const Array1D & array()  const { return A_; }
        Array1D & array()        { return A_; }

  int lbound()        const { return A_.lbound(); }
  int offset(int i=1) const { return offset_;}
  int dim   (int i=1) const { return dim_; }
  int size  ()        const { return dim_; }
  const char* name()  const { return A_.name(); }


  Region1D(Array1D &V, int i1, int i2)
    : A_(V)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i1 );
    assert(i2 <= V.dim() + (lbound()-1));
    assert(i1 <= i2);
#endif

    offset_ = i1 - lbound();
       dim_ = i2-i1 + 1;
  }


  Region1D(Array1D &V, const Index1D &I) 
    : A_(V)
  {
#if (CHECK_ARRAY_INDEX)
    assert(  lbound() <= I.lbound());
    assert(I.ubound() <= V.dim() + (lbound()-1));
    assert(I.lbound() <= I.ubound());
#endif

    offset_ = I.lbound() - lbound();
       dim_ = I.ubound() - I.lbound() + 1;
  }


  Region1D(Region1D<Array1D> &V, int i1, int i2) 
    : A_(V.A_)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i1);
    assert(i2 <= V.dim() + (lbound()-1));
    assert(i1 <= i2);
#endif

    //         (old-offset)     (new-offset)
    //
    offset_ = (i1 - lbound()) + V.offset_;
       dim_ = i2-i1 + 1;
  }


  Region1D<Array1D> operator()(int i1, int i2)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i1);
    assert(i2 <= dim() + (lbound()-1));
    assert(i1 <= i2);
#endif

    // offset_ is 0-based
    return Region1D<Array1D>(A_, i1+offset_, offset_ + i2);
  }


  Region1D<Array1D> operator()(const Index1D &I)
  {
#if (CHECK_ARRAY_INDEX)
    assert(  lbound() <= I.lbound());
    assert(I.ubound() <= dim() + (lbound()-1));
    assert(I.lbound() <= I.ubound());
#endif

    return Region1D<Array1D>(A_, I.lbound()+offset_, 
                                 I.ubound()+offset_);
  }


  T & operator()(int i)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i);
    assert(i <= dim() + (lbound()-1));
#endif

    return A_(i+offset_);
  }


  const T & operator() (int i) const
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i);
    assert(i <= dim() + (lbound()-1));
#endif

    return A_(i+offset_);
  }


  Region1D<Array1D> & operator=(const Array1D &B)
  {
    // make sure both sides conform
    assert(dim() == B.dim());
    int N=dim(), istart=lbound();
    int iend = istart + N-1;

#if (USE_BLAS_REGION)
    //#####################################################
    if (sizeof(T) == sizeof(double)) {
      const double* p = (const double*)B.data();  // source
      double* my_v=(double*)(A_.data())+offset_;  // destination
      COPY(N, p, 1, my_v, 1);
    }
    // else ...
    //#####################################################
#else
      for (int i=istart; i<=iend; ++i)
        (*this)(i) = B(i);
#endif

    if (B.get_mode() == OBJ_temp) {delete (&B);} // clean up temps
    return *this;
  }


  Region1D<Array1D> & operator=(const Region1D<Array1D> &R)
  {
    // make sure both sides conform
    assert(dim() == R.dim());
    int N = dim();
    int istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<=iend; ++i)
      (*this)(i) = R(i);

    return *this;
  }


  Region1D<Array1D> & operator=(const const_Region1D<Array1D> &R)
  {
    // make sure both sides conform
    assert(dim() == R.dim());
    int N = dim();
    int istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<=iend; ++i)
      (*this)(i) = R(i);

    return *this;
  }


  //-------------------------------------
  // Apply scalar to each mapped element
  //-------------------------------------

  Region1D<Array1D> & operator=(const T& t)
  {
    int N=dim(), istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<= iend; ++i)
      (*this)(i) = t;

    return *this;
  }

  Region1D<Array1D> & operator += (const T& t)
  {
    int N=dim(), istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<= iend; ++i)
      (*this)(i) += t;

    return *this;
  }

  Region1D<Array1D> & operator -= (const T& t)
  {
    int N=dim(), istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<= iend; ++i)
      (*this)(i) -= t;

    return *this;
  }

  Region1D<Array1D> & operator *= (const T& t)
  {
    int N=dim(), istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<= iend; ++i)
      (*this)(i) *= t;

    return *this;
  }

  Region1D<Array1D> & operator /= (const T& t)
  {
    int N=dim(), istart = lbound();
    int iend = istart + N-1;
    for (int i=istart; i<= iend; ++i)
      (*this)(i) /= t;

    return *this;
  }


  //-------------------------------------
  // arithmetic operators += , -=
  //-------------------------------------

  Region1D<Array1D> & operator += (const Array1D &B)
  {
    int Nv = this->size();
    assert(Nv == B.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) += B(i);

    if (B.get_mode() == OBJ_temp) {delete (&B);} // delete temps
    return *this;
  }

  Region1D<Array1D> & operator -= (const Array1D &B)
  {
    int Nv = this->size();
    assert(Nv == B.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) -= B(i);

    if (B.get_mode() == OBJ_temp) {delete (&B);} // delete temps
    return *this;
  }

  Region1D<Array1D> & operator += (const Region1D<Array1D> &R)
  {
    int Nv = this->size();
    assert(Nv == R.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) += R(i);

    return *this;
  }

  Region1D<Array1D> & operator -= (const Region1D<Array1D> &R)
  {
    int Nv = this->size();
    assert(Nv == R.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) -= R(i);

    return *this;
  }

};



//---------------------------------------------------------
template <class Array1D>
class const_Region1D
//---------------------------------------------------------
{
protected:

  const Array1D &  A_;
  int offset_;          // 0-based
  int dim_;

public:

  // data type of referenced array
  typedef typename Array1D::data_type   T;

  typedef         T   value_type;
  typedef         T   element_type;
  typedef         T*  pointer;
  typedef         T*  iterator;
  typedef         T&  reference;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;

  const Array1D & array()  const { return A_; }

  int lbound()        const { return A_.lbound(); }
  int offset(int i=1) const { return offset_; }
  int dim   (int i=1) const { return dim_; }
  int size  ()        const { return dim_; }
  const char* name()  const { return A_.name(); }


  const_Region1D(const Array1D &V, int i1, int i2)
    : A_(V)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i1 );
    assert(i2 <= V.dim() + (lbound()-1));
    assert(i1 <= i2);
#endif

    offset_ = i1 - lbound();
       dim_ = i2-i1 + 1;
  }

  const_Region1D(const Array1D &V, const Index1D &I)
    : A_(V)
  {
#if (CHECK_ARRAY_INDEX)
    assert(  lbound() <= I.lbound());
    assert(I.ubound() <= V.dim() + (lbound()-1));
    assert(I.lbound() <= I.ubound());
#endif

    offset_ = I.lbound() - lbound();
       dim_ = I.ubound() - I.lbound() + 1;
  }


  const_Region1D(const_Region1D<Array1D> &V, int i1, int i2)
    : A_(V.A_)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i1);
    assert(i2 <= V.dim() + (lbound()-1));
    assert(i1 <= i2);
#endif

    //     (old-offset)        (new-offset)
    //
    offset_ = (i1 - lbound()) + V.offset_;
       dim_ = i2-i1 + 1;
  }


  const_Region1D<Array1D> operator()(int i1, int i2)
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i1);
    assert(i2 <= dim() + (lbound()-1));
    assert(i1 <= i2);
#endif

    // offset_ is 0-based
    return const_Region1D<Array1D>(A_, i1+offset_, offset_ + i2);
  }


  const_Region1D<Array1D> operator()(const Index1D &I)
  {
#if (CHECK_ARRAY_INDEX)
    assert(  lbound() <= I.lbound());
    assert(I.ubound() <= dim() + (lbound()-1));
    assert(I.lbound() <= I.ubound());
#endif

    return const_Region1D<Array1D>(A_, I.lbound()+offset_, 
                                       I.ubound()+offset_);
  }


  const T & operator() (int i) const
  {
#if (CHECK_ARRAY_INDEX)
    assert(lbound() <= i);
    assert(i <= dim() + (lbound()-1));
#endif

    return A_(i+offset_);
  }

};



//---------------------------------------------------------
// I/O : non-const Region1D
//---------------------------------------------------------
template <class Array1D>
std::ostream& operator<<(std::ostream &s, Region1D<Array1D> &R)
{
  int N = R.dim();
  int istart = R.lbound();
  int iend = N - 1 + R.lbound();
  for (int i=istart; i<=iend; i++)
    s << R(i) << std::endl;

  return s;
}


//---------------------------------------------------------
// I/O : const Region1D
//---------------------------------------------------------
template <class Array1D>
std::ostream& operator<<(std::ostream &s, const_Region1D<Array1D> &R)
{
  int N = R.dim();
  int istart = R.lbound();
  int iend = N - 1 + R.lbound();
  for (int i=istart; i<=iend; i++)
    s << R(i) << std::endl;

  return s;
}


#endif  // NDG__Region1D_H__INCLUDED
