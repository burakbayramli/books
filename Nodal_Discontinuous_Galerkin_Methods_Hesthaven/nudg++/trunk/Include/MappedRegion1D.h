// MappedRegion1D.h
// Mapped 1D sub-regions of vectors
// 2007/10/16
//---------------------------------------------------------
#ifndef NDG__MappedRegion1d_H__INCLUDED
#define NDG__MappedRegion1d_H__INCLUDED

#include <iostream>
#include <cassert>

#include "IMap.h"

// declare "const" version
template <class Array1D> class const_MappedRegion1D;


//---------------------------------------------------------
template <class Array1D> class MappedRegion1D
//---------------------------------------------------------
{
protected:

  Array1D &   A_;     // the mapped vector
  IMap        map_;   // id's of mapped values

  int   dim_;         // num entries in map
  int A_dim_;         // num entries in vector

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


        Array1D & array()        { return A_; }
  const Array1D & array()  const { return A_; }

  int lbound()        const { return A_.lbound(); }
  int dim   (int i=1) const { return dim_; }
  int size  ()        const { return dim_; }
  const char* name()  const { return A_.name(); }


  MappedRegion1D(Array1D &V, const IMap &I) 
    : A_(V), map_(I)
  {
    dim_ = I.size();
    A_dim_ = A_.size();
  }


  MappedRegion1D(MappedRegion1D<Array1D> &V, const IMap &I) 
    : A_(V.A_), map_(I)
  {
    umWARNING("Debugging MappedRegion1D", 
              "Mapped region passed to mapped region?");

    dim_ = I.size();
    A_dim_ = A_.size();
  }


  MappedRegion1D<Array1D> operator()(const IMap &I)
  {
    return MappedRegion1D<Array1D>( A_, I );
  }


  T & operator()(int i)
  {
#if (CHECK_ARRAY_INDEX)
    assert( map_(i) <= A_dim_ );
#endif

    return A_(map_(i));
  }


  const T & operator() (int i) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( map_(i) <= A_dim_ );
#endif

    return A_(map_(i));
  }


  MappedRegion1D<Array1D> & operator=(const Array1D &R)
  {
    // make sure both sides conform
    assert(dim() == R.dim());
    int N = dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) = R(i);

    if (R.get_mode() == OBJ_temp) {delete (&R);} // delete temps
    return *this;
  }

  MappedRegion1D<Array1D> & operator=(const MappedRegion1D<Array1D> &R)
  {
    // make sure both sides conform
    assert(dim() == R.dim());
    int N = dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) = R(i);      // assigns R.A_(map_(i))

    return *this;
  }


  MappedRegion1D<Array1D> & operator=(const const_MappedRegion1D<Array1D> &R)
  {
    // make sure both sides conform
    assert(dim() == R.dim());
    int N = dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) = R(i);      // assigns R.A_(map_(i))

    return *this;
  }


  //-------------------------------------
  // Apply scalar to each mapped element
  //-------------------------------------

  MappedRegion1D<Array1D> & operator=(const T& t)
  {
    int N=dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) = t;

    return *this;
  }

  MappedRegion1D<Array1D> & operator += (const T& t)
  {
    int N=dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) += t;

    return *this;
  }

  MappedRegion1D<Array1D> & operator -= (const T& t)
  {
    int N=dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) -= t;

    return *this;
  }

  MappedRegion1D<Array1D> & operator *= (const T& t)
  {
    int N=dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) *= t;

    return *this;
  }

  MappedRegion1D<Array1D> & operator /= (const T& t)
  {
    assert (T(0) != t);
    int N=dim();
    for (int i=1; i<=N; ++i)
      (*this)(i) /= t;

    return *this;
  }


  //-------------------------------------
  // arithmetic operators += , -=
  //-------------------------------------

  MappedRegion1D<Array1D> & operator += (const Array1D &B)
  {
    int Nv = this->size();
    assert(Nv == B.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) += B(i);

    if (B.get_mode() == OBJ_temp) {delete (&B);} // delete temps
    return *this;
  }

  MappedRegion1D<Array1D> & operator -= (const Array1D &B)
  {
    int Nv = this->size();
    assert(Nv == B.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) -= B(i);

    if (B.get_mode() == OBJ_temp) {delete (&B);} // delete temps
    return *this;
  }

  MappedRegion1D<Array1D> & operator += (const MappedRegion1D<Array1D> &R)
  {
    int Nv = this->size();
    assert(Nv == R.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) += R(i);

    return *this;
  }

  MappedRegion1D<Array1D> & operator -= (const MappedRegion1D<Array1D> &R)
  {
    int Nv = this->size();
    assert(Nv == R.size());
    for (int i=1; i<=Nv; ++i)
      (*this)(i) -= R(i);

    return *this;
  }

};



//---------------------------------------------------------
template <class Array1D> class const_MappedRegion1D
//---------------------------------------------------------
{
protected:

  const Array1D &   A_;     // the mapped vector
        IMap        map_;   // id's of mapped values

  int   dim_;               // num entries in map
  int A_dim_;               // num entries in vector

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
  int dim   (int i=1) const { return dim_; }
  int size  ()        const { return dim_; }
  const char* name()  const { return A_.name(); }


  const_MappedRegion1D(const Array1D &V, const IMap &I) 
    : A_(V), map_(I)
  {
    dim_ = I.size();
    A_dim_ = A_.size();
  }


  const_MappedRegion1D(const_MappedRegion1D<Array1D> &V, const IMap &I) 
    : A_(V.A_), map_(I)
  {
    umWARNING("Nigel: debugging const_MappedRegion1D", 
              "const Mapped region passed to mapped region");

    dim_ = I.size();
    A_dim_ = A_.size();
  }


  const_MappedRegion1D<Array1D> operator()(const IMap &I) const
  {
    return const_MappedRegion1D<Array1D>( A_, I );
  }


  const T & operator() (int i) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( map_(i) <= A_dim_ );
#endif

    return A_(map_(i));
  }

};



//---------------------------------------------------------
// I/O : non-const MappedRegion1D
//---------------------------------------------------------
template <class Array1D>
std::ostream& operator<<(std::ostream &s, MappedRegion1D<Array1D> &R)
{
  int N = R.dim();
  for (int i=1; i<=N; i++)
    s << R(i) << std::endl;
  return s;
}


//---------------------------------------------------------
// I/O : const MappedRegion1D
//---------------------------------------------------------
template <class Array1D>
std::ostream& operator<<(std::ostream &s, const_MappedRegion1D<Array1D> &R)
{
  int N = R.dim();
  for (int i=1; i<=N; ++i)
    s << R(i) << std::endl;
  return s;
}

#endif  // NDG__MappedRegion1d_H__INCLUDED
