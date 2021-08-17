// MappedRegion2D.h
// Mapped 2D sub-regions of matrices
// 2007/10/16
//---------------------------------------------------------
#ifndef NDG__MappedRegion_22D_H__INCLUDED
#define NDG__MappedRegion_22D_H__INCLUDED

#include <iostream>
#include <cassert>

#include "IMap.h"

// declare "const" version
template <class Array2D> class const_MappedRegion2D;


//---------------------------------------------------------
template <class Array2D> class MappedRegion2D
//---------------------------------------------------------
{
protected:

  Array2D & A_;     // the mapped matrix
  IMap      Rmap_;  // id's of mapped "Rows"
  IMap      Cmap_;  // id's of mapped "Cols"

  int   dim_[2];    // num entries in {R,C} maps
  int A_dim_[2];    // num entries in matrix

public:

  // data type of referenced array
  typedef typename Array2D::m_data_type   T;

  typedef         T   value_type;
  typedef         T   element_type;
  typedef         T*  pointer;
  typedef         T*  iterator;
  typedef         T&  reference;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;


        Array2D & array()        { return A_; }
  const Array2D & array()  const { return A_; }

  int lbound() const      { return A_.lbound(); }
  int size() const        { return A_.size(); }
  int num_rows() const    { return dim_[0]; }
  int num_cols() const    { return dim_[1]; }
  int dim(int i) const    { return (1==i)?dim_[0]:dim_[1]; }
  const char* name() const{ return A_.name(); }

  // Map subset of Rows in matrix
  MappedRegion2D(Array2D &V, const IMap &I, const MatDimension&)
    : A_(V), Rmap_(I)
  {
       int Nc = A_.num_cols();  // num cols in Cmap (All)
      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = Nc;             // mapping entire rows
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = Nc;             // num cols in matrix
    Cmap_.linspace(1,Nc, Nc);   // map "All" cols (start,stop, len)
  }

  MappedRegion2D(Array2D &V, const Index1D &IDX, const MatDimension&)
    : A_(V)
  {
    // Build Rmap (selected rows):
    int i1=IDX.lo(), i2=IDX.hi(), Nr=IDX.N();
    Rmap_.linspace(i1,i2, Nr);  // map rows [i1:i2]

       int Nc = A_.num_cols();  // num cols in Cmap (All)
      dim_[0] = Nr;             // num rows in Rmap
      dim_[1] = Nc;             // mapping entire rows
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = Nc;             // num cols in matrix
    Cmap_.linspace(1,Nc, Nc);   // map "All" cols (start,stop, len)
  }


  //---------------------------------------------
  // Map subset of Columns in matrix
  //---------------------------------------------

  MappedRegion2D(Array2D &V, const MatDimension&, const IMap &J)
    : A_(V), Cmap_(J)
  {
       int Nr = A_.num_rows();  // num rows in Rmap (All)
      dim_[0] = Nr;             // mapping entire columns
      dim_[1] = J.size();       // num cols in Cmap 
    A_dim_[0] = Nr;             // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
    Rmap_.linspace(1,Nr, Nr);   // map "All" rows (start,stop, len)
  }

  MappedRegion2D(Array2D &V, const MatDimension&, const Index1D &JDX)
    : A_(V)
  {
    // Build Cmap (selected columns):
    int j1=JDX.lo(), j2=JDX.hi(), Nc=JDX.N();
    Cmap_.linspace(j1,j2, Nc);  // map cols [j1:j2]

       int Nr = A_.num_rows();  // num rows in Rmap (All)
      dim_[0] = Nr;             // mapping entire columns
      dim_[1] = Nc;             // num cols in Cmap 
    A_dim_[0] = Nr;             // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
    Rmap_.linspace(1,Nr, Nr);   // map "All" rows (start,stop, len)
  }


  //---------------------------------------------
  // Generalized 2D mapping : (Map, Map)
  //---------------------------------------------

  MappedRegion2D(Array2D &V, const IMap &I, const IMap &J)
    : A_(V), Rmap_(I), Cmap_(J)
  {
      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = J.size();       // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }

  // Generalized 2D mapping : (Map, Range)
  MappedRegion2D(Array2D &V, const IMap &I, const Index1D &JDX)
    : A_(V), Rmap_(I)
  {
    // Build Cmap (selected columns):
    int j1=JDX.lo(), j2=JDX.hi(), Nc=JDX.N();
    Cmap_.linspace(j1,j2, Nc);  // map cols [j1:j2]

      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = Nc;             // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }


  // Generalized 2D mapping : (Range, Map)
  MappedRegion2D(Array2D &V,  const Index1D &IDX, const IMap &J)
    : A_(V), Cmap_(J)
  {
    // Build Rmap (selected rows):
    int i1=IDX.lo(), i2=IDX.hi(), Nr=IDX.N();
    Rmap_.linspace(i1,i2, Nr);  // map rows [i1:i2]

      dim_[0] = Nr;             // num rows in Rmap
      dim_[1] = J.size();       // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }


  MappedRegion2D(MappedRegion2D<Array2D> &V, const IMap &I, const MatDimension&)
    : A_(V.A_), Rmap_(I)
  {
    umWARNING("Nigel: debugging MappedRegion2D", "Mapped region passed to mapped region");

       int Nc = A_.num_cols();  // num cols in Cmap (All)
      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = A_.num_cols();  // mapping entire rows
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = Nc;             // num cols in matrix
    Cmap_.linspace(1,Nc, Nc);   // map "All" cols (start,stop, len)
  }

  MappedRegion2D(MappedRegion2D<Array2D> &V, const IMap &I, const IMap &J)
    : A_(V.A_), Rmap_(I), Cmap_(J)
  {
    umWARNING("Nigel: debugging MappedRegion2D", "Mapped region passed to mapped region");

      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = J.size();       // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }


  MappedRegion2D<Array2D> operator()(const IMap &I, const MatDimension& MD)
  {
    return MappedRegion2D<Array2D>( A_, I, MD );
  }

  MappedRegion2D<Array2D> operator()(const IMap &I, const IMap &J)
  {
    return MappedRegion2D<Array2D>( A_, I, J );
  }


  T & operator()(int i, int j)
  {
#if (CHECK_ARRAY_INDEX)
    assert( Rmap_(i) <= A_dim_[0] );  // check row index in range
    assert( Cmap_(j) <= A_dim_[1] );  // check col index in range
#endif

    return A_(Rmap_(i), Cmap_(j));
  }


  const T & operator() (int i, int j) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( Rmap_(i) <= A_dim_[0] );  // check row index in range
    assert( Cmap_(j) <= A_dim_[1] );  // check col index in range
#endif

    return A_(Rmap_(i), Cmap_(j));
  }


  MappedRegion2D<Array2D> & operator=(const MappedRegion2D<Array2D> &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) = R(i,j);  // A_(Rmap_(i), Cmap_(j)) = R(i,j)

    return *this;
  }


  MappedRegion2D<Array2D> & operator=(const const_MappedRegion2D<Array2D> &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) = R(i,j);  // A_(Rmap_(i), Cmap_(j)) = R(i,j)

    return *this;
  }


  MappedRegion2D<Array2D> & operator=(const Array2D &R)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    assert(M == R.num_rows());  // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) = R(i,j);  // A_(Rmap_(i), Cmap_(j)) = R(i,j)

    // if R is temporary, delete it.
    if (R.get_mode() == OBJ_temp)
      delete (&R);

    return *this;
  }


  MappedRegion2D<Array2D> & operator=(const T* data)
  {
    // load a (single) mapped column or row with raw data 
    // (Note: fudge to allow loading from IVec/DVec)

    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols

    if ((1==M) && (N>1)) {
      // load the mapped row (assumes data has correct length)
      for (int j=1; j<=N; ++j) {
        (*this)(1,j) = data[j-1];
      }
    }
    else if ((M>1) && (1==N)) {
      // load the mapped column (assumes data has correct length)
      for (int i=1; i<=M; ++i) {
        (*this)(i,1) = data[i-1];
      }
    }
    else if ((1==M) && (1==N)) {
      // degenerate case: a single element
      (*this)(1,1) = data[0];
    }
    else {
      umERROR("MappedRegion2D operator=(T*)", 
              "Map should contain a single column or row");
    }

    return *this;
  }


  //-------------------------------------
  // Apply scalar to each mapped element
  //-------------------------------------

  MappedRegion2D<Array2D> & operator=(const T t)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) = t;       // A_(Rmap_(i), Cmap_(j)) = t

    return *this;
  }

  MappedRegion2D<Array2D>& operator += (const T& t)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) += t;      // A(Rmap_(i), Cmap_(j)) += t

    return *this;
  }

  MappedRegion2D<Array2D>& operator -= (const T& t)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) -= t;      // A_(Rmap_(i), Cmap_(j)) -= t

    return (*this);
  }

  MappedRegion2D<Array2D> & operator *= (const T& t)
  {
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) *= t;      // A_(Rmap_(i), Cmap_(j)) *= t

    return *this;
  }

  MappedRegion2D<Array2D>& operator /= (const T& t)
  {
    assert (T(0) != t);
    int M = this->num_rows();   // num. mapped rows
    int N = this->num_cols();   // num. mapped cols
    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) /= t;      // A_(Rmap_(i), Cmap_(j)) /= t

    return (*this);
  }


  //-------------------------------------
  // arithmetic operators += , -=
  //-------------------------------------

  MappedRegion2D<Array2D>& operator += (const Array2D& B)
  {
    int M = this->num_rows();     // num. mapped rows
    int N = this->num_cols();     // num. mapped cols
    assert(M == B.num_rows());    // assume both sides conform
    assert(N == B.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) += B(i,j);   // A_(Rmap_(i), Cmap_(j)) += B(i,j)

    if (B.get_mode() == OBJ_temp) {delete (&B);} // clean up temps
    return (*this);
  }


  MappedRegion2D<Array2D>& operator -= (const Array2D& B)
  {
    int M = this->num_rows();     // num. mapped rows
    int N = this->num_cols();     // num. mapped cols
    assert(M == B.num_rows());    // assume both sides conform
    assert(N == B.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) -= B(i,j);   // A_(Rmap_(i), Cmap_(j)) -= B(i,j)

    if (B.get_mode() == OBJ_temp) {delete (&B);} // clean up temps
    return (*this);
  }

  MappedRegion2D<Array2D>& operator += (const MappedRegion2D<Array2D> &R)
  {
    int M = this->num_rows();     // num. mapped rows
    int N = this->num_cols();     // num. mapped cols
    assert(M == R.num_rows());    // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) += R(i,j);   // A_(Rmap_(i), Cmap_(j)) += R(i,j)

    return (*this);
  }

  MappedRegion2D<Array2D>& operator -= (const MappedRegion2D<Array2D> &R)
  {
    int M = this->num_rows();     // num. mapped rows
    int N = this->num_cols();     // num. mapped cols
    assert(M == R.num_rows());    // assume both sides conform
    assert(N == R.num_cols());

    for (int j=1; j<=N; ++j)
      for (int i=1; i<=M; ++i)
        (*this)(i,j) -= R(i,j);   // A_(Rmap_(i), Cmap_(j)) -= R(i,j)

    return (*this);
  }

};



//---------------------------------------------------------
template <class Array2D> class const_MappedRegion2D
//---------------------------------------------------------
{
protected:

  const Array2D &   A_;     // the mapped matrix
        IMap Rmap_;         // id's of mapped "Rows"
        IMap Cmap_;         // id's of mapped "Cols"

  int   dim_[2];            // num entries in {R,C} maps
  int A_dim_[2];            // num entries in matrix

public:

  // data type of referenced array
  typedef typename Array2D::m_data_type   T;

  typedef         T   value_type;
  typedef         T   element_type;
  typedef         T*  pointer;
  typedef         T*  iterator;
  typedef         T&  reference;
  typedef const   T*  const_iterator;
  typedef const   T&  const_reference;

  const Array2D & array()  const { return A_; }

  int lbound()   const  { return A_.lbound(); }
  int size() const      { return A_.size(); }
  int num_rows() const  { return dim_[0]; }
  int num_cols() const  { return dim_[1]; }
  int dim (int i) const { return (1==i)?dim_[0]:dim_[1]; }
  const char* name() const{ return A_.name(); }


  // Map subset of Rows in matrix
  const_MappedRegion2D(const Array2D &V, const IMap &I, const MatDimension&) 
    : A_(V), Rmap_(I)
  {
       int Nc = A_.num_cols();  // num cols in Cmap (All)
      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = Nc;             // mapping entire rows
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = Nc;             // num cols in matrix
    Cmap_.linspace(1,Nc, Nc);   // map "All" cols (start,stop, len)
  }
  const_MappedRegion2D(const Array2D &V, const Index1D &IDX, const MatDimension&)
    : A_(V)
  {
    // Build Rmap (selected rows):
    int i1=IDX.lo(), i2=IDX.hi(), Nr=IDX.N();
    Rmap_.linspace(i1,i2, Nr);  // map rows [i1:i2]

       int Nc = A_.num_cols();  // num cols in Cmap (All)
      dim_[0] = Nr;             // num rows in Rmap
      dim_[1] = Nc;             // mapping entire rows
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = Nc;             // num cols in matrix
    Cmap_.linspace(1,Nc, Nc);   // map "All" cols (start,stop, len)
  }


  // Map subset of Columns in matrix
  const_MappedRegion2D(const Array2D &V, const MatDimension&, const IMap &J)
    : A_(V), Cmap_(J)
  {
       int Nr = A_.num_rows();  // num rows in Rmap (All)
      dim_[0] = Nr;             // mapping entire columns
      dim_[1] = J.size();       // num cols in Cmap 
    A_dim_[0] = Nr;             // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
    Rmap_.linspace(1,Nr, Nr);   // map "All" rows (start,stop, len)
  }
  const_MappedRegion2D(const Array2D &V, const MatDimension&, const Index1D &JDX)
    : A_(V)
  {
    // Build Cmap (selected columns):
    int j1=JDX.lo(), j2=JDX.hi(), Nc=JDX.N();
    Cmap_.linspace(j1,j2, Nc);  // map cols [j1:j2]

       int Nr = A_.num_rows();  // num rows in Rmap (All)
      dim_[0] = Nr;             // mapping entire columns
      dim_[1] = Nc;             // num cols in Cmap 
    A_dim_[0] = Nr;             // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
    Rmap_.linspace(1,Nr, Nr);   // map "All" rows (start,stop, len)
  }


  // Generalized 2D mapping : (Map, Map)
  const_MappedRegion2D(const Array2D &V, const IMap &I, const IMap &J)
    : A_(V), Rmap_(I), Cmap_(J)
  {
      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = J.size();       // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }

  // Generalized 2D mapping : (Map, Range)
  const_MappedRegion2D(const Array2D &V, const IMap &I, const Index1D &JDX)
    : A_(V), Rmap_(I)
  {
    // Build Cmap (selected columns):
    int j1=JDX.lo(), j2=JDX.hi(), Nc=JDX.N();
    Cmap_.linspace(j1,j2, Nc);  // map cols [j1:j2]

      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = Nc;             // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }

  // Generalized 2D mapping : (Range, Map)
  const_MappedRegion2D(const Array2D &V,  const Index1D &IDX, const IMap &J)
    : A_(V), Cmap_(J)
  {
    // Build Rmap (selected rows):
    int i1=IDX.lo(), i2=IDX.hi(), Nr=IDX.N();
    Rmap_.linspace(i1,i2, Nr);  // map rows [i1:i2]

      dim_[0] = Nr;             // num rows in Rmap
      dim_[1] = J.size();       // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }



  const_MappedRegion2D(const const_MappedRegion2D<Array2D> &V, const IMap& I, const MatDimension&) 
    : A_(V.A_), Rmap_(I)
  {
    umWARNING("Nigel: debugging const_MappedRegion2D", "Mapped region passed to mapped region");

       int Nc = A_.num_cols();  // num cols in Cmap (All)
      dim_[0] = I.size();       // num rows in map
      dim_[1] = Nc;             // mapping entire rows
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = Nc;             // num cols in matrix
    Cmap_.linspace(1,Nc, Nc);   // map "All" cols (start,stop, len)
  }
  const_MappedRegion2D(const const_MappedRegion2D<Array2D> &V, const IMap &I, const IMap &J)
    : A_(V.A_), Rmap_(I), Cmap_(J)
  {
    umWARNING("Nigel: debugging MappedRegion2D", "Mapped region passed to mapped region");

      dim_[0] = I.size();       // num rows in Rmap
      dim_[1] = J.size();       // num rows in Cmap
    A_dim_[0] = A_.num_rows();  // num rows in matrix
    A_dim_[1] = A_.num_cols();  // num cols in matrix
  }


  const_MappedRegion2D<Array2D> operator()(const IMap &I, const MatDimension& MD) const
  {
    return const_MappedRegion2D<Array2D>( A_, I, MD );
  }
  const_MappedRegion2D<Array2D> operator()(const IMap &I, const IMap &J) const
  {
    return const_MappedRegion2D<Array2D>( A_, I, J );
  }


  const T & operator() (int i, int j) const
  {
#if (CHECK_ARRAY_INDEX)
    assert( Rmap_(i) <= A_dim_[0] );  // check row index in range
    assert( Cmap_(j) <= A_dim_[1] );  // check col index in range
#endif

    return A_(Rmap_(i), Cmap_(j));
  }

};



//---------------------------------------------------------
// I/O : non-const MappedRegion2D
//---------------------------------------------------------
template <class Array2D>
std::ostream& operator<<(std::ostream &s, MappedRegion2D<Array2D> &R)
{
  int M = R.num_rows();   // num mapped rows
  int N = R.num_cols();   // FIXME: entire rows only

  s << M << "  " << N << "\n";

  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) {
      s << R(i,j) << " ";
    }
    s << "\n";
  }
  s.flush();
  return s;
}


//---------------------------------------------------------
// I/O : const MappedRegion2D
//---------------------------------------------------------
template <class Array2D>
std::ostream& operator<<(std::ostream &s, const_MappedRegion2D<Array2D> &R)
{
  int M = R.num_rows();   // num mapped rows
  int N = R.num_cols();   // FIXME: entire rows only

  s << M << "  " << N << "\n";

  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) {
      s << R(i,j) << " ";
    }
    s << "\n";
  }
  s.flush();
  return s;
}

#endif  // NDG__MappedRegion_22D_H__INCLUDED
