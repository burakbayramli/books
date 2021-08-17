// ArrayGen.h
//
// 2007/02/14
//---------------------------------------------------------
#ifndef NDG__ArrayGen_Type_H__INCLUDED
#define NDG__ArrayGen_Type_H__INCLUDED

#include "Mat_COL.h"


// forward declarations:
template <typename T> class ArrayGen;

typedef ArrayGen<double>  DArray;

template <typename T> std::ostream& operator << (std::ostream& os, const ArrayGen<T>& x);
template <typename T> std::istream& operator >> (std::istream& is,       ArrayGen<T>& x);


//---------------------------------------------------------
template <typename T>
class ArrayGen : public virtual Vector<T>
//---------------------------------------------------------
{
  // A multi-dimensional array implemented as a simple Vector<T>. 
  // Multi-dimensional indexing is enabled by subscript operators 
  // for 1, 2, 3 and LVec indices.  The base of each index must 
  // be set manually (default base is 1).  If there are more than 
  // 3 dimensions, a "LVec" argument needs to be used. Note that 
  // for arrays of dimension > 1, the first index varies fastest,
  // i.e. storage is fortran-compatible column-major. To interface 
  // with fortran, pass the pointer returned from getPtr0().

  //
  // Member data
  //
protected:

  long  ndim;   // number of dimensions
  LVec  nm;     // length of each dimension, multiple index
  LVec  bm;     // base of each dimension, multiple index
  long  nm1;    // length of 1D
  long  nm1nm2; // length 1D multiplied with length 2D
  long  bm0;    // helper for subscript operator()

  mutable long local_index; // relative subscripting parameter
 
  long  current_iterator_index; // for iterating over the array entries
  long  totalLength(const LVec& n);

  void  init(long n1);
  void  init(long n1, long n2);
  void  init(long n1, long n2, long n3);
  void  init(const LVec& n);

  bool compatible(const Vector<T>& X) const 
  { 
    // compatible if total length the same.
    return this->size() == X.size(); 
  }

  bool compatible(const ArrayGen<T>& a)
  {
    // compatible if total length the same.
    // return this->size() == X.size(); 

    bool b = true;
    if (this->m_Len != a.m_Len) b = false;
    if (nm != a.nm) b = false;
    if (bm != a.bm) b = false;
    return b;
  }

  // Used in subscript functions having a LVec argument.
  long elmNo(const LVec& index, long base=0) const
  {
    long i,r,n;
    for (i=1, r=base, n=1; i <= ndim; n *= nm(i), ++i)
    { 
      r += n*index(i); 
    }
    return r;
  }


public:

  //
  // constructors
  //
  ArrayGen()                          : Vector<T>()               {}
  ArrayGen(long n1)                   : Vector<T>(n1)             { init(n1); }
  ArrayGen(long n1, long n2)          : Vector<T>(n1*n2)          { init(n1, n2); }
  ArrayGen(long n1, long n2, long n3) : Vector<T>(n1*n2*n3)       { init(n1, n2, n3); }
  ArrayGen(const LVec& n)             : Vector<T>(totalLength(n)) { init(n); }
  ArrayGen(const ArrayGen<T>& X)      : Vector<T>(X.size())       { init(X.nm); setBase(X.bm); }

  ArrayGen(long n1, long n2, long n3, long n4) 
    : Vector<T>(n1*n2*n3*n4)
  {
    LVec idx(4); 
    idx(1)=n1; idx(2)=n2; 
    idx(3)=n3; idx(4)=n4; 
    init(idx);
  }

  //
  // destructor
  //
  virtual ~ArrayGen () {}


  //
  // allocation
  //
  // 1/2/3-dim. arrays, or multiple index
  bool resize(long n1)                   { init(n1);       return Vector<T>::resize(n1, 0.0); }
  bool resize(long n1, long n2)          { init(n1,n2);    return Vector<T>::resize(n1*n2, 0.0); }
  bool resize(long n1, long n2, long n3) { init(n1,n2,n3); return Vector<T>::resize(n1*n2*n3, 0.0); }
  bool resize(const LVec& lvec)        { init(lvec);     return Vector<T>::resize(totalLength(lvec),0.0); }
  bool resize(T* a, long n, long base=0) 
  {
     Vector<T>::resize(n, 0.0); 
     copy(a); 
     return *this; 
  }

  // setBase() - choose arbitrary base for the index of each dimension.
  void setBase(long b1)                   { assert(ndim >= 1);  bm(1)=b1;                     bm0 = 1 - b1; }
  void setBase(long b1, long b2)          { assert(ndim >= 2);  bm(1)=b1; bm(2)=b2;           bm0 = 1 - nm1*b2 - b1; }
  void setBase(long b1, long b2, long b3) { assert(ndim >= 3);  bm(1)=b1; bm(2)=b2; bm(3)=b3; bm0 = 1 - nm1nm2*b3 - nm1*b2 - b1; }
  void setBase(const LVec& b)
  {
    assert(ndim == b.size()); 
    bm.resize (ndim, 0); 
    bm = b;
    // set bm0 = 1 - b(1) - nm(1)*b(2) - nm(1)*nm(2)*b(3) ...
    bm0 = 1-bm(1);
    long n = 1;
    for (long i = 1; i < ndim; ++i) {
      n *= nm(i);
      bm0 -= n*bm(i+1);
    }
  }

  // getBase() and getMaxI() allow one to determine the 
  // lower and upper bounds on loops involving ArrayGen 
  // objects. getDim() finds the length of the loops.

  // lower index:
  void getBase(long& b1) const                     { assert(ndim >= 1); b1 = bm(1); }
  void getBase(long& b1, long& b2) const           { assert(ndim >= 2); b1 = bm(1); b2 = bm(2); }
  void getBase(long& b1, long& b2, long& b3) const { assert(ndim >= 3); b1 = bm(1); b2 = bm(2); b3 = bm(3); }
  void getBase(LVec& b) const                      { b.resize(ndim, 0);  b = bm; }

  // upper index:
  void getMaxI(long& n1) const                     { assert(ndim >= 1); n1 = nm(1) + bm(1) - 1; }
  void getMaxI(long& n1, long& n2) const           { assert(ndim >= 2); n1=nm(1)+bm(1) - 1; n2=nm(2)+bm(2)-1; }
  void getMaxI(long& n1, long& n2, long& n3) const { assert(ndim >= 3); n1=nm(1)+bm(1)-1;  n2=nm(2)+bm(2)-1; n3=nm(3)+bm(3)-1; }
  void getMaxI(LVec& n) const                      { assert(ndim >= n.size()); n.resize(ndim, 0); for (long i = 1; i <= ndim; i++) n(i) = nm(i) + bm(i) - 1; }

  long  getNoIndices() const { return ndim; }

  // length of each dimension:
  void getDim(long& n1) const                     { assert(ndim >= 1); n1 = nm1; }
  void getDim(long& n1, long& n2) const           { assert(ndim >= 2); n1 = nm(1); n2 = nm(2); }
  void getDim(long& n1, long& n2, long& n3) const { assert(ndim >= 3); n1 = nm(1); n2 = nm(2); n3 = nm(3); }
  void getDim(LVec& n) const                      { assert(ndim >= n.size()); n.resize(ndim,0); n = nm; }

  // for singleIndex1
#ifndef NDEBUG
  void indexOk1(long i) const                 { assert(i >= 1 && i <= this->m_Len); }
  void indexOk (long i) const                 { assert( ndim == 1 ); assert( i>=bm(1) && i<=bm(1)+nm(1)-1); }
  void indexOk (long i, long j) const         { assert( ndim == 2 ); assert( i >= bm(1) && i <= bm(1)+nm(1)-1); assert( j >= bm(2) && j <= bm(2)+nm(2)-1); }
  void indexOk (long i, long j, long k) const { assert( ndim == 3 ); assert( i >= bm(1) && i <= bm(1)+nm(1)-1); assert( j >= bm(2) && j <= bm(2)+nm(2)-1); assert( k >= bm(3) && k <= bm(3)+nm(3)-1); }
  void indexOk (const LVec& index) const      { assert( ndim == index.size()); for (long k=1; k<=index.size(); ++k) assert( index(k) >= bm(k) && index(k) <= bm(k)+nm(k)-1); }
#else
  void indexOk1(long i) const                 {}
  void indexOk (long i) const                 {}
  void indexOk (long i, long j) const         {}
  void indexOk (long i, long j, long k) const {}
  void indexOk (const LVec& index) const      {}
#endif


  std::string arraySize() const { return std::string("NOT_IMPLEMENTED...[0,0]x[0,0]"); }

  // singleIndex1() enables indexing the n-dimensional array with 
  // a single index. Avoids separate loops over each dimension.
        T& singleIndex1(long i)       { indexOk1(i); return this->vm1_[i]; }
  const T& singleIndex1(long i) const { indexOk1(i); return this->vm1_[i]; }

  //---------------------------------------------
  // return the single index element number:
  //   i returns                                           (i-bm(1) + 1);
  //  ij returns                         ((j-bm(2))*nm(1) + i-bm(1) + 1);
  // ijk returns ((k-bm(3))*nm(1)*nm(2) + (j-bm(2))*nm(1) + i-bm(1) + 1);
  //---------------------------------------------
  long multiple2single(long i) const                 { return i + bm0; }                    // (i-bm(1)+1);
  long multiple2single(long i, long j) const         { return j*nm1 + i + bm0; }            // (j-bm(2))*nm(1) + i-bm(1)+1;
  long multiple2single(long i, long j, long k) const { return k*nm1nm2 + j*nm1 + i + bm0; } // (k-bm(3))*nm(1)*nm(2) + (j-bm(2))*nm(1) + i-bm(1)+1;
  long multiple2single(const  LVec& index) const     { return elmNo(index,bm0); }

  long length1D() const    { return nm1;   }
  long length1Dx2D() const { return nm1nm2;}

  // set a local reference index :
  void setLocalIndex(long i) const                 { indexOk(i);     local_index = multiple2single(i); }
  void setLocalIndex(long i, long j) const         { indexOk(i,j);   local_index = multiple2single(i,j); }
  void setLocalIndex(long i, long j, long k) const { indexOk(i,j,k); local_index = multiple2single(i,j,k); }
  void setLocalIndex(const LVec& index) const      { indexOk(index); local_index = multiple2single(index); }

  // getLocalIndex() returns the single index, 
  // corresponding to element number in base Vector.
  long getLocalIndex() const { return local_index; }

  // subscript the neighbors of the local index :
        T& local(long i)                       { indexOk1(local_index+i); return this->vm1_[local_index+i]; }
  const T& local(long i) const                 { indexOk1(local_index+i); return this->vm1_[local_index+i]; }
        T& local(long i, long j)               { indexOk1(local_index + nm1*j + i); return this->vm1_[local_index + nm1*j + i]; }
  const T& local(long i, long j) const         { indexOk1(local_index + nm1*j + i); return this->vm1_[local_index + nm1*j + i]; }
        T& local(long i, long j, long k)       { indexOk1(local_index + nm1nm2*k + nm1*j + i); return this->vm1_[local_index + nm1nm2*k + nm1*j + i]; }
  const T& local(long i, long j, long k) const { indexOk1(local_index + nm1nm2*k + nm1*j + i); return this->vm1_[local_index + nm1nm2*k + nm1*j + i]; }
        T& local(const LVec& index)            { long r = elmNo(index,local_index); indexOk1(r); return this->vm1_[r]; }
  const T& local(const LVec& index) const      { long r = elmNo(index,local_index); indexOk1(r); return this->vm1_[r]; }

  //-------------------------------------
  // indexing operators :
  //-------------------------------------
        T& operator()(long i)                       { indexOk(i);     return this->vm1_[multiple2single(i)]; }
  const T& operator()(long i) const                 { indexOk(i);     return this->vm1_[multiple2single(i)]; }
        T& operator()(long i, long j)               { indexOk(i,j);   return this->vm1_[multiple2single(i,j)]; }
  const T& operator()(long i, long j) const         { indexOk(i,j);   return this->vm1_[multiple2single(i,j)]; }
        T& operator()(long i, long j, long k)       { indexOk(i,j,k); return this->vm1_[multiple2single(i,j,k)]; }
  const T& operator()(long i, long j, long k) const { indexOk(i,j,k); return this->vm1_[multiple2single(i,j,k)]; }
        T& operator()(const LVec& index)            { indexOk(index); return this->vm1_[multiple2single(index)]; }
  const T& operator()(const LVec& index) const      { indexOk(index); return this->vm1_[multiple2single(index)]; }

  T& operator()(long i, long j, long k, long l) 
  {
    LVec idx(4); 
    idx(1)=i; idx(2)=j;
    idx(3)=k; idx(4)=l;
    indexOk(idx); return this->vm1_[multiple2single(idx)]; 
  }
  const T& operator()(long i, long j, long k, long l) const 
  { 
    LVec idx(4); 
    idx(1)=i; idx(2)=j;
    idx(3)=k; idx(4)=l;
    indexOk(idx); return this->vm1_[multiple2single(idx)]; 
  }

  // startIterator() starts an iteration over all the array entries.
  void startIterator() { current_iterator_index = 0; }

  // nextEntry() moves a pointer to the next entry in the array. 
  // Returns true if there is a next entry, else false.
  bool nextEntry() 
  {
    current_iterator_index++;
    return  (current_iterator_index >= 1 && current_iterator_index <= this->m_Len) ? true : false;
  }

  // thisEntry() returns a reference to the current entry. 
  const T& thisEntry() const { 
    // read-only version
    assert(current_iterator_index >= 1 && current_iterator_index <= this->m_Len);
    return this->vm1_[current_iterator_index];
  }

  T& thisEntry () {
    // read-write version
    assert(current_iterator_index >= 1 && current_iterator_index <= this->m_Len);
    return this->vm1_[current_iterator_index];
  }


  ArrayGen<T>& operator=(T a)   { fill(a); return *this; }

  ArrayGen<T>& operator=(const ArrayGen<T>& a)
  {
    ndim = a.ndim;
    nm = a.nm;
    bm = a.bm;
    nm1 = a.nm1;
    nm1nm2 = a.nm1nm2;
    bm0 = a.bm0;

    Vector<T>::operator=(a);
    return *this;
  }

  void fill(const T& a)         { Vector<T>::operator=(a); }
  void fill(const Vector<T>& X) { Vector<T>::operator=(X); }
  void fill(T start, T stop)    { Vector<T>::fill(start,stop); }

  void set_slice(int k, const Mat_COL<T>& Q)
  {
    umMSG(1, "DArray: setting array slice %d\n", k);
  }

  //#######################################################
  void get_slice(int k, Mat_COL<T>& Q) const
  //#######################################################
  {
    assert(3==ndim);    // only deal with slices of 3d arrays
    assert(k<=nm(3));   // nm(3) is number of slices in array

    umMSG(1, "DArray: returning array slice %d\n", k);

    int M=nm(1), N=nm(2);
    int idx = (k-1) * (M*N);  // idx is offset to kth slice
    Q.load(M,N, this->v_[idx]);
  }


  //#######################################################
  Mat_COL<T>& get_slice(int k) const
  //#######################################################
  {
    assert(3==ndim);    // only deal with slices of 3d arrays
    assert(k<=nm(3));   // nm(3) is number of slices in array

    umMSG(1, "DArray: returning array slice %d\n", k);
    std::string sz = umOFORM("%s(slice:%d)", this->name(), k);

    int M=nm(1), N=nm(2);
    int idx = (k-1) * (M*N);  // idx is offset to kth slice

    Mat_COL<T> *tmp=new Mat_COL<T>(M,N, this->v_[idx], OBJ_temp, sz.c_str());
    return (*tmp);
  }


  void print (
    std::ostream& os, 
    const char* header = NULL, 
    long nentries_per_line = 3,
                      // dec places : [ 4] ios::fixed
    long  prec = 4,   // sig figures: [ 4] ios::scientific
    long  wdth = 12   // spacing      [12]
  ) const;


  friend std::ostream& operator << <> (std::ostream& os, const ArrayGen<T>& x);
  friend std::istream& operator >> <> (std::istream& is, ArrayGen<T>& x);
};


void printAsIndex(std::ostream& os, const LVec& X);
long sv_single2multiple(long single, long loop_length, long prod_loop_length);
void single2multiple(long single, LVec& multiple, const LVec& loop_length, const LVec& prod_loop_length, const LVec& loop_base);


// Initialization
template <typename T> inline
long ArrayGen<T>::totalLength (const LVec& n)
{
  const long nsd = n.size();
  long ntot = 1;
  for (long i = 1; i <= nsd; i++)
    ntot *= n(i);

  return ntot;
}


template <typename T> inline
void ArrayGen<T>::init(long n1)
{
  ndim = 1;
  nm.resize(ndim);
  bm.resize(ndim);
  bm = 1;       // fill(1); // base = 1
  bm0  = 0;
  nm(1) = n1;
  nm1 = n1;
  nm1nm2 = 0;
  local_index = bm0;
}


template <typename T> inline
void ArrayGen<T>::init(long n1, long n2)
{
  ndim = 2;
  nm.resize(ndim);
  bm.resize(ndim);
  bm = 1;       // fill(1); // base = 1
  bm0 = -n1;
  nm(1) = nm1 = n1;
  nm(2) = n2;
  nm1nm2= n1*n2;
  local_index = bm0;
}


template <typename T> inline
void ArrayGen<T>::init(long n1, long n2, long n3)
{
  ndim = 3;
  nm.resize(ndim);
  bm.resize(ndim);
  bm = 1;       // fill(1); // base = 1
  bm0 = -n1*n2 - n1;

  nm(1) = nm1 = n1;
  nm(2) = n2;
  nm(3) = n3;
  nm1nm2= n1*n2;
  local_index = bm0;
}


template <typename T> inline
void ArrayGen<T>::init(const LVec& n)
{
  ndim = n.size();
  nm.resize(ndim, 0);
  bm.resize(ndim, 0);
  bm  = 1;    // fill(1); // base = 1
  nm  = n;
  nm1 = nm(1);

  if (ndim >= 2)  nm1nm2 = nm(1)*nm(2);
  else nm1nm2=0;

  bm0 = 1-bm(1);
  long nn = 1;

  for (long i = 1; i < ndim; ++i)
  {
    nn*=nm(i);
    bm0 -= nn*bm(i+1);
  }
  local_index = bm0;
}


template <typename T>
std::ostream& operator << (std::ostream& os, const ArrayGen<T>& x) 
{
  x.print(os);
  return os;
}


template <typename T>
std::istream& operator >> (std::istream& is, ArrayGen<T>& x) 
{
  umWARNING("operator >> (std::istream&, ArrayGen<T>&)", "Not implemented yet.");
  return is;
}


template <typename T>
void ArrayGen<T>::print
(
  std::ostream& os, 
  const char* header,       //  [NULL]
  long nentries_per_line,   //  [ 3] 
                // dec places : [ 4] ios::fixed
  long  prec,   // sig figures: [ 4] ios::scientific
  long  wdth    // spacing      [12]
) const
{
  std::string header_str = header != NULL ? header : "";
  bool text = (header != NULL);
  long i, j, k;

  if (this->size()<1) 
    return;

  if (text) 
    os << "\n[" << this->m_Len << "]  " << header_str.c_str() << " $\n";

  //-------------------------------------
  // Find max index value (not used)
  //-------------------------------------
  long maxi = -10000;
  bool negative = false;
  for (i = 1; i <= ndim; i++) {
    if (maxi < abs(bm(i)+nm(i)-1)) {
      maxi = abs(bm(i)+nm(i)-1);
      if (bm(i)+nm(i)-1 < 0)
        negative = true;
    }
  }

  LVec idx  (ndim);
  LVec prod (ndim);
  for (j = 1; j <= ndim; j++)
  {
    prod(j) = 1;
    for (k = 1; k <= j-1; k++)
      prod(j) *= nm(k);
  }

  os << std::setiosflags(std::ios::fixed);
  os << std::setprecision(prec);

  for (i = 1; i <= this->m_Len; i++) 
  {
    single2multiple (i, idx, nm, prod, bm);
    if (text) {
      // write index
      printAsIndex(os, idx);
      os << '=';
    }
    os << std::setw(wdth) << operator()(idx) << " ";
    if ((i % nentries_per_line) == 0)
      os << '\n';
  }
  if (bm.size() != 1)
    os << '\n';
  os.flush();
}


#endif  // NDG__ArrayGen_Type_H__INCLUDED
