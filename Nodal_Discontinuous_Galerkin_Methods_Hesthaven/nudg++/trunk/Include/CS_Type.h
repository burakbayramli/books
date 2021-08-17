// CS_Type.h
// low-level routines on CS structures
// 2007/10/11
//---------------------------------------------------------
#ifndef NDG__CS_Type_H__INCLUDED
#define NDG__CS_Type_H__INCLUDED

#include "Mat_COL.h"
#include "LOG_funcs.h"
#include "CS_Utils.h"

// TODO: regions of sparse matrices
#include "CS_SpReg2D.h"


// typedef versions for certain data types
template <typename T> class CS;

// forward
class CSS;

typedef CS<double>  CSd;
typedef CS<dcmplx>  CSz;
typedef CS<int>     CSi;

// define pointers to "keep" functions
typedef int (*KeepFunc)(int, int, double, void *);

#define CS_READY_4_THIS    0
#define USE_SPARSE_THREADS 0
//#########################################################
// FIXME: See SSMULT re sorting row indices
// http://www.cise.ufl.edu/research/sparse/ssmult/SSMULT/
#define CS_ROWS_ARE_SORTED 1   // FIXME: row indices not sorted?
//#########################################################


// forward
template <typename T> inline CS<T>& trans (const CS<T> &A, int values=1);
template <typename T> inline CS<T>& trans2(const CS<T> &A, int values=1);
template <typename T> inline CS<T>& perm(const CS<T> &A, const IVec& pinv, const IVec& q, int values);


//---------------------------------------------------------
template <typename T> class CS
//---------------------------------------------------------
{
public:

  int   nzmax;  // maximum number of entries
  int   m;      // number of rows
  int   n;      // number of columns
  IVec  P;      // column pointers (size n+1) or col indices (size nzmax)
  IVec  I;      // row indices, size nzmax
  
  Vector<T> X;  // numerical values, size nzmax

  int   nz;     // if (triplet) nz stores num entries, 
                // if (is_csc ) nz must be -1

  int   m_mode;         // {OBJ_real,OBJ_temp}
  int   m_values;       // data as well as structure?
  int   m_shape;        // lower/upper/triangular/symmetric?

  std::string m_name;   // string identifier
  static int sp_count;  // track number of objects

public:

  typedef T m_data_type;  // for SpReg2D< SpMat<T> >


  CS(const char* sz="CS", int mode = OBJ_real);
  CS(int Nr, int Nc, int nzmax, int values, int triplet, int mode=OBJ_real, const char* sz="CS");
  CS(const CS<T>& B);
  CS(const CS<T>& B, int mode, const char* sz, int values);
  virtual ~CS();

  bool  ok() const;
  void  reset();
  void  resize(int Nr, int Nc, int nzmax, int values, int triplet);
  bool  realloc(int max_nz, bool bCheck=true);
  CS<T>& own(const CS<T>& B, int values=1);

  int   num_rows() const    { return m; }
  int   num_cols() const    { return n; }
  int   lbound()  const     { return 1; } // base for operator(i,j)
  int   dim(int d) const    { return (d==1)?m:((d==2)?n:0); }
  int   size() const        { return is_csc() ? nzmax : nz; }
  int   nnz() const         { return is_csc() ? nzmax : nz; }
  int   get_sp_count() const { return sp_count; }

  const char* name() const  { return m_name.c_str(); }
  void  set_name(const char* sz) { m_name = sz; }
  int   get_mode() const    { return m_mode; }
  void  set_mode(int mode)  { m_mode = mode; }
  int   get_shape() const   { return m_shape; }
  void  set_shape(int flag) { m_shape = flag; }
  bool  is_square() const   { return (m == n); }
  bool  is_zero() const     { return is_csc() ? (P[n]<1) : (nz<1); }
  bool  is_triplet() const  { return (nz >= 0 ); }
  bool  is_csc() const      { return (-1 == nz); }
  bool  is_compatible(const CS<T> &B) const { return (B.num_rows()==m && B.num_cols()==n); }

  T       operator()(int i, int j) const;
  T&      operator()(int i, int j);

  CS<T>&  operator = (const CS<T>& B);
  void    copy(const CS<T>& B, int values=1);

  void add_val (const T& x);
  void mult_val(const T& x);
  void div_val (const T& x) { if (x != T(0)) { mult_val(T(1)/x); } 
                              else { umERROR("CS<T>::div_val", "division by zero"); }}

  CS<T>& operator += (const T& x) { add_val ( x); return (*this); }
  CS<T>& operator -= (const T& x) { add_val (-x); return (*this); }
  CS<T>& operator *= (const T& x) { mult_val( x); return (*this); }
  CS<T>& operator /= (const T& x) { div_val ( x); return (*this); }


  CS<T>& operator += (const CS<T>& B);
  CS<T>& operator -= (const CS<T>& B);
  CS<T>& operator *= (const CS<T>& B);

  // FIXME: implement solvers
  CS<T>& operator /= (const CS<T>& B) { return (*this); }
  CS<T>& operator |= (const CS<T>& B) { return (*this); }

  //-------------------------------------
  // cs_* routines
  //-------------------------------------

  int     val_idx(int i, int j) const;  // Note: !CS_ROWS_ARE_SORTED
  CS<T>&  compress(bool bSort=false);
  CS<T>&  compress(const CS<T>& B, bool bSort=false);
  double  cumsum(IVec& c, int Nc);
  cs_dp*  dmperm(int seed) const;
  void    dropdiag();
  int     dropsort(double tol=0.0);   // drop + sort
  int     droptol(double tol=0.0);    // drop small entries
  int     dupl();
  bool    entry(int i, int j, T x);   // add 0-based (i,j) to matrix
  bool    set1 (int i, int j, T x);   // add 1-based (i,j) to matrix
  int     ereach(int k, const IVec& parent, int *s, int *w);
  IVec&   etree(int ata) const;
  CS<T>&  identity(int Ni, bool pack=true);
  int     is_tri() const;
  IMat&   find2D(char op, T val) const;
  int     fkeep(KeepFunc fK, void *other);
  void    gaxpy(const Vector<T>& x, Vector<T>& y) const; // y += Ax
  void    gxapy(const Vector<T>& x, Vector<T>& y) const; // y += xA

  void    load(FILE *fp);
  void    load(int Nr, int Nc, IVec& ir, IVec& jc, Vector<T>& Ax, 
               int part=sp_All, bool forceSym=true, double tol=0.0, 
               bool free_args=false);  // release {ir,jc, Ax}?

  CS<T>&  make_tri_sym();
  bool    maxtrans(IVec& jimatch, int seed) const;
  double  nonsymmetry() const;
  T       norm() const;
  T       norm1() const { return norm(); }
  CS<T>&  permute(const IVec& pinv, const IVec& q, int values=1);
  CS<T>&  permuteP(const IVec& pinv, int values=1);
  CS<T>&  permuteQ(const IVec& q, int values=1);
  void    print(bool brief=true);
  void    scale(const CS<T>& B);
  int     scatter(int j, T beta, IVec& w, Vector<T>& x, int mark, CS<T>& C, int Nz) const;
  cs_dp*  SCC();
  void    spy(const char* msg) const;
  CS<T>&  symperm(const IVec& pinv, int values);
  CS<T>&  transpose(int values);
  void    write_ML(FILE* fp) const;

  //-------------------------------------
  // Debug utilities
  //-------------------------------------
#ifndef NDEBUG
  // 1-based indexing
  #define CS_BOUNDS_CHECK(i, iLO, iHI) \
  assert((int)(iLO) <= (int)(i  )); \
  assert((int)(i  ) <= (int)(iHI));
  //-------------------------------------
  #define CS_BOUNDS_CHECK_IJ(i, j) \
  CS_BOUNDS_CHECK(i, 1, (this->m)); \
  CS_BOUNDS_CHECK(j, 1, (this->n));
  //-------------------------------------
  #define CS_BOUNDS_CHECK_00(i, j) \
  CS_BOUNDS_CHECK(i, 0, (this->m-1)); \
  CS_BOUNDS_CHECK(j, 0, (this->n-1));
  //-------------------------------------
#else
  // no checking
  #define CS_BOUNDS_CHECK(i, iLO, iHI)
  #define CS_BOUNDS_CHECK_IJ(i, j)
  #define CS_BOUNDS_CHECK_00(i, j)
#endif

  //-------------------------------------------------------
  // operations involving (contiguous) Region1D
  //-------------------------------------------------------
public:
  SpReg2D< CS<T> >  operator()(const Index1D &I, const Index1D &J);
  SpReg2D< CS<T> >  operator()(const Index1D &I, int j);
//SpReg1D< CS<T> >  operator()(const Index1D &I, int j);
//SpReg1D< CS<T> >  operator()(const Index1D &I);
//SpReg1D< CS<T> >  operator()(const MatDimension&, int j);
//SpReg2D< CS<T> >  operator()(int i, const MatDimension&);

};


template <typename T> inline
SpReg2D< CS<T> >
CS<T>::operator()(const Index1D &I, const Index1D &J)
{
  // return a 2D region of (*this)
  return SpReg2D< CS<T> >(*this, I,J);
}

template <typename T> inline
SpReg2D< CS<T> >
CS<T>::operator()(const Index1D &I, int j)
{
  // return a 2D region of (*this)
  return SpReg2D< CS<T> >(*this, I,j);
}



///////////////////////////////////////////////////////////
//
// define static member data
//
///////////////////////////////////////////////////////////

// allow toggling of allocation tracing in debug mode
template <typename T> int CS<T>::sp_count=0;



///////////////////////////////////////////////////////////
//
// CS<T> member functions
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T>
CS<T>::CS(const char* sz, int mode)
//---------------------------------------------------------
  : nzmax(0), m(0), n(0), P("P"), I("I"), X("X"), nz(0),
    m_mode(mode), m_values(0), m_shape(sp_NONE), m_name(sz)
{
  ++sp_count;
  //umTRC(2, "+++ CS<T> ctor (1) +++ : name: %s, count: %d\n", name(), sp_count);
}


//---------------------------------------------------------
template <typename T>
CS<T>::CS(int Nr, int Nc, int nzmax, int values, int triplet, int mode, const char* sz)
//---------------------------------------------------------
  : nzmax(0), m(0), n(0), P("P"), I("I"), X("X"), nz(0),
    m_mode(mode), m_values(0), m_shape(sp_NONE), m_name(sz)
{
  ++sp_count;
  resize(Nr, Nc, nzmax, values, triplet);
  //umTRC(2, "+++ CS<T> ctor (2) +++ : name: %s, count: %d\n", name(), sp_count);
}


//---------------------------------------------------------
template <typename T>
CS<T>::CS(const CS<T>& B) //, int mode, const char* sz, int values)
//---------------------------------------------------------
  : nzmax(0), m(0), n(0), P("P"), I("I"), X("X"), nz(0),
    m_mode(OBJ_real), m_values(B.m_values), m_shape(B.m_shape), m_name("CS")
{
  ++sp_count;
  this->copy(B, B.m_values);
  //umTRC(2, "+++ CS<T> ctor (3) +++ : name: %s, count: %d\n", name(), sp_count);
}


//---------------------------------------------------------
template <typename T>
CS<T>::CS(const CS<T>& B, int mode, const char* sz, int values)
//---------------------------------------------------------
  : nzmax(0), m(0), n(0), P("P"), I("I"), X("X"), nz(0),
    m_mode(mode), m_values(values), m_shape(sp_NONE), m_name(sz)
{
  ++sp_count;
  this->copy(B, values);
  //umTRC(2, "+++ CS<T> ctor (4) +++ : name: %s, count: %d\n", name(), sp_count);
}


//---------------------------------------------------------
template <typename T>
CS<T>::~CS()
//---------------------------------------------------------
{
  --sp_count;
  this->reset();  // force full deallocation of arrays
  //umTRC(2, "    CS<T> dtor     --- : name: %s, count: %d\n", name(), sp_count);
}


//---------------------------------------------------------
template <typename T> inline
T CS<T>::operator()(int i, int j) const
//---------------------------------------------------------
{
  // 1-based public interface:
  // if entry (i,j) exists in sparsity pattern, 
  // return its value, else return 0.

  assert(is_csc());             // only implemented for compressed form
  CS_BOUNDS_CHECK_IJ(i,j);      // 1-based interface
  int idx = val_idx(i-1,j-1);   // 0-based storage
  if (idx < 0) {
    return (T)0;
  }
  assert(idx < P[n]);   // check index is valid
  return X[idx];
}

 
//---------------------------------------------------------
template <typename T> inline
T& CS<T>::operator()(int i, int j)
//---------------------------------------------------------
{
  static T tZ = 0;
  // if entry (i,j) exists in current sparsity pattern
  // return a reference to it.  However...

  // TODO: non-const version allows changes to both the 
  // element values and the sparsity structure 

  if ( is_csc() ) {
    umERROR("CS<T>::operator()(i,j)", "compressed form is read-only");
  } else if ( !is_csc() ) {
    umERROR("CS<T>::operator()(i,j)", "triplet form is read-only");
  }

  //#######################################################
  // FIXME: transitional operation!
  //#######################################################
  // Version 1:  allow duplicates; sum when compressing
  // add entry(i,j)
  // if entry (i,j) is not in the sparsity pattern of 
  // a csc matrix, it is very expensive to insert it.
  // for now, treat a missing entry as an error.
  //#######################################################

  assert(is_csc());             // only implemented for compressed form
  CS_BOUNDS_CHECK_IJ(i,j);      // 1-based interface
  int idx = val_idx(i-1,j-1);   // 0-based storage
  if (idx < 0) {
  //umERROR("CS<T>::operator()(%d,%d)", "FIXME: insertion to csc matrix", i,j);
    return tZ;
  }
  return X[idx];
}

//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::operator = (const CS<T>& B)
//---------------------------------------------------------
{
  if (this == &B) return (*this);

  // define dimensions, nzmax, (triplet or csc)
  m=B.m; n=B.n; nzmax=B.nzmax; nz=B.nz; 
  m_values=B.m_values;
  if (m_name == "CS") { m_name = B.m_name; }

  if (OBJ_temp != B.get_mode()) {
    // copy internal arrays
    P = B.P; I = B.I; X = B.X;
  } else {
    // take ownership of B's internal arrays
    CS<T>& Bref = const_cast< CS<T>& >(B);
    // transfer {P,I,X}.  Note: since these arrays may be 
    // huge, call own_2() to force immediate deallocation
    P.own_2(Bref.P); I.own_2(Bref.I); X.own_2(Bref.X);
    delete (&B);
  }

  assert(ok());
  return (*this); 
}

//---------------------------------------------------------
template <typename T> inline
void CS<T>::copy(const CS<T>& B, int values)
//---------------------------------------------------------
{
  if (this == &B) return;

  // define dimensions, nzmax, (triplet or csc)
  m=B.m; n=B.n; nzmax=B.nzmax; nz=B.nz; 
  if (m_name == "CS") { m_name = B.m_name; }
  m_shape = B.m_shape;  // shapes will match

  if (B.X.size() != B.I.size()) values = 0;
  m_values = values;    // copy numeric data?

  if (OBJ_temp != B.get_mode()) 
  {
    // copy internal arrays
    P = B.P; I = B.I;     // deep copy {P,I}
    if (values) { 
      X = B.X;  // deep copy {X} ?
    } else { X.Free(); }

  } else {
    // take ownership of B's internal arrays
    CS<T>& Bref = const_cast< CS<T>& >(B);

    // Note: since these arrays may be huge, we call 
    // own_2() to force immediate deallocation
    P.own_2(Bref.P); I.own_2(Bref.I);   // transfer {P,I}
    if (values) X.own_2(Bref.X);        // transfer {X} ?
    delete (&B);
  }

  if (m>0 && n>0) { 
    assert(this->ok()); 
  }
}


//---------------------------------------------------------
template <typename T> inline
bool CS<T>::ok() const
//---------------------------------------------------------
{
  //-----------------------------------
  // not "ok" if empty or inconsistent
  //-----------------------------------

  // check for empty matrix
  if (m<=0 || n<=0) { return false; }

  // triplet: p holds nzmax column ids
  // for csc: p holds n column offsets, and p[n] <- nzmax
  if (nz >= 0) { if (P.size() != nzmax) return false; }
  else         { if (P.size() != (n+1)) return false; }

  // I holds nzmax row ids, 
  if (I.size() != nzmax) return false;
  // X holds {0 || nzmax} non-zeros
  if (m_values && (X.size() != nzmax)) return false;

  // non-empty and consistent
  return true; 
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::reset()
//---------------------------------------------------------
{
  // release allocations, reset counters and flags
  m = n = nzmax = nz = 0;
  P.Free(); I.Free(); X.Free();

  // not changing mode or name
  // m_mode = OBJ_real; 
  // m_values = 0;
  // m_name = "CS";
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::resize(int Nr, int Nc, int max_nz, int values, int triplet)
//---------------------------------------------------------
{
  // define dimensions, nzmax, (triplet or csc)
  m = Nr; n = Nc; m_values = values;
  nzmax = max_nz = std::max(max_nz, 1);
  nz = triplet ? 0 : -1;
  P.resize(triplet ? max_nz : n+1);
  I.resize(nzmax);
  X.resize(values ? max_nz : 0);

  if (m>0 && n>0) {
    assert(this->ok());
  }
}


//---------------------------------------------------------
template <typename T> inline
bool CS<T>::realloc(int max_nz, bool bCheck)
//---------------------------------------------------------
{
  // realloc internal arrays to accommodate max_nz entries
  // pass (max_nz<=0) to remove extra space (keeping data)

  if (bCheck) {         // expect size info to be correct
    if (!ok()) return false;    // inconsistent size info
  }

  if (max_nz <= 0) max_nz = (is_csc()) ? P[n] : nz;
  I.realloc( max_nz );

  // NBN: also realloc P when non-triplet. 
  // See C.fkeep(cs_rprune, rr) in dmperm
  P.realloc( is_triplet() ? max_nz : n+1);

  if (m_values) { X.realloc(max_nz); } // non-zeros
  this->nzmax = max_nz;
  return (ok());
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::own(const CS<T>& B, int values)
//---------------------------------------------------------
{
  // assume ownership of all data in another CS<T>

  if (this == &B) return (*this);

  umTRC(2, "taking ownership of CS(%s)...\n", B.name());

  // define dimensions, nzmax, (triplet or csc)
  m=B.m; n=B.n; nzmax=B.nzmax; nz=B.nz; 
  if (m_name == "CS") { m_name = B.m_name; }
  m_shape = B.m_shape;  // shapes will match

  if (B.X.size() != B.I.size()) values = 0;
  m_values = values;  // copy numeric data?

  // take ownership of B's internal arrays
  CS<T>& Bref = const_cast< CS<T>& >(B);
  P.own(Bref.P); I.own(Bref.I); // transfer {P,I}
  if (values) X.own(Bref.X);    // transfer {X} ?
  else X.Free();

  Bref.reset();    // reset B to empty

  if (m>0 && n>0) {
    assert(this->ok()); 
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::add_val(const T& x)
//---------------------------------------------------------
{
  // add x to all non-zeros

  // handle special cases
  if (!ok())     { umWARNING("CS<T>::add_val()", "matrix is empty"); return; }
  if ((T)0 == x) { return; }
  
  if (!m_values) {
    m_values = 1;         // add numeric data
    if (is_csc()) 
         X.resize(nzmax); // nzmax is size of index array
    else X.resize(nz);    // nz is size of index array
  }

  X += x;   // add x to each non-zero
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::mult_val(const T& x)
//---------------------------------------------------------
{
  // multiply all non-zeros by x

  // handle special cases
  if (!ok())     { umWARNING("CS<T>::mult_val()", "matrix is empty"); return; }
  if (!m_values) { umWARNING("CS<T>::mult_val()", "matrix is sybolic (numeric data not stored)"); return; }
  if ((T)1 == x) { return; }

  X *= x;   // mult each non-zero by x
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::operator += (const CS<T> &B)
//---------------------------------------------------------
{
  // this += B
  // deletes B (if temp)

  if (!is_csc() || !B.is_csc() || !is_compatible(B)) {
    umERROR("CS<T>::operator+=()", "Both args must be csc"); 
  }

  int anz = P[n], bnz = B.P[n];                 // num. nonzeros
  int values = (m_values && B.m_values)?1:0;    // do values?
  IVec w(m); Vector<T> x(values?m:0);           // workspaces
  CS<T> *C=new CS<T>(m,n,anz+bnz,values,0,OBJ_temp, "t.(+=)"); // result
  if (!C->ok() || !w.ok() || (values && !x.ok())) { umERROR("CS<T>::operator +=", "out of memory"); }
  
  T alpha=1, beta=1;
  int Nz=0, p=0;
  for (int j=0; j<n; ++j) {
    C->P[j] = Nz;     // column j of C starts here
    Nz = this->scatter(j, alpha, w, x, j+1, (*C), Nz);  // alpha*A(:,j)
    Nz =     B.scatter(j, beta,  w, x, j+1, (*C), Nz);  // beta *B(:,j)
    if (values) {
      for (p = C->P[j]; p<Nz; ++p) {
        C->X[p] = x[C->I[p]];
      }
    }
  }

  if (B.get_mode()==OBJ_temp) { 
    delete (&B);    // delete temporaries
  }
  C->P[n] = Nz;     // finalize the last column of C
  C->realloc(0);    // remove extra space from C
  (*this) = (*C);   // swap with result: A <- A+B
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::operator -= (const CS<T> &B)
//---------------------------------------------------------
{
  // this -= B
  // deletes B (if temp)

  if (!is_csc() || !B.is_csc() || !is_compatible(B)) {
    umERROR("CS<T>::operator-=()", "Both args must be csc"); 
  }

  int anz = P[n], bnz = B.P[n];                 // num. nonzeros
  int values = (m_values && B.m_values)?1:0;    // do values?
  IVec w(m); Vector<T> x(values?m:0);           // workspaces
  CS<T> *C=new CS<T>(m,n,anz+bnz,values,0,OBJ_temp, "t.(-=)"); // result
  if (!C->ok() || !w.ok() || (values && !x.ok())) { umERROR("CS<T>::operator -=", "out of memory"); }
  
  T alpha=1, beta = -1;   // (beta = -1) ==> subtract columns
  int Nz=0, p=0;
  for (int j=0; j<n; ++j) {
    C->P[j] = Nz;     // column j of C starts here
    Nz = this->scatter(j, alpha, w, x, j+1, (*C), Nz);  //  1 * A(:,j)
    Nz =     B.scatter(j, beta,  w, x, j+1, (*C), Nz);  // -1 * B(:,j)
    if (values) {
      for (p = C->P[j]; p<Nz; ++p) {
        C->X[p] = x[C->I[p]];
      }
    }
  }

  if (B.get_mode()==OBJ_temp) { 
    delete (&B);    // delete temporaries
  }

  C->P[n] = Nz;     // finalize the last column of C

#if (1)
  C->droptol();     // drop zeros, then conpact
#else
  C->realloc(0);    // compact only (retains zeros)
#endif
  (*this) = (*C);   // swap with result: A <- A+B
  return (*this);
}



///////////////////////////////////////////////////////////
//
// cs_* routines
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline 
int CS<T>::val_idx(int i, int j) const
//---------------------------------------------------------
{
  static int last_j = -1;  // remember last col
  static int last_i = -1;  // remember last row

//umWARNING("CS<T>::val_idx", "TODO: needs testing");
  CS_BOUNDS_CHECK_00(i,j); // 0-based interface

  // get 0-based index into {I,X} arrays for entry (i,j)
  // return -(idx+2) if entry not in sparsity pattern
  //         (idx is index of insertion point)

  //---------------------------------------------
  // speed up search by checking extreme cases
  //---------------------------------------------
  int len = P[j+1] - P[j];
  if (len <= 0) {return -2;}  // no entries in col j
  
  int iLO = I[P[j]];          // find lowest row index in col j
  if (i < iLO) { return -2; }
  
  int iHI = I[P[j+1]-1];      // find highest row index in col j
  if (i > iHI) { return -(len+2); }


#if (!CS_ROWS_ARE_SORTED)

  //#######################################################
  // FIXME: cannot guarantee that row ids are stored in
  // sequential order, so check row ids sequentially:
  //#######################################################

  // [SLOW] sequential search
  for (int p=P[j]; p<P[j+1]; ++p) {
    if (I[p]==i) 
      return p;   // entry(i,j) = (I[p],j) = X[p]
  }
  return -1;      // entry(i,j) not in sparsity pattern

#else

  // 2007/01/10: first test: known to be sorted
  //#######################################################
  // binary search of sorted array
  //#######################################################

  // {lo:hi} = 0-based row range in col j
  int lo=P[j], hi=P[j+1]-1;

  //---------------------------------------------
  // Optimize sequential access into sorted data,
  // check if target = next entry in same column:
  //---------------------------------------------
  
  if ((j==last_j) && (last_i>=0) && (last_i<hi)) 
  {
    if (i == I[1+last_i]) {
      ++last_i;
      return last_i;      // target was next entry
    } else {
      if (i >= I[last_i]) { 
        lo = last_i;      // search upper block
      } else {
        hi = last_i;      // search lower block
      }
    }
  }

  int mid=lo, tmp=0;
  while (lo <= hi) 
  {
    mid = (hi + lo)/2;
    if ((tmp=I[mid] - i) > 0)
    {
      hi = mid-1;
      if (I[hi] == i) {
        last_j=j; last_i=hi; // remember hi index
        return hi;
      }
    }
    else if (tmp < 0)
    {
      lo = mid+1;
      if (I[lo] == i) {       // check 
        last_j=j; last_i=lo;  // remember lo index
        return lo;
      }
    }
    else  // tmp == 0
    { 
      last_j=j; last_i=mid;   // remember mid index
      return mid;
    }
  }
  tmp = I[mid] - i;

  if (tmp > 0) {
    return -(mid+2);        // insert at mid
  } else {                  // tmp < 0
    return -(mid+3);        // insert at mid+1
  }
  //#######################################################
#endif
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::compress(bool bSort)
//---------------------------------------------------------
{
  // convert this from triplet to csc form

  if (is_csc()) { 
    return (*this);   // already converted
  } else if (!ok()) {
    umWARNING("CS<T>::compress()", "empty matrix"); 
    return (*this);   // empty: nothing to do
  }
  
  // accumulate compressed result in a temporary 
  // matrix, then swap ownership of the data

  // CS<T> B(m, n, nz, m_values, 0);   // temp matrix
  CS<T>* tmp=new CS<T>(m,n,nz,m_values, 0, OBJ_temp, "t.compress");

  CS<T>& B = (*tmp);                // reference
  int p=0, k=0; IVec w(n);          // workspace
  for (k=0; k<nz; ++k) w[P[k]]++;   // column counts
  B.cumsum(w, n);                   // column pointers
  for (k=0; k<nz; ++k) {
    B.I[p=w[P[k]]++] = I[k];        // this(i,j) becomes pth entry in B
    if (m_values) B.X[p] = X[k];
  }
  (*this) = B;        // replace this with csc form

  if (bSort) {
    dropsort();       // drop zeros and sort row indices
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::compress(const CS<T>& B, bool bSort)
//---------------------------------------------------------
{
  // copy B, converting triplet -> csc if necessary

  if (B.is_csc()) { 
    (*this) = B;      // B is in csc form, so
    return (*this);   // just copy and return.
  } else if (!B.ok()) {
    umWARNING("CS<T>::compress(B)", "empty matrix"); 
    return (*this);   // empty: nothing to do
  }

  // convert triplet data in B to csc form

  m_values = B.m_values;
  int Nr=B.m, Nc=B.n, Nz=B.nz, p=0, k=0;
  this->resize(Nr, Nc, Nz, m_values, 0);
  IVec w(Nc);                       // workspace
  for (k=0; k<Nz; ++k) w[B.P[k]]++; // column counts
  this->cumsum(w, Nc);              // column pointers
  for (k=0; k<Nz; ++k) {
    I[p=w[B.P[k]]++]  = B.I[k];     // A(i,j) becomes pth entry in C
    if (m_values) X[p]= B.X[k];
  }

  if (bSort) {
    dropsort();       // drop zeros and sort row indices
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
double CS<T>::cumsum(IVec& c, int Nc)
//---------------------------------------------------------
{
  // p [0..n] = cumulative sum of c [0..n-1], 
  // and then copy p [0..n-1] into c

  if (c.size() != Nc) return -1.0;
  int Nz=0;  double Nz2=0.0;
  for (int i=0; i<Nc; ++i) {
    P[i] = Nz;
    Nz  += c[i];
    Nz2 += c[i];  // also in double to avoid int overflow
    c[i] = P[i];  // also copy p[0..n-1] back into c[0..n-1]
  }
  P[Nc] = Nz;     // store total non-zeros
  return Nz2;     // return sum (c [0..n-1])
}


//---------------------------------------------------------
template <typename T> inline
cs_dp* CS<T>::dmperm (int seed) const
//---------------------------------------------------------
{
  // compute coarse and then fine dmperm for (*this)

  int i=0, j=0, k=0; bool bOk=true;
  //--- Maximum matching ----------------------------------
  if (!is_csc()) return NULL;
  cs_dp *D = new cs_dp(m,n);        // allocate result
  if (!D) return NULL;
  IVec &Dp=D->P, &Dq=D->Q, &Dr=D->R, &Ds=D->S;
  int *cc=D->cc, *rr=D->rr;
  IVec wij;                         // workspace for max transversal
  if (!maxtrans(wij, seed)) { delete D; return NULL; }
  int *jmatch = wij.data();         // jmatch
  int *imatch = wij.data()+m;       // imatch = inverse of jmatch
  //--- Coarse decomposition ------------------------------
  int *wi=Dr.data(), *wj=Ds.data(); // use Dr and Ds as workspace
  for (j=0; j<n; ++j) wj[j] = -1;   // unmark all cols for bfs
  for (i=0; i<m; ++i) wi[i] = -1;   // unmark all rows for bfs
      CS_bfs(*this,n,wi,wj,Dq,imatch,jmatch,1);  // find C1, R1 from C0
  bOk=CS_bfs(*this,m,wj,wi,Dp,jmatch,imatch,3);  // find R3, C3 from R0
  if (!bOk) { delete D; return NULL; }

  CS_unmatched(n, wj, Dq, cc, 0) ;                    // unmatched set C0
  CS_matched  (n, wj, imatch, Dp, Dq, cc, rr, 1,  1); // set R1 and C1
  CS_matched  (n, wj, imatch, Dp, Dq, cc, rr, 2, -1); // set R2 and C2
  CS_matched  (n, wj, imatch, Dp, Dq, cc, rr, 3,  3); // set R3 and C3
  CS_unmatched(m, wi, Dp, rr, 3) ;                    // unmatched set R0
  wij.Free();  // release workspace

  //--- Fine decomposition --------------------------------
  IVec p_inv = CS_pinv(Dp, m);            // p_inv=p'
  if (!p_inv.ok()) {delete D; return NULL;}

  CS<T> C = perm(*this, p_inv, Dq, 0);    // C=A(p,q) (it will hold A(R2,C2))
  if (!C.ok()) { 
    umWARNING("CS<T>::dmperm","error permuting matrix"); 
    delete D; return NULL;
  }

  // p_inv.resize(0); // cs_free (p_inv);

  int Nc = cc[3] - cc[2];     // delete cols C0, C1, and C3 from C
  if (cc[2]>0) {
    for (j=cc[2]; j<=cc[3]; ++j) { C.P[j-cc[2]] = C.P[j]; }
  }

  C.n = Nc;
  if (rr[2]-rr[1] < m) {      // delete rows R0, R1, and R3 from C
    C.fkeep(&CS_rprune, rr);
    int cnz = C.P[Nc];
    if (rr[1] > 0) { for (k=0; k<cnz; ++k) {C.I[k] -= rr[1]; } }
  }

  C.m = Nc;
  cs_dp *scc = C.SCC();       // find strongly connected components of C
  if (!scc) { delete D; return NULL; }

  //--- Combine coarse and fine decompositions ------------
  IVec& ps = scc->P;          // C(ps,ps) is the permuted matrix
  IVec& rs = scc->R;          // kth block is rs[k]..rs[k+1]-1
  int nb1 = scc->nb;          // # of blocks of A(R2,C2)
  for (k=0; k<Nc; k++) wj[k]       = Dq[ps[k]+cc[2]];
  for (k=0; k<Nc; k++) Dq[k+cc[2]] = wj[k];
  for (k=0; k<Nc; k++) wi[k]       = Dp[ps[k]+rr[1]];
  for (k=0; k<Nc; k++) Dp[k+rr[1]] = wi[k];
  int nb2 = 0;                // create the fine block partitions
  Dr[0] = Ds[0] = 0;

  if (cc [2] > 0) nb2++;      // leading coarse block A (R1, [C0 C1])
  for (k=0; k<nb1; ++k) {     // coarse block A (R2,C2)
    Dr[nb2] = rs[k] + rr[1];  // A (R2,C2) splits into nb1 fine blocks
    Ds[nb2] = rs[k] + cc[2];
    nb2++ ;
  }
  if (rr[2] < m) {
    Dr[nb2] = rr[2];          // trailing coarse block A ([R3 R0], C3)
    Ds[nb2] = cc[3];
    nb2++ ;
  }
  Dr[nb2] = m;
  Ds[nb2] = n;
  D->nb   = nb2;
  delete scc;
  return D;
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::dropdiag()
//---------------------------------------------------------
{
  // drop diagonal entries from csc matrix

  if (!is_csc()) {umWARNING("CS<T>::dropdiag", "expected csc form"); return;}

  int p=0, Nz=0;
  for (int j=0; j<n; ++j) 
  {
    p = P[j];     // get current location of col j
    P[j] = Nz;    // record new location of col j
    
    for ( ; p<P[j+1]; ++p) {
      if (I[p] != j) {
        if (m_values) X[Nz] = X[p]; // keep A(i,   j)
        I[Nz++] = I[p];             // i.e. A(I[p],j)
      }
    }
  }

  P[n] = Nz;    // finalize: Nz non-zeros passed droptol test
  realloc(0);   // remove extra space
}


//---------------------------------------------------------
template <typename T> inline
int CS<T>::dropsort(double tol)
//---------------------------------------------------------
{
  // 1. drop entries with |x| <= tol
  // 2. sort row indices in each column.
  //
  // Note: duplicate entries
  //-------------------------
  // Currently retains the last value found for duplicate
  // entries.  An alternative is to sum such duplicates.

  if (!is_csc()) { umWARNING("CS<T>::sort()", "expected csc form"); return 0; }
  if (tol<0.0)   { tol = 0.0; }  // verfy drop tolerance


  IVec w(m, -1);  assert(w.ok()); // init workspace array to -1 (row i not yet seen)
  int Nz=0, i=0, j=0, p=0, q=0;
  for (j=0; j<n; ++j) 
  {
    q = Nz;     // column j starts at q

    for (p=P[j]; p<P[j+1]; ++p) 
    {

      if (std::abs(X[p]) <= tol) {
        continue;         // drop this entry
      }
      i = I[p];           // A(i,j) passed droptol test
      if (w[i] >= q)      // now check for duplicates...
      {
        //-------------------------------
        // duplicate entry for A(i,j)
        //-------------------------------
        X[w[i]]  = X[p];  // retain only last value
      //X[w[i]] += X[p];  // sum duplicate values

      } else {
        w[i]    = Nz;     // record where row i occurs
        I[Nz]   = i;      // keep A(i,j)
        X[Nz++] = X[p];
      }
    }
    P[j] = q;   // record start of column j
  }
  P[n] = Nz;    // record total nnz
  realloc(0);   // remove extra space

#if (1)
  //#######################################################
  // sort array A[n] using workspace W[n]
  // void mergesort(int A[], int W[], int n);
  // mergesort(Ci, (int*) (Cx+pcstart), cnz - pcstart);
  //#######################################################
  //umTRC(1, "Starting dropsort (m,n) = (%d,%d)\n", m,n);
  //#######################################################

  IVecSort sv, sortI; IVec iv; VecSort<T> sortX;
  int istart=0,istop=0,len=0;
  for (j=0; j<n; ++j) {

    istart=P[j]; istop=P[j+1];  len=istop-istart; 

    //umTRC(1, " -- column %5d  (len = %3d)\n", j, len);

    if (len>1)
    {
      sv.borrow(len, &(I[istart])); sv.makeIndex(iv);

      sortI.borrow(len, &(I[istart])); // wrap row.ids for col j
      sortX.borrow(len, &(X[istart])); // wrap values for col j

      sortI.sortFromIndexVec(iv);   // sort indices in place
      sortX.sortFromIndexVec(iv);   // sort values in place
    }
  }

  //#######################################################
  //umTRC(1, "\nDone -- dropsort complete\n\n");
  //#######################################################
#endif



  return 1;
}


//---------------------------------------------------------
template <typename T> inline
int CS<T>::droptol(double tol)
//---------------------------------------------------------
{
  // drop entries with |x| <= tol
  // return new Nz if ok(), else -1

  if (!m_values) {
    umTRC(2, "CS<T>::droptol called on symbolic object (no values).\n"); 
    return P[n];
  }

  assert(tol >= 0.0);
  if (!is_csc()) {umWARNING("CS<T>::droptol", "Please compress before calling droptol()."); return -1;}
//if (!m_values) {umWARNING("CS<T>::droptol", "Values are not yet stored in this object."); return P[n];}

#ifndef NDEBUG
  int Nz1 = P[n]; // get current nnz
#endif

  int p=0, Nz=0;
  for (int j=0; j<n; ++j) 
  {
    p = P[j];     // get current location of col j
    P[j] = Nz;    // record new location of col j
    
    for ( ; p<P[j+1]; ++p) {
      if (std::abs(X[p]) > tol) {
        X[Nz  ] = X[p];   // keep A(i,   j)
        I[Nz++] = I[p];   // i.e. A(I[p],j)
      }
    }
  }

#ifndef NDEBUG
  if (Nz1 != Nz) umMSG(1, "droptol: dropped %d entries from %s\n", Nz1-Nz, name());
  else           umMSG(1, "droptol: no dropped entries from %s\n", name());
#endif

  P[n] = Nz;    // finalize: Nz non-zeros passed droptol test
  realloc(0);   // remove extra space
  return Nz;    // num. non-zeros that remain
}


//---------------------------------------------------------
template <typename T> inline
int CS<T>::dupl()
//---------------------------------------------------------
{
  // sum duplicate entries, and remove

  if (!is_csc()) { umWARNING("CS<T>::dupl()", "expected csc form"); return 0; }

  IVec w(m, -1);  assert(w.ok()); // init workspace array to -1 (row i not yet seen)
  int Nz=0, i=0, j=0, p=0, q=0;
  for (j=0; j<n; ++j) 
  {
    q = Nz;     // column j starts at q

    for (p=P[j]; p<P[j+1]; ++p) {
      i = I[p];           // A(i,j) is nonzero
      if (w[i] >= q) {
        X[w[i]] += X[p];  // A(i,j) is a duplicate
      } else {
        w[i]    = Nz;     // record where row i occurs
        I[Nz]   = i;      // keep A(i,j)
        X[Nz++] = X[p];
      }
    }
    P[j] = q;   // record start of column j
  }
  P[n] = Nz;    // finalize: record nnz
  realloc(0);   // remove extra space
  return 1;
}

    
//---------------------------------------------------------
template <typename T> inline
bool CS<T>::entry(int i, int j, T x)
//---------------------------------------------------------
{
  // add an entry to a triplet matrix; 
  // indices i,j are 0-based
  // return 1 if ok, 0 otherwise

  if (!is_triplet() || i < 0 || j < 0) return false;

  if (T(0) == x) {
    // avoid adding zeros, but allow that the user might 
    // be defining the matrix "size" with this entry:
    m = std::max(m, i+1);
    n = std::max(n, j+1);
    return false;         
  }

  if (this->nz >= this->nzmax) {
    if (!realloc (2*(this->nzmax))) return false;
  }

  if (m_values) X[nz] = x;
  I[nz] = i; P[nz++] = j;
  m = std::max(m, i+1);
  n = std::max(n, j+1);
  return true;
}


//---------------------------------------------------------
template <typename T> inline
bool CS<T>::set1(int i, int j, T x)
//---------------------------------------------------------
{
  // alias for entry(i,j,x) 
  // helper for 1-based indices 
  return entry(i-1, j-1, x);
}


//---------------------------------------------------------
template <typename T>
int CS<T>::ereach(int k, const IVec& parent, int *s, int *w)
//---------------------------------------------------------
{
  // find nonzero pattern of Cholesky L(k,1:k-1) 
  // using etree and triu(A(:,k))

  // check inputs
  if (!this->is_csc() || !parent.ok() || !s || !w) return -1;

  int top=n, i=0, p=0, len=0;
  CS_MARK (w, k);           // mark node k as visited
  for (p=P[k]; p<P[k+1]; ++p)
  {
    i = I[p];               // A(i,k) is nonzero
    if (i>k) continue;      // only use upper triangular part of A
    for (len=0; !CS_MARKED (w,i); i=parent[i]) // traverse up etree
    {
      s[len++] = i;         // L(k,i) is nonzero
      CS_MARK (w, i);       // mark i as visited
    }
    while (len>0) {
      s[--top] = s[--len];  // push path onto stack
    }
  }
  for (p=top; p<n; ++p) CS_MARK (w, s[p]);  // unmark all nodes
  CS_MARK (w, k);   // unmark node k
  return (top);     // s[top..n-1] contains pattern of L(k,:)
}


//---------------------------------------------------------
template <typename T>
IVec& CS<T>::etree(int ata) const
//---------------------------------------------------------
{
  // compute the etree of A (using triu(A), 
  // or A'A without forming A'A

  if (!is_csc()) { umERROR("CS<T>::etree","expected csc matrix"); }

  int i=0, k=0, p=0, inext=0;
  IVec* parent = new IVec(n, "parent", OBJ_temp); // allocate result
  IVec w(n+(ata?m:0));  // workspace
  if (!w.ok() || !parent->ok()) {
    umERROR("CS<T>::etree","out of memory"); return (*parent);
  }
  int *wd = w.data();
  int *ancestor=wd, *prev=wd+n;
  int *pd = parent->data();
  if (ata) for (i=0; i<m; ++i) prev[i] = -1;
  for (k=0; k<n; ++k)
  {
    pd[k] = -1;                       // node k has no parent yet
    ancestor[k] = -1 ;                // nor does k have an ancestor
    for (p=P[k]; p<P[k+1]; ++p)
    {
      i = ata ? (prev[I[p]]) : (I[p]);
      for ( ; i!=-1 && i<k; i=inext)  // traverse from i to k
      {
        inext = ancestor[i];          // inext = ancestor of i
        ancestor[i] = k;              // path compression
        if (inext == -1) pd[i]=k;     // no anc., parent is k
      }
      if (ata) prev[I[p]]=k;
    }
  }
  return (*parent);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::identity(int Ni, bool pack)
//---------------------------------------------------------
{
  resize(Ni, Ni, Ni, 1, 1); // alloc (n,n) triplet form
  
  for (int i=0; i<Ni; ++i) {
    entry (i, i, T(1));
  }

  if (pack) {
    compress();     // convert to csc form
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
int CS<T>::is_tri() const
//---------------------------------------------------------
{
  // Returns:
  //  1 if A is square & upper tri., 
  // -1 if square & lower tri., 
  //  0 otherwise

  if (m != n) return (0);
  bool is_upper=true, is_lower=true;
  for (int j=0; j<n; ++j) {
    for (int p = P[j]; p<P[j+1]; ++p) {
      if (I[p] > j) is_upper = false; // not upper tri.
      if (I[p] < j) is_lower = false; // not lower tri.
    }
  }
  return (is_upper ? 1 : (is_lower ? -1 : 0)) ;
}


//---------------------------------------------------------
template <typename T>
IMat& CS<T>::find2D(char op, T val) const
//---------------------------------------------------------
{
  // Return both row and column info.
  // See also find2D(Mat_COL<T>,...), Vector<T>::find(...)

  if (!ok())     { umERROR("CS<T>::find2D()", "empty matrix"); }
  if (!is_csc()) { umERROR("CS<T>::find2D()", "expected csc matrix"); }

  CS<int> mask(m,n, 1, 1, 1);   // record hits in (triplet) mask
  int count=0, j=0, p=0,i=0;

  switch (op) {

  case '<':   // find ids of elements less than val
    //-----------------------------------------------------
    for (j=0; j<n; ++j) {
      for (p=P[j]; p<P[j+1]; ++p) {
        if (X[p] < val) {
          i = I[p];
          mask.entry(i,j,1);    // A(i,j) satisfies condition
          ++count;              // increment total hits
        }
      }
    }
    break;

  case '=':   // find ids of elements that equal val
    //-----------------------------------------------------
    for (j=0; j<n; ++j) {
      for (p=P[j]; p<P[j+1]; ++p) {
        if (X[p] == val) {
          i = I[p];
          mask.entry(i,j,1);    // A(i,j) satisfies condition
          ++count;              // increment total hits
        }
      }
    }
    break;

  case '>':   // find ids of elements greater than val
    //-----------------------------------------------------
    for (j=0; j<n; ++j) {
      for (p=P[j]; p<P[j+1]; ++p) {
        if (X[p] > val) {
          i = I[p];
          mask.entry(i,j,1);    // A(i,j) satisfies condition
          ++count;              // increment total hits
        }
      }
    }
    break;
  }

  mask.compress();    // convert A to csc form
  mask.dupl();        // sum up duplicates

  // extract find() results
  std::string sz;
  if (sizeof(T) == sizeof(double)) 
       { sz=umOFORM("find(CSd, %c, %g)", op, val); } 
  else { sz=umOFORM("find(CSi, %c, %d)", op, val); }

  // allocate result array
  IMat *tmp = new IMat(count, 2, sz.c_str(), OBJ_temp);

  int sk=0;
  for (j=0; j<n; ++j) {
    for (p=mask.P[j]; p<mask.P[j+1]; ++p) {
      assert(1 == mask.X[p]); // mask only recorded "hits"
      ++sk;                   // 1-based IMat
      i = mask.I[p];
      (*tmp)(sk,1) = 1 + i;   // row indices in col 1
      (*tmp)(sk,2) = 1 + j;   // col indices in col 2
    }
  }
  assert(count == sk);

  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
int CS<T>::fkeep(KeepFunc fK, void *other)
//---------------------------------------------------------
{
  // drop entries for which fkeep(A(i,j)) is false; 
  // return nz if OK, else -1

  if (!is_csc()) {umWARNING("CS<T>::fkeep", "expected csc form"); return -1;}
  if (!fK)       {umWARNING("CS<T>::fkeep", "invalid KeepFunc"); return -1;}

  int nz=0, p=0;
  for (int j=0; j<n; ++j) {
    p = P[j];          // get current location of col j
    P[j] = nz;         // record new location of col j
    for ( ; p<P[j+1]; ++p) {
      if (fK(I[p], j, m_values ? (double)X[p] : 1.0, other)) {
        if (m_values) X[nz] = X[p]; // keep A(i,j)
        I[nz++] = I[p];
      }
    }
  }
  P[n] = nz ;         // finalize nnz, then remove any extra space.
  realloc(0,false);   // false => size info no longer consistent!
  return nz;
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::gaxpy(const Vector<T>& x, Vector<T>& y) const
//---------------------------------------------------------
{
  // y += A*x

  assert(is_csc() && x.ok() && y.ok());
  assert(num_rows()==y.size() && num_cols()==x.size());

  T xj=(T)0;
  for (int j=0; j<n; ++j) {
    xj = x[j];
    if (T(0) != xj) {
      for (int p=P[j]; p<P[j+1]; ++p) {
        y[I[p]] += X[p] * xj;
      }
    }
  }
}


//---------------------------------------------------------
template <typename T> inline
void CS<T>::gxapy(const Vector<T>& x, Vector<T>& y) const
//---------------------------------------------------------
{
  // y += x*A

#if (USE_SPARSE_THREADS)
  // Using OpenMP + ACML sparse BLAS
#else

  assert(is_csc() && x.ok() && y.ok());
  assert(num_cols()==y.size() && num_rows()==x.size());

  for (int j=0; j<n; ++j) {
    for (int p=P[j]; p<P[j+1]; ++p) {
      y[j] += X[p] * x[p];
    }
  }

#endif // (USE_SPARSE_THREADS)
}


//---------------------------------------------------------
template <typename T>
void CS<T>::load(FILE *fp)
//---------------------------------------------------------
{
  if (!fp) { umWARNING("CS<T>::load", "invalid file"); return; }

  this->resize(0, 0, 1, 1, 1);    // reset to empty (triplet form)

  int i=0, j=0;  double x=0.0;
  while (fscanf (fp, "%d %d %lg\n", &i, &j, &x) == 3)
  {
    if (!entry (i, j, T(x))) {
      umERROR("CS<T>::load", "failed to add entry (%d,%d) = %g)", i,j,x);
    }
  }
}


//---------------------------------------------------------
template <typename T>
void CS<T>::load
(
  int Nr, int Nc,     // matrix dimensions
  IVec&   ir,         // row indices
  IVec&   jc,         // col indices
  Vector<T>& Ax,      // numeric data (TODO: make optional)
  int     part,       // select {upper/lower/all}
  bool    forceSym,   // mirror L\U triangle?
  double  tol,        // drop tolerance
  bool    free_args   // release {ir,jc, Ax} arrays?
)
//---------------------------------------------------------
{
  // convert {i,j,Aij} triplets to csc format.
  // {ir,jc} indices are 0-based 
  // duplicate triples are summed

  int Nz = Ax.size(), i=0,j=0,k=0; T x=0; bool bOk=true;
  assert(ir.size()>=Nz);  // need row index for each A_ij
  assert(jc.size()>=Nz);  // need col index for each A_ij
  if (tol<0.0) tol=0.0;

#if (0)
  // set to empty (triplet form) with logical size (Nr,Nc)
  this->resize(Nr, Nc, 1, 1, 1);
#else
  // set to triplet form with estimated nnz and logical size (Nr,Nc)
  int to_load = Ax.get_nnz(tol);
  if (part != sp_All) {
    int Nn=std::max(Nr,Nc);           // estimate storage required
    to_load = Nn+5 + (to_load-Nn)/2;  // ... to load L\U triangle
  }
  this->resize(Nr, Nc, to_load, 1, 1);
#endif

  //---------------------------------------------
  // select which part of the matrix to load
  //---------------------------------------------
  switch (part) {

  case sp_LT:
    //-----------------------------------
    // load entries in lower triangle
    //-----------------------------------
    m_shape = (sp_LOWER | sp_TRIANGULAR);

    for (k=0; k<Nz; ++k) {
      i=ir[k]; j=jc[k]; 
      if (i >= j) {     // is entry in lower triangle ?
        x=Ax[k]; if (fabs(double(x)) > tol) {
          bOk=entry(i,j,x); assert(bOk);
        }
      }
    } 
    break;

  case sp_All:
    //-----------------------------------
    // load all entries -- no "shape"
    //-----------------------------------
    m_shape = sp_NONE;

    for (k=0; k<Nz; ++k) {
      i=ir[k]; j=jc[k]; x=Ax[k];
      if (fabs(double(x))>tol) { 
        bOk=entry(i,j,x); assert(bOk);
      }
    }
    break;

  case sp_UT:
    //-----------------------------------
    // load entries in upper triangle
    //-----------------------------------
    m_shape = (sp_UPPER | sp_TRIANGULAR);

    for (k=0; k<Nz; ++k) { 
      i=ir[k]; j=jc[k]; 
      if (i <= j) {     // is entry in upper triangle?
        x=Ax[k]; if (fabs(double(x))>tol) {
          bOk=entry(i,j,x); assert(bOk);
        }
      } 
    } 
    break; 
  }

  if (free_args) {
    // If free_args is true, then the user is allowing us
    // to release these allocations, which can be HUGE(!).
    // make_tri_sym() can then use the freed memory.
    ir.Free(); jc.Free(); Ax.Free();
  }

  this->compress();       // convert to csc form
  this->dupl();           // sum up duplicates
//this->droptol(tol);     // all x passed tol test above.
  if (part != sp_All) {   // was only L\U triangle loaded?
    if (forceSym) {       // make L\U tri symmetric?
      make_tri_sym();     // mirror L\U triangle
    }
  }

  if (!ok()) {
    umERROR("CS<T>::load(m,n, ...)", "error loading triplet data");
  }

#if (0)
  //#######################################################
  // Testing sort() behavior:
  //#######################################################
  {
    dropsort();       // drop zeros and sort row indices

    FILE* fp = fopen("MLsorted.dat", "w");
  //FILE* fp = fopen("MLnosort.dat", "w");
    write_ML(fp);
    fclose(fp);
  }
  //#######################################################
#endif

}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::make_tri_sym()
//---------------------------------------------------------
{
  // A += triu(A,1)'
  //
  // Note: this routine is intended to make a symmetric
  // matrix from an upper or lower triangular csc matrix

  assert(get_mode()==OBJ_real); // else need to adjust mode

  // AT = A'
  CS<T>* AT = new CS<T>(trans(*this),OBJ_temp,"(self)^(T)",1);
  AT->dropdiag();     // drop diagonal entries from AT
  (*this) += (*AT);   // += deletes AT (OBJ_temp)
  return (*this);
}


//---------------------------------------------------------
template <typename T>
bool CS<T>::maxtrans(IVec& jimatch, int seed) const
//---------------------------------------------------------
{
  // find a maximum transveral
  // [jmatch [0..m-1]; imatch [0..n-1]]

  if (!is_csc()) return false;

  int i=0, j=0, k=0, p=0;
  int *jmatch=NULL, *imatch=NULL;

  jimatch.resize(m+n);        // allocate result
  if (!jimatch.ok()) return false;

  int n2=0, m2=0;
  for (k=0, j=0; j<n; ++j)    // count nonempty rows and columns
  {
    if (P[j] < P[j+1]) {++n2;}
    for (p=P[j]; p<P[j+1]; ++p) {
      jimatch[I[p]] = 1;
      if (j==I[p]) {++k;}     // count entries already on diagonal
    }
  }
  if (k == std::min(m,n))      // quick return if diagonal zero-free
  {
    jmatch = jimatch.data();
    imatch = jimatch.data() + m;
    for (i=0 ; i<k; ++i) jmatch[i] =  i;
    for (    ; i<m; ++i) jmatch[i] = -1;
    for (j=0 ; j<k; ++j) imatch[j] =  j;
    for (    ; j<n; ++j) imatch[j] = -1;
    return true;              // done (results in jimatch)
  }

  for (i=0; i<m; ++i) m2 += jimatch[i];

  // select either this or transpose
  CS<T> *C = const_cast<CS<T>*>(this);
  bool use_trans = false;
  if (m2 < n2) {
    use_trans = true;                 // work with transpose
    C = new CS<T>(trans((*this),0));  // copy structure only
    if (!C->ok()) { delete C; return false; }
  }

  // allow for transposed case
  int cn=C->n, cm=C->m;
  int* ji_data = jimatch.data();
  jmatch = (m2 < n2) ? ji_data + cn : ji_data;
  imatch = (m2 < n2) ? ji_data : ji_data + cm;
  
  // allocate workspace and set pointers
  IVec w(5*cn);  assert(w.ok());
  int *wd = w.data();
  int *cheap=wd+cn, *js=wd+2*cn, *is=wd+3*cn, *ps=wd+4*cn;
  for (j=0; j<cn; ++j)      w[j] = -1;      // all columns unflagged
  for (j=0; j<cn; ++j)  cheap[j] = C->P[j]; // for cheap assignment
  for (i=0; i<cm; ++i) jmatch[i] = -1;      // nothing matched yet

  IVec q;
  CS_randperm (cn, seed, q);    // q = random permutation
  bool bq = (q.size()==cn);
  for (k=0; k<cn; ++k) {        // augment, starting at column q[k]
    CS_augment(bq?q[k]:k, (*C), jmatch, cheap, w, js, is, ps) ;
  }
  // q.Free(); // release workspace
  for (j=0; j<cn; ++j) imatch[j] = -1;    // find row match
  for (i=0; i<cm; ++i) {
    if (jmatch[i]>=0) {imatch[jmatch[i]]=i;}
  }

  if (use_trans) { delete C; } // if allocated here, release C

  return true;
}


//---------------------------------------------------------
template <typename T>
CS<T>& CS<T>::operator *= (const CS<T>&B)
//---------------------------------------------------------
{
  // this *= B, (column-wise gaxpy multiplication)
  // deletes B (if temp)

  if (!is_csc() || !B.is_csc()) { 
    umERROR("CS<T>::multiply", "Both args must be csc"); 
  }

  int Nr=this->m, anz=this->P[n], Nc=B.n, bnz=B.P[B.n];
  int values = (m_values && B.m_values)?1:0;
  IVec w(Nr); Vector<T> x(values ? Nr : 0);  // workspaces

  bool size_is_known = false;

#if (1)

  // umWARNING("sparse A*=B", "Nigel, please check this untested code!");

  //#######################################################
  // preprocess to find nnz in result, C = A*B
  //#######################################################
  int cnz=0;
  {
  //IVec Cp(Nc+1, "Cp");    // precalc pattern for C?
    IVec Flag(Nr, "Flag");  // workspace
    int pa=0,paend=0,pb=0,pbend=0,mark=0,pcstart=0,i=0,j=0,k=0;
    const IVec &Ap=this->P, &Ai=this->I, &Bi=B.I;
    for (j=0; j<Nc; ++j) {
      mark-- ;                    // Flag [0:n-1] != mark is now true
      pbend = B.P[j+1];
      pcstart = cnz;
    //Cp[j] = cnz;
      for ( ; pb<pbend; ++pb) {
        k = Bi[pb];               // nonzero entry B(k,j)
        paend = Ap[k+1];
        for (pa=Ap[k]; pa<paend; ++pa) {
          i = Ai[pa];             // nonzero entry A(i,k)
          if (Flag[i] != mark) {  // C(i,j) is a new nonzero
            Flag[i] = mark;       // mark i as appearing in C(:,j)
            cnz++ ;               // increment number of non-zeros
          }
        }
      }
      if (cnz < pcstart) {
        umERROR("sparse A*=B", "integer overflow: nnz too big for type int") ;
      }
    }
  //Cp[Nc] = cnz;
  }

  umMSG(1, "preprocessing A*=B : nnz in result = %d\n", cnz);
  CS<T> *C = new CS<T>(Nr, Nc, cnz, values, 0, OBJ_temp, "t.(*=)");
  
  size_is_known = true;

  //#######################################################
#else
  //#######################################################

  CS<T> *C = new CS<T>(Nr, Nc, anz+bnz, values, 0, OBJ_temp, "t.(*=)");

  //#######################################################
#endif


  if (!C->ok() || !w.ok() || (values && !x.ok())) { 
    umERROR("CS<T>::multiply", "out of memory"); 
  }
  
  int Nz=0, p=0;
  for (int j=0; j<Nc; ++j)
  {
    // if (Nz+Nr > C->nzmax)
    if ((!size_is_known) && (Nz+Nr > C->nzmax))
    {
      C->realloc(2*C->nzmax+Nr);
      if (!C->ok()) { umERROR("CS<T>::multiply", "out of memory"); }
    } 

    C->P[j] = Nz;   // column j of C starts here
    for (p=B.P[j]; p<B.P[j+1]; ++p) {
      Nz = this->scatter(B.I[p], values?B.X[p]:T(1), w, x, j+1, (*C), Nz);
    }
    if (values) { 
      for (p=C->P[j]; p<Nz; ++p) {C->X[p] = x[C->I[p]];} 
    }
  }

  // if B is temporary, delete it
  if (B.get_mode()==OBJ_temp) {delete (&B);}

  C->P[Nc] = Nz;    // finalize last column of C
  C->droptol();     // drop zeros, then conpact
  (*this) = (*C);   // swap with result: A <- A*C
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
double CS<T>::nonsymmetry() const
//---------------------------------------------------------
{
  umERROR("CS<T>::nonsymmetry", "not implemented");

  // TODO:  find max relative non-symmetry: 
  //        e.g. max( (A-A')/abs(A) )

//int old_mode = m_mode;        // avoid premature delete
  assert(OBJ_real==m_mode);     // TODO: make mutable

  const CS<T>& ref = (*this);
  CS<T> A2 = ref - trans(ref);  // compute (A-A')
  Vector<T>& X2 = A2.X;
  X2 /= abs(this->X);           // compute (A-A')/abs(A)
//m_mode = old_mode;            // restore mode

  double max_val = X2.max_val_abs();
  return max_val;
}


//---------------------------------------------------------
template <typename T> inline
T CS<T>::norm() const
//---------------------------------------------------------
{
  // return 1-norm: largest column sum

  if (!is_csc() || !m_values) return T(-1);

  T nrm1=0, s=0; int p=0;
  for (int j=0; j<n; ++j) {
    for (s=0, p=P[j]; p<P[j+1]; ++p) { s += std::abs(X[p]); }
    nrm1 = std::max(nrm1, s);
  }
  return nrm1;
}


//---------------------------------------------------------
template <typename T>
CS<T>& CS<T>::permute(const IVec& pinv, const IVec& q, int values)
//---------------------------------------------------------
{
  // permute this "in-place"
  if (!ok())     {umWARNING("CS<T>::permute","empty matrix"); return (*this);}
  if (!is_csc()) {umWARNING("CS<T>::permute","expected csc"); return (*this);}
  if (m_values && !values) {X.resize(0);} // drop numeric data

  bool swap_mode = false;
  if (OBJ_temp == m_mode) {
    m_mode = OBJ_real;  // avoid prematue deletion of this
    swap_mode = true;   // during copy constructor for B
  }

  m_values = values;

  // copy current state into C, then permute (*this)
  CS<T> C(*this, OBJ_temp, "CS", values);
  if (!C.ok()) { umERROR("CS<T>::permute", "error making copy"); }

  int Nz=0, j=0,t=0;  bool bpi=pinv.ok(), bq=q.ok();
  for (int k=0; k<n; ++k) {
    this->P[k] = Nz;
    j = bq ? q[k] : k;        // col k of result is col q[k] of original
    for (t=C.P[j]; t<C.P[j+1]; ++t) {
      if (values) this->X[Nz]=C.X[t];  // row i of original is row pinv[i] of result
      this->I[Nz++] = bpi ? pinv[C.I[t]] : C.I[t];
    }
  }
  this->P[n] = Nz;                    // finalize last column
  if (swap_mode) { m_mode=OBJ_temp; } // restore "temp" mode
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::permuteP(const IVec& pinv, int values)
//---------------------------------------------------------
{
  // Apply left-permutation only: A = p(A)
  IVec NoQ;
  permute(pinv, NoQ, values);
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::permuteQ(const IVec& q, int values)
//---------------------------------------------------------
{
  // Apply right-permutation only: A = (A)q
  IVec NoPinv; 
  permute(NoPinv, q, values);
  return (*this);
}


//---------------------------------------------------------
template <typename T>
void CS<T>::print(bool brief)
//---------------------------------------------------------
{
  if (!ok()) { printf("(null)\n"); return; }

  if (nz < 0) {
    // compressed sparse column
    printf ("%d-by-%d, nzmax: %d nnz: %d, 1-norm: %g\n", m, n, nzmax, P[n], (double)norm());
    for (int j=0; j<n; ++j) {
      printf ("    col %d : locations %d to %d\n", j, P[j], P[j+1]-1);
      for (int p=P[j]; p<P[j+1]; ++p) {
        printf ("      %d : %g\n", I[p], m_values ? (double)X[p] : 1.0);
        if (brief && p>20) { printf ("  ...\n"); return; }
      }
    }
  } else {
    // triplet form
    printf ("triplet: %d-by-%d, nzmax: %d nnz: %d\n", m, n, nzmax, nz);
    for (int p=0; p<nz; ++p) {
      printf ("    %d %d : %g\n", I[p], P[p], m_values ? (double)X[p] : 1);
      if (brief && p>20) { printf ("  ...\n"); return; }
    }
  }
}


//---------------------------------------------------------
template <typename T> inline
int CS<T>::scatter
(
  int         j, 
  T           beta, 
  IVec&       w, 
  Vector<T>&  x, 
  int         mark, 
  CS<T>&      C, 
  int         Nz
) const
//---------------------------------------------------------
{
  // x += beta * A(:,j), 
  // where x is a dense vector and A(:,j) is sparse

  assert(this->is_csc() && C.is_csc() && w.ok());
  bool values = x.ok();

  int i=0, p=0;
  for (p=P[j]; p<P[j+1]; ++p) {
    i = this->I[p];             // A(i,j) is nonzero
    if (w[i] < mark) {
      w[i] = mark;              // i is new entry in column j
      C.I[Nz++] = i;            // add i to pattern of C(:,j)
      if (values) {
        x[i] = beta*this->X[p]; // x(i) = beta*A(i,j)
      }
    }
    else if (values) {
      x[i] += beta*this->X[p];  // i exists in C(:,j) already
    }
  }
  return Nz;
}


//---------------------------------------------------------
template <typename T>
cs_dp* CS<T>::SCC()
//---------------------------------------------------------
{
  // find the strongly connected components of a square matrix
  // matrix A temporarily modified, then restored

  if (this->n > 0) {
    if (!ok() || !is_csc()) {umWARNING("CS<T>::SCC","invalid matrix"); return NULL;}
  }

  cs_dp *D = new cs_dp(n, 0);     // allocate result
  CS<T> AT = trans((*this), 0);   // AT = A'
  IVec xi(2*n+1);                 // workspace
  if (!D || !xi.ok())  { umWARNING("CS<T>::SCC", "D or xi empty"); return NULL; }
  if (n>0 && !AT.ok()) { umWARNING("CS<T>::SCC", "A^T invalid"); return NULL; }

  int *Blk=xi.data(), *pstack=xi.data() + n;
  int *rcopy = pstack; IVec &Dp=D->P, &Dr=D->R;
  int top = n, nb=0, i=0, k=0, b=0;
  
  for (i=0; i<n; ++i)   // first dfs(A) to find finish times (xi)
  {
    if (!CS_MARKED (P, i)) 
      top = CS_dfs (i, (*this), top, xi, pstack, NULL);
  }
  for (i=0; i<n; ++i) CS_MARK (P, i); // restore A; unmark all nodes
  
  top=n; 
  nb=n;
  for (k=0; k<n; ++k)       // dfs(A') to find strongly connnected comp
  {
    i = xi[k];              // get i in reverse order of finish times
    if (CS_MARKED (AT.P, i)) 
      continue;             // skip node i if already ordered
    Dr[nb--] = top;         // node i is the start of a component in D->p
    top = CS_dfs(i, AT, top, Dp, pstack, NULL);
  }
  Dr[nb] = 0;               // first block starts at zero; 
  for (k=nb; k<=n; ++k) 
    Dr[k-nb] = Dr[k];       // shift D->r up
  D->nb = nb = n-nb;        // nb = # of strongly connected components
  for (b=0; b<nb; ++b)      // sort each block in natural order
  {
    for (k=Dr[b]; k<Dr[b+1]; ++k) Blk[Dp[k]] = b;
  }
  for (b=0; b<=nb; ++b) rcopy[b] = Dr[b];
  for (i=0; i< n ; ++i) Dp[rcopy[Blk[i]]++] = i;
  return D;
}


//---------------------------------------------------------
template <typename T>
void CS<T>::spy(const char* msg) const
//---------------------------------------------------------
{
  if (!ok()) { umLOG(1, "spy(%s): empty matrix\n", msg); return; }

  if (this->is_csc()) {
    if (nzmax<2 && this->P[1]<1) {
      umLOG(1, "spy(%s): empty matrix\n", msg); 
    } else {
      umLOG(1, "spy(%s): (%5d,%5d)  nnz %8d   (csc form)\n", msg,m,n,nzmax);
    }
  } else {
    umLOG(1, "spy(%s): (%5d,%5d)  nnz %8d   (triplet)\n", msg,m,n,nz);
  }

  // TODO: dump as bitmap.
  // return;

  // limit 
  int MAX_spy_cols = 200;
  if (this->num_cols() > MAX_spy_cols) { return; }

  CS<T> C = trans(*this, m_values);
  int Nr = C.num_rows(), Nc = C.num_cols();
  printf("\nCS(%d,%d)\n", Nr,Nc);

  char buf[42]={'\0'}, buf2[42]={'\0'}, buf3[42]={'\0'};
  int di = (int) ceil(log10(double(Nr+1)));
  sprintf(buf, "Row[%%0%dd] ", di);
  
  if (Nc>20) { sprintf(buf2, "." ); sprintf(buf3, " " ); }
  else       { sprintf(buf2, ". "); sprintf(buf3, "  "); }

  bool brief = true;
  if (brief)
  {
    // ...
    for (int j=0; j<Nc; ++j) {
    //fprintf(stderr, buf, j+1);
      printf(buf, j+1);
      for (int i=0; i<Nr; ++i) 
      {
        if (C.val_idx(i,j) >= 0) {
        //fprintf(stderr, buf2);
           printf(buf2);
        } else {
        //fprintf(stderr, buf3);
          printf(buf3);
        }
      }
    //fprintf(stderr, "\n");
      printf("\n");
    }
  }
  else
  {
    // ...
    printf("TODO: spy( +details )\n");
  }
}


//---------------------------------------------------------
template <typename T>
CS<T>& CS<T>::symperm(const IVec& pinv, int values)
//---------------------------------------------------------
{
  // in-place permutation of (*this)
  // A = A(p,p) where A is symmetric 
  // upper part stored; uses pinv not p.

  if (!is_csc()) { umERROR("CS<T>::symperm", "not csc"); }
  if (!m_values) { values=0; }    // cannot do values if no X data
  CS<T> *C = new CS<T>(n, n, P[n], values, 0, OBJ_temp, "t.symperm"); // alloc result

  IVec w(n);     // workspace
  if (!C->ok() || !w.ok()) { umERROR("CS<T>::symperm", "out of memory"); }

  // check for (optional) permutation
  bool bp = (pinv.size() >= n);
  int i=0,i2=0,j=0,j2=0,p=0,q=0;

  for (j=0; j<n; ++j) {           // count entries in each column of C
    j2 = bp ? pinv[j] : j;        // column j of A is column j2 of C
    for (p=P[j]; p<P[j+1]; ++p) 
    {
      i = I[p];
      if (i>j) continue;          // skip lower triangular part of A
      i2 = bp ? pinv[i] : i;      // row i of A is row i2 of C
      w[std::max(i2,j2)]++ ;      // column count of C
    }
  }
  C->cumsum(w, n);                // compute column pointers of C
  for (j=0; j<n; ++j) {
    j2 = bp ? pinv[j] : j;        // column j of A is column j2 of C
    for (p=P[j]; p<P[j+1]; ++p) {
      i = I[p];
      if (i>j) continue;          // skip lower triangular part of A
      i2 = bp ? pinv[i] : i;      // row i of A is row i2 of C
      C->I[q = w[std::max(i2,j2)]++] = std::min(i2,j2);
      if (values) C->X[q] = X[p];
    }
  }
  (*this) = (*C);   // swap ownership of data
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& CS<T>::transpose(int values)
//---------------------------------------------------------
{
  // transpose this "in-place"

  if (!ok()) { return (*this); } // empty matrix
  if (!is_csc()) {umWARNING("CS<T>::transpose","expected csc"); return (*this);}
  if (m_values && !values) {X.resize(0);} // drop numeric data

  bool swap_mode = false;
  if (OBJ_temp == m_mode) {
    m_mode = OBJ_real;  // avoid prematue deletion of this
    swap_mode = true;   // during copy constructor for B
  }

  m_values = values;

  // copy current state, then transpose
  CS<T> B(*this);
  if (!B.ok()) { umERROR("CS<T>::transpose", "error making copy"); }

  int mm=B.m, nn=B.n;
  resize(nn, mm, B.P[nn], values, 0);  IVec w(mm);  // result + workspace
  if (!ok() || !w.ok()) { umERROR("CS<T>::transpose", "out of memory"); }
  int p=0, q=0, j=0;
  for (p=0; p<B.P[nn]; ++p) w[B.I[p]]++;  // row counts
  this->cumsum(w, mm);                    // row pointers
  for (j=0; j<nn; ++j) {
    for (p=B.P[j]; p<B.P[j+1]; ++p) {
      I[q=w[B.I[p]]++] = j;               // (j,i) <- B(i,j)
      if (values) X[q]=B.X[p];
    }
  }

  if (swap_mode) { m_mode=OBJ_temp; }  // restore "temp" mode
  return (*this);
}


//---------------------------------------------------------
template <typename T>
void CS<T>::write_ML(FILE* fp) const
//---------------------------------------------------------
{
  // Write as (Matlab-compatible) ASCII triplets.
  // Note that indices are adjusted to be 1-based.

  if (m<1 || n<1) { fprintf(fp,"SparseMatrix: *** EMPTY ***\n"); return; }
  if (!m_values)  { fprintf(fp,"SparseMatrix: *** SYMBOLIC ***\n"); return; }

  int i=0, j=0, p=0; double x=0.0;

#if (1)
  //#######################################################
  // FIXME: forcing correct dimensions
  // FIXME: check if Matlab sums duplicate entries
  //
  // sort_drop_dupl();
  //
  // If entry (m,n) is not in the sparsity pattern, 
  // we must output it to define "logical" size of 
  // the "triplet" matrix for Matlab.
  fprintf(fp,"%4d %4d  %25.15e\n", m, n, 0.0);
  //#######################################################
#endif

  if (is_triplet()) 
  {
    for (int k=0; k<nz; ++k) {
      i=1+I[k]; j=1+P[k]; x=(double)X[k];
    //fprintf(fp,"%4d %4d  %25.15e\n", i, j, x);
      fprintf(fp,"%4d %4d  %10.4lf\n", i, j, x); // Matlab-short
    }
  }
  else 
  {
    for (int col=0; col<n; ++col) {
      for (p=P[col]; p<P[col+1]; ++p) {
        i=1+I[p]; j=1+col; x=X[p];
      //fprintf(fp,"%4d %4d  %25.15e\n", i, j, x);
        fprintf(fp,"%4d %4d  %10.4lf\n", i, j, x); // Matlab-short
      }
    }
  }

  fprintf(fp,"\n");  // indicate end of data to Matlab
}


///////////////////////////////////////////////////////////
//
// CS<T> global operators
//
///////////////////////////////////////////////////////////


template <typename T> inline
Vector<T>& full(const CS<T>& A, const IVec& ids)
{
  // FIXME: currently only appropriate for 
  // a sparse matrix with 1 column

  // return a dense vector containing elements mapped by 
  // the ids index array.  If element A(ids(i),1) is not 
  // in the sparsity pattern of A, set that entry to 0.

  assert(A.is_csc());       // only implemented for compressed form
  if (A.num_cols() > 1) {   // this version: testing "sparse columns"
    umERROR("full(CS<T>&, IVec&)", "transitional code: matrix must have only 1 column");
  }

  // initialize dense vector with len zeros
  int len = ids.length(), id=0, idx=0;
  Vector<T> *tmp = new Vector<T>(len, "full(CS<T>)", OBJ_temp);
  for (int i=1; i<=len; ++i) {
    id = ids(i)-1;            // 0-based storage
    idx = A.val_idx(id, 0);     // (idx>=0) ==> A(id,1) non-zero
    if (idx >= 0) {
      (*tmp)(i) = A.X[idx];   // make this entry non-zero
    }
  }
  return (*tmp);
}




//---------------------------------------------------------
// Addition
//---------------------------------------------------------
// C = A + B
// C = A + x
// C = x + A
//---------------------------------------------------------

template <typename T> inline
CS<T>& operator+(const CS<T>& A, const CS<T>& B)
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"A+B",1); // deletes A (if temp)
  (*tmp) += B;                              // deletes B (if temp)
  return (*tmp);
}


template <typename T> inline 
CS<T>& operator+(const CS<T>& A, const T& x) 
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"A+x",1);
  (*tmp) += x;
  return (*tmp);
}


template <typename T> inline 
CS<T>& operator+(const T& x, const CS<T>& A) 
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"x+A",1);
  (*tmp) += x;
  return (*tmp);
}


//---------------------------------------------------------
// Subtraction
//---------------------------------------------------------
// C = A - B
// C = A - x
// C = x - A
//---------------------------------------------------------

template <typename T> inline
CS<T>& operator-(const CS<T>& A, const CS<T>& B)
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"A-B",1); // deletes A (if temp)
  (*tmp) -= B;                              // deletes B (if temp)
  return (*tmp);
}


template <typename T> inline 
CS<T>& operator-(const CS<T>& A, const T& x) 
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"A-x",1);
  (*tmp) -= x;
  return (*tmp);
}


template <typename T> inline 
CS<T>& operator-(const T& x, const CS<T>& A) 
{
  // FIXME: what operation should this perform?
  umERROR("operator-(x, CS<T>)", "Not implemented");

  CS<T> *tmp=new CS<T>(A,OBJ_temp,"x-A",1);
  (*tmp) -= x;
  return (*tmp);
}


//---------------------------------------------------------
// Multiplication
//---------------------------------------------------------
// C = A * B
//-----------------
// C = A * x
// C = x * A
//-----------------
// Y = A * DMat
//-----------------
// y = A * vec
// y = vec * A
//---------------------------------------------------------


template <typename T> inline 
CS<T>& operator*(const CS<T>& A, const CS<T>& B)
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"A*B",1); // deletes A (if temp)
  (*tmp) *= B;                              // deletes B (if temp)
  return (*tmp);
}

template <typename T> inline 
CS<T>& operator*(const CS<T>& A, const T& x) 
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"A*x",1);
  (*tmp) *= x;
  return (*tmp);
}

template <typename T> inline 
CS<T>& operator*(const T& x, const CS<T>& A) 
{
  CS<T> *tmp=new CS<T>(A,OBJ_temp,"x*A",1);
  (*tmp) *= x;
  return (*tmp);
}


inline  // specialization for <T>=<double>
DMat& operator*(const CSd& A, const DMat& cX) 
{
  // Y = A*X ... sparse * dense
  
  if (!A.ok())     { umERROR("CSd*DMat", "matrix is empty");}
  if (!A.is_csc()) { umERROR("CSd*DMat", "matrix must be in csc form");}

  // use a non-const reference
  DMat& X = const_cast<DMat&>(cX);

  int NrA=A.num_rows(), NcA=A.num_cols();
  int NrX=X.num_rows(), NcX=X.num_cols();
  assert(NrX == NcA);
  // allocate zero result
  DMat *Y=new DMat(NrA,NcX, "SP*X", OBJ_temp);

  // if the sparse mat is zero, return zero result
  if (A.is_zero()) { 
    return (*Y); 
  }

  DVec x("x"),y("y");
  for (int j=1; j<=NcX; ++j)
  {
    y.borrow(NrA, Y->pCol(j));  // jth column of result
    x.borrow(NrX, X.pCol(j));   // jth column of arg
    A.gaxpy(x, y);              // y += A*x
  }
  return (*Y);
}


template <typename T> inline 
Vector<T>& operator*(const CS<T>& A, const Vector<T>& x) 
{
  // y = A*x

  int Nr=A.num_rows(), Nc=A.num_cols();
  assert(x.size() == Nc);
  Vector<T> *tmp=new Vector<T>(Nr, 0.0, OBJ_temp, "SP*v");
  Vector<T> &y=(*tmp);  // use a reference (for syntax)

  if (A.get_shape() & sp_SYMMETRIC) 
  {
    // enable the operation when only one triangle 
    // of a symmetric matrix A is actually stored:
    assert(A.is_square()); double Aij=0.0;
    int n = A.num_cols(), i=0;
    for (int j=0; j<n; ++j) {
      for (int ip=A.P[j]; ip<A.P[j+1]; ++ip) {
        i = A.I[ip]; Aij = A.X[ip];
        y[i] += x[j]*Aij;           // set y[i]
        if (i != j) {
          y[j] += x[i]*Aij;         // set y[j]
        }
      }
    }
  } 
  else {
    A.gaxpy(x, y);    // y = A*x + 0
  }

  // delete temporaries
  if (A.get_mode()==OBJ_temp) { delete (&A); }
  if (x.get_mode()==OBJ_temp) { delete (&x); }

  return y;
}


template <typename T> inline 
Vector<T>& operator*(const Vector<T>& x, const CS<T>& A) 
{
  // y = x*A

  int Nr=A.num_rows(), Nc=A.num_cols();
  assert(x.size() == Nr);
  Vector<T> *tmp=new Vector<T>(Nc, 0.0, OBJ_temp, "v*SP");
  Vector<T> &y=(*tmp);  // use a reference (for syntax)
  A.gxapy(x, y);        // y = x*A + 0

  // delete temporaries
  if (A.get_mode()==OBJ_temp) { delete (&A); }
  if (x.get_mode()==OBJ_temp) { delete (&x); }

  return y;
}


//---------------------------------------------------------
// Division
//---------------------------------------------------------
// x = A | b    ... Matlab's  x = A\b
// x = A / b    ... ?
//---------------------------------------------------------

// implemented in file CS_Utils.cpp
DVec& operator | (const CSd& A, const DVec& b);
DMat& operator | (const CSd& A, const DMat& B);



///////////////////////////////////////////////////////////
//
// Miscellaneous global operators (add as required)
//
//---------------------------------------------------------
// find2D(A, op, val)   Matlab "find(...)"
// trans(A,  vals)      return A^T
// perm (A,p,vals)      return p(A)
// abs(A)               return abs(A)
// aA_plus_bB           C = alpha*A + beta*B
//---------------------------------------------------------


///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline 
IMat& find2D(const CS<T> &A, char op, T val)
//---------------------------------------------------------
{
  // Matlab "find": return both row and column info.
  // See also find2D(Mat_COL<T>,...), Vector<T>::find(...)
  //
  // Note: from sparse matrices, Matlab seems to return 
  // row indices in col 2, and column indices in col 1.

  IMat& tmp = A.find2D(op, val);
  // delete temp objects
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  return (tmp);
}



//---------------------------------------------------------
template <typename T> inline
CS<T>& trans(const CS<T> &A, int values)
//---------------------------------------------------------
{
  // return C = A'

  if (!A.ok())     { umERROR("trans(CS<T>)", "matrix is empty");}
  if (!A.is_csc()) { umERROR("trans(CS<T>)", "matrix must be in csc form");}
  int do_values = (values && A.m_values) ? 1 : 0;
  int m=A.m, n=A.n, p=0, q=0, j=0;  IVec w(m);
  CS<T>* C = new CS<T>(n, m, A.P[n], do_values, 0, OBJ_temp, "t.trans");
  if (!C->ok()||!w.ok()) {umERROR("trans(CS<T>)", "out of memory");}
  for (p=0; p<A.P[n]; ++p) w[A.I[p]]++; // count elements in each row
  C->cumsum(w, m);                      // prepare row pointers in C
  for (j=0; j<n; ++j) {
    for (p=A.P[j]; p<A.P[j+1]; ++p) {
      C->I[q=w[A.I[p]]++] = j;          // place A(i,j) as entry C(j,i)
      if (do_values) C->X[q]=A.X[p];
    }
  }
  return (*C);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& trans2(const CS<T> &A, int values)
//---------------------------------------------------------
{
  static char buf[100]={""}; 
  snprintf(buf, (size_t)90, "trans(%s)", A.name()); 

  // Copy constructor deletes A (if temp)
  CS<T>* At = new CS<T>(A, OBJ_temp, buf, values);
  At->transpose(values);
  return (*At);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& perm(const CS<T> &A, const IVec& pinv, const IVec& q, int values)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "perm(%s)", A.name()); 

  // Copy constructor deletes A (if temp)
  CS<T>* tmp = new CS<T>(A, OBJ_temp, buf, values);
  tmp->permute(pinv, q, values);
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
CS<T>& abs(const CS<T> &A)
//---------------------------------------------------------
{
  // return abs(A)
  CS<T>* Z = new CS<T>("t.abs", OBJ_temp);
  if (!A.ok())     { umWARNING("abs(CS<T>)", "matrix is empty"); return (*Z); }
  if (!A.m_values) { umWARNING("abs(CS<T>)", "matrix is sybolic (numeric data not stored)"); return (*Z); }

  CS<T>* C = new CS<T>(A, OBJ_temp, "abs(A)", 1);
  if (!C->ok()) {umERROR("abs(CS<T>)", "out of memory");}
  C->X.set_abs();
  return (*C);
}


// C = alpha*A + beta*B
//---------------------------------------------------------
template <typename T> inline
CS<T>& aA_plus_bB
(
  const T& alpha, const CS<T>& A, 
  const T& beta,  const CS<T>& B
)
//---------------------------------------------------------
{
  // C = alpha*A + beta*B

  if (!A.is_csc() || !B.is_csc() || !A.is_compatible(B)) {
    umERROR("CS<T>: aA_plus_bB", "Both args must be csc"); 
  }

#if (0)

  //#######################################################
  CS<T> *C = new CS<T>(A, OBJ_temp, "a*A+b*B");
  (*C) *= alpha;
  (*C) += (beta*B);
  return (*tmp);
  //#######################################################

#else

  //#######################################################
  int m=A.m, n=B.n, anz=A.P[A.n], bnz=B.P[B.n]; // num. nonzeros
  int values = (A.m_values && B.m_values)?1:0;  // do values?
  IVec w(m);  Vector<T> x(values?m:0);          // workspaces
  CS<T> *C = new CS<T>(m,n,anz+bnz,values,0,OBJ_temp, "aA_plus_bB"); // result
  if (!C->ok() || !w.ok() || (values && !x.ok())) { umERROR("aA_plus_bB", "out of memory."); }
  int p=0, Nz=0;
  for (int j=0; j<n; ++j) {
    C.P[j] = Nz;     // column j of C starts here
    Nz = A.scatter(j, alpha, w, x, j+1, (*C), Nz);  // alpha*A(:,j)
    Nz = B.scatter(j, beta,  w, x, j+1, (*C), Nz);  // beta *B(:,j)
    if (values) for (p=C.P[j]; p<Nz; ++p) C.X[p] = x[C.I[p]];
  }
  C.P[n] = Nz;      // finalize the last column of C
  C.realloc(0);     // remove extra space from C
  assert(C->ok());
  return (*C);
  //#######################################################

#endif
}


//---------------------------------------------------------
class CSS   // CS Symbolic
//---------------------------------------------------------
{
public:

  IVec  pinv;     // inverse row perm. for QR, fill red. perm for Chol
  IVec  Q;        // fill-reducing column permutation for LU and QR
  IVec  parent;   // elimination tree for Cholesky and QR
  IVec  cp;       // column pointers for Cholesky, row counts for QR
  IVec  leftmost; // leftmost[i] = min(find(A(i,:))), for QR
  int   m2;       // # of rows for QR, after adding fictitious rows
  double lnz;     // # entries in L for LU or Cholesky; in V for QR
  double unz;     // # entries in U for LU; in R for QR
  int   m_mode;   // {OBJ_real,OBJ_temp}

public:
  CSS();
  ~CSS();

  void Free();
  void show_alloc() const;
  int  get_mode() const    { return m_mode; }
  void set_mode(int mode)  { m_mode = mode; }
  bool ok() const;

};


//---------------------------------------------------------
class CSN   // CS Numeric
//---------------------------------------------------------
{
public:

  CSd   L;      // L for LU and Cholesky, V for QR
  CSd   U;      // U for LU, R for QR, not used for Cholesky
  IVec  pinv;   // partial pivoting for LU
  DVec  B;      // beta [0..n-1] for QR
  CSd *C, *E;   // pointers
  int   m_mode; // {OBJ_real,OBJ_temp}

public:
  CSN();
  ~CSN();
  
  void Free();
  void show_alloc() const;
  int  get_mode() const    { return m_mode; }
  void set_mode(int mode)  { m_mode = mode; }
  bool ok() const;
};


//---------------------------------------------------------
class CS_Chol
//---------------------------------------------------------
{
public:
  
  CS_Chol();
  ~CS_Chol();

  // Perform Cholesky factorization using 
  // selected AMD re-ordering mode:
  int chol(CSd& A, int order=1, double dummy=0.0);

  // use factored form to solve for rhs, return x=A\rhs
  DVec& solve(const DVec& rhs);

  // factor and solve for rhs, return x=A\rhs
  DVec& chol_solve(int order, CSd& A, DVec& rhs);

protected:
  CSS  *S;        // symbolic info
  CSN  *N;        // numeric data
  DVec b, x;      // rhs, solution
};


//---------------------------------------------------------
class CS_LU
//---------------------------------------------------------
{
public:
 
  CS_LU();
  ~CS_LU();

  CSS*  get_S() { return this->S; }  // symbolic info
  CSN*  get_N() { return this->N; }  // numeric data
  DVec& get_b() { return this->b; }  // rhs
  DVec& get_x() { return this->x; }  // solution

  // Perform LU factorization using appropriate
  // AMD re-ordering: 0:natural, 1:Chol, 2:LU, 3:QR
  // tol: partial pivoting "scale factor"
  int lu(const CSd& A, int order=2, double tol=1.0);

  // use factored form to solve for rhs, return x=A\rhs
  DVec& solve(const DVec& rhs);

  // use factored form to solve for MULTIPLE rhs's, return X=A\RHS
  DMat& solve(const DMat& RHS);

protected:
  CSS  *S;        // symbolic info
  CSN  *N;        // numeric data
  DVec b, x;      // rhs, solution
};


//---------------------------------------------------------
class CS_QR
//---------------------------------------------------------
{
public:
  CS_QR() {}
  ~CS_QR() {}

  bool ok() const { return true; }
};


/*
//---------------------------------------------------------
class CS_Cholinc
//---------------------------------------------------------
{
public:
  
  CS_Cholinc() {}
  ~CS_Cholinc() {}

  // Perform Cholesky factorization using 
  // selected AMD re-ordering mode:
  int cholinc(CSd& A, double droptol=1e-3, int order=1);

protected:
  CSd   L;      // lower triangular factor: A = L*L'
};
*/


//---------------------------------------------------------
class CS_PCG
//---------------------------------------------------------
{
public:
  
  CS_PCG() 
    : m_droptol(1e-3), m_tol(1e-6), m_maxit(20), 
      m_verbose(true), m_factor(false), m_oldsol(false) {}

  ~CS_PCG() {}

  // create incomplete Cholesky preconditioner
  int cholinc(CSd& A, double droptol=1e-3);

  // Use a preconditioned Conjugate Gradient method 
  // to return an iterative solution to: x = A\rhs.
  DVec& solve(const DVec& rhs, double tol=1e-6, int maxit=20);
  DVec& solve_LLT(const DVec& rhs); // x <- [LL']\rhs.

  // adjust options for incomplete factorization
  void set_droptol(double dtol) { m_droptol = dtol; }

  // adjust options for pcg solver
  void set_tol(double tol)      { m_tol = tol;      }
  void set_maxit(int maxit)     { m_maxit = maxit;  }
  void set_verbose(bool verb)   { m_verbose = verb; }
  
  // get solver results
  DVec&   get_x()             { return x; }
  int     get_flag() const    { return m_flag;   }
  double  get_relres() const  { return m_relres; }
  int     get_iter() const    { return m_iter;   }
  DVec&   get_resvec()        { return m_resvec; }

protected:
  // the system -------------------------
  CSd  A;             // symmetric pos.def system to solve
  CSd  L;             // cholinc() preconditioner
  DVec pb, px, x;     // permuted rhs, permuted sol, sol.
  DVec prec_x;        // solution from preconditioner 
  IVec perm, pinv;    // permutations
  // parameters -------------------------
  double  m_droptol;  // [in] factorization drop-tol
  double  m_tol;      // [in] solution convergence tol
  int     m_maxit;    // [in] max iterations before abort
  bool    m_verbose;  // [in] adjust log output
  bool    m_factor;   // [in] cholinc factor exists
  bool    m_permute;  // [in] previous solution exists
  bool    m_oldsol;   // [in] previous solution exists
  // result info ------------------------
  int     m_flag;     // [out]  status info
  double  m_relres;   // [out] |B-A*X|/|B|  relative residual
  int     m_iter;     // [out]  num. iterations used 0<=iter<=MAXIT
  DVec    m_resvec;   // [out]  vector of |resid| at each iteration
  //-------------------------------------
};


// TODO: gmres
//---------------------------------------------------------
class CS_GMRES
//---------------------------------------------------------
{
public:
  
  CS_GMRES() {}
  ~CS_GMRES() {}

  // create incomplete LU preconditioner
  int luinc(CSd& A, int order=2, double droptol=1e-6);

  // Use a preconditioned GMRES method 
  // to return an iterative solution to: x = A\rhs.
  DVec& solve(const DVec& rhs);

protected:
  CSd  A;           // unsymmetric system to solve
  CSd  L,U;         // luinc() preconditioner
  DVec b, x;        // rhs, solution
  IVec perm, pinv;  // permutations
};


#endif // NDG__CS_Type_H__INCLUDED
