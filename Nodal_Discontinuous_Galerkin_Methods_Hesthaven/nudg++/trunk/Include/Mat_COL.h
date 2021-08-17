// Matrix_COL.h
// Matlab-compatible dense matrix
// 1-based (i,j) indexing
// 2007/10/16
//---------------------------------------------------------
#ifndef NDG__Matrix_COL_H__INCLUDED
#define NDG__Matrix_COL_H__INCLUDED

#include "Vec_Type.h"
#include "Region2D.h"
#include "VecSort_Type.h"

// typedef versions for common data types
template <typename T> class Mat_COL;

typedef Mat_COL<double>  DMat;
typedef Mat_COL<dcmplx>  ZMat;
typedef Mat_COL<int>     IMat;
typedef Mat_COL<long>    LMat;
typedef Mat_COL<bool>    BMat;

// add forward declarations (as necessary)
//
// C = A * B
void umAxB(const DMat& A, const DMat& B, DMat& C);
void umAxB(const ZMat& A, const ZMat& B, ZMat& C);

// bool  chol_solve(const DMat& ch, const DMat& B, DMat& X);
// DVec& chol_solve(const DMat& ch, const DVec& b);



//---------------------------------------------------------
template <typename T> 
class Mat_COL : public virtual Vector<T>
//---------------------------------------------------------
{
  //
  // Member data
  //
protected:

  int   m_M;    // num rows
  int   m_N;    // num cols
  int   m_MN;   // num elements = (m*n)

  T**   col_;   // 1-based data pointers, adjusted 
                // to enable 1-based (i,j) indexing

  fact_type   m_fact_mode;  // factorization scheme
  int*        m_ipiv;       // pivot data for LU factorization
  
  double  m_norm_1,         // max col sum
          m_norm_inf,       // max row sum
          m_norm_frob;      // sqrt(sum(squares))

public:

  // typedef's for std::compatibility
  typedef     int   size_type;
  typedef       T   m_data_type;
  typedef       T*  pointer;
  typedef       T*  iterator;
  typedef       T&  reference;
  typedef const T*  const_iterator;
  typedef const T&  const_reference;

  // constructors
  explicit Mat_COL(const char* sz="mat", OBJ_mode md=OBJ_real);
           Mat_COL(const Mat_COL<T> &B, OBJ_mode md=OBJ_real, const char* sz="mat");
  explicit Mat_COL(int M, int N, const char* sz, OBJ_mode md=OBJ_real);
  explicit Mat_COL(int M, int N, const T x=T(0), OBJ_mode md=OBJ_real, const char* sz="mat");
  explicit Mat_COL(int M, int N, const T *data, OBJ_mode md=OBJ_real, const char* sz="mat");
  explicit Mat_COL(const ArrayData& rAD, int M, int N, const char *sdata, OBJ_mode md=OBJ_real, const char* sz="mat");

  // destructor
  virtual ~Mat_COL();
  virtual void destroy();
  virtual void Free();

  // manage allocation
  Mat_COL<T>& borrow(int M, int N, T* p);
  void lend_col(int N, Vector<T> &col);
  void set_pointers(int M, int N);
  bool resize(int M, int N, bool bInit=true, T x=T(0));         // reinit to zeros(M,N)
  bool resize(const Mat_COL<T>& B, bool bInit=true, T x=T(0));  // reinit to zeros(M,N)
  bool realloc(int newM, int newN, bool bInit=true, T x=T(0));  // map col-data onto new shape
  bool reshape(int newM, int newN, bool bInit=true, T x=T(0));  // wrap data into new shape
  bool compatible(const Mat_COL<T>& B) const;

  void append_col(const Vector<T>& V);
  void append_row(const Vector<T>& V);

  void append_cols(const Mat_COL<T>& B);
  void append_rows(const Mat_COL<T>& B);
  void append_rows(int Nr, int Nc=1);
  void merge_rows(IVec map, Mat_COL<T>& B);

  // copy/load
  Mat_COL<T>& copy(const T *vec);
  Mat_COL<T>& copy(const Mat_COL<T>& B);
  void load(int M, int N, const T *vdata);
  void load(int M, int N, const Vector<T>& V);
  void load_col (int j, int Nr, const T* pdata);
  void load_rows(int M, int N, const char *sdata);
  void load_cols(int M, int N, const char *sdata);

  // assignment
  Mat_COL<T>& operator=(const T &val);
  Mat_COL<T>& operator=(const Mat_COL<T> &B);
  Mat_COL<T>& operator=(const Vector<T> &V);
  // allow assignment of IMat to DMat
  Mat_COL<T>& assign(const IMat &B);


  // Utilities
  bool  ok() const       { return ((this->v_!=NULL) && (m_M>0) && (m_N>0)); }
  int   size() const     { return this->m_Len; }
  void  size(int& M, int& N) const { M=m_M; N=m_N; }
  bool  is_square() const { return (this->ok() && (m_M==m_N)); }
  int   lbound ()  const { return 1; } // base for operator(i,j)
  int   dim(int d) const { return (d==1)?m_M:((d==2)?m_N:0); }
  int   num_rows() const { return m_M; }
  int   num_cols() const { return m_N; }
  int     max_mn() const { return (m_M > m_N) ? m_M : m_N; }
  int     min_mn() const { return (m_M < m_N) ? m_M : m_N; }

  // "mode".  See also Vector<T>::m_mode
  void  set_factmode(fact_type fm)  { m_fact_mode = fm; }
  fact_type get_factmode() const    { return m_fact_mode; }
  bool  is_factored() const         { return (FACT_NONE==m_fact_mode)?false:true;}
  int*  get_pivots() const          { return m_ipiv; } 
  void  set_pivots(int* ipiv)   {if(m_ipiv){umIVectorFree(m_ipiv);} m_ipiv=ipiv;}

  Mat_COL<T>& zeros()             {Vector<T>::fill(T(0)); return (*this);} 
  Mat_COL<T>& zeros(int M, int N) {resize(M,N,true,T(0)); return (*this);} 

  Mat_COL<T>& ones()              {Vector<T>::fill(T(1)); return (*this);} 
  Mat_COL<T>& ones(int M, int N)  {resize(M,N,true,T(1)); return (*this);} 

  Mat_COL<T>& fill(const T &x)    {Vector<T>::fill(x);    return (*this);} 
  Mat_COL<T>& set_abs()           {Vector<T>::set_abs();  return (*this);} 

  Mat_COL<T>& operator!() const;
  Mat_COL<T>& identity(int N);
  Mat_COL<T>& diag(const Vector<T>& d);
  Mat_COL<T>& set_diag(int i, const Vector<T>& d, bool MLmode=false);
  Mat_COL<T>& set_diags(const Mat_COL<T>& B, const IVec& d, int m, int n);

  Mat_COL<T>& transpose();
  Mat_COL<T>& hilbert(int N);

  // Comparison  ("fuzzy", uses m_EqTol)
  bool operator==(const Mat_COL<T> &B) const;
  bool operator!=(const Mat_COL<T> &B) const { return (!operator==(B));}

  //-------------------------------------
  // return "Boolean" results
  //-------------------------------------
  Mat_COL<T>& eq    (T val) const;          // R = (BCType==Wall)
  Mat_COL<T>& eq    (Mat_COL<T>& B) const;  // R = (EToE==outer(Range,Ones))

  Mat_COL<T>& le    (T val) const;   // R = (U <= val)
  Mat_COL<T>& lt    (T val) const;   // R = (U <  val)
  Mat_COL<T>& lt_abs(T val) const;   // R = (U <  tol)

  Mat_COL<T>& ge    (T val) const;   // R = (U >= val)
  Mat_COL<T>& gt    (T val) const;   // R = (U >  val)
  Mat_COL<T>& gt_abs(T val) const;   // R = (U >  tol)


  // MAX row/col vals
  T max_row_val    (const int i) const;
  T max_row_val_abs(const int i) const;
  T max_col_val    (const int j) const;
  T max_col_val_abs(const int j) const;

  // MIN row/col vals
  T min_row_val    (const int i) const;
  T min_row_val_abs(const int i) const;
  T min_col_val    (const int j) const;
  T min_col_val_abs(const int j) const;

  // matrix norms
  double norm1() const;       // max column sum
  double norm_inf() const;    // max row sum
  double norm_frob() const;   // frobenius-norm

  // Element-by-element operations.

  // The first set update (*this) in place
  Mat_COL<T>&  mult_element(const Mat_COL<T> &B);
  Mat_COL<T>&  mult_element(const Vector<T>& b);
  Mat_COL<T>&  mult_element(const T* data);
  Mat_COL<T>&  div_element (const Mat_COL<T> &B);
  Mat_COL<T>&  div_element (const Vector<T>& b);

  // The following variations do not change (*this)
  Mat_COL<T>&  dd(const Mat_COL<T> &B) const;
  Mat_COL<T>&  dm(const Mat_COL<T> &B) const;
  Mat_COL<T>&  dm(const Vector<T> &V) const;
  Mat_COL<T>&  dm(const T* data) const;


  //-------------------------------------
  // numerical routines
  //-------------------------------------

  // apply scalar to all elements
  void add_val (const T &x) { Vector<T>::add_val (x); }
  void mult_val(const T &x) { Vector<T>::mult_val(x);}
  void div_val (const T &x) { Vector<T>::div_val (x); }
  void pow_val (const T &x) { Vector<T>::pow_val (x); }
  
  //-------------------------------------
  // Overloaded arithmetic operators.
  // Note: pattern for global operators,
  // tmp(A);  tmp+=x;  return tmp;
  //-------------------------------------
  Mat_COL<T>& operator += (const T &x) {add_val ( x); return (*this);}
  Mat_COL<T>& operator -= (const T &x) {add_val (-x); return (*this);}
  Mat_COL<T>& operator *= (const T &x) {mult_val( x); return (*this);}
  Mat_COL<T>& operator /= (const T &x) {div_val ( x); return (*this);}


  // element-by-element operations (A.+B) all call 
  // unrolled vector version (deleting arg if temp)
  //
  // A.+B, A.+V, A.-B, A.-V
  Mat_COL<T>& operator+=(const Mat_COL<T> &B) {Vector<T>::operator+=((const Vector<T>&)B);return(*this);}
  Mat_COL<T>& operator+=(const Vector <T> &V) {Vector<T>::operator+=(V);return(*this);}
  Mat_COL<T>& operator-=(const Mat_COL<T> &B) {Vector<T>::operator-=((const Vector<T>&)B);return(*this);}
  Mat_COL<T>& operator-=(const Vector <T> &V) {Vector<T>::operator-=(V);return(*this);}

  Mat_COL<T>& operator*=(const Mat_COL<T> &A);  // matrix multiplication
  Mat_COL<T>& operator/=(const Mat_COL<T> &A);  // mrdivide(B,A): B/A => B*inv(A)
  Mat_COL<T>& operator|=(const Mat_COL<T> &A);  // mldivide(A,B): A\B => inv(A)*B

  Mat_COL<T>& invert();

  void zero_below_diag();   // helper for chol()
  Mat_COL<T>& fact_Chol();
  void solve_Chol(const Mat_COL<T>& B, Mat_COL<T>& X);
  void solve_Chol(const Vector<T>&  b, Vector<T>&  x);

  Mat_COL<T>& fact_LU();
  double solve_LU(const Mat_COL<T>& B, Mat_COL<T>& X, bool bTrans=false, bool bCond=false);
  double solve_LU(const Vector<T>&  b, Vector<T>&  x, bool bTrans=false, bool bCond=false);


  //-------------------------------------
  // element access
  //-------------------------------------

  // single (1-based) index into base array
        T& operator()(int i)       { return Vector<T>::operator()(i); }
  const T& operator()(int i) const { return Vector<T>::operator()(i); }

  // 1-based element access (make "virtual" if required)
        T& operator()(int i, int j)       {CheckIdx_IJ_1(i,j); return col_[j][i];}
  const T& operator()(int i, int j) const {CheckIdx_IJ_1(i,j); return col_[j][i];}

  // return pointer to (1-based) col j
        T* operator[](int j)       {CheckIdx_Col_1(j); return col_[j];}
  const T* operator[](int j) const {CheckIdx_Col_1(j); return col_[j];}

  // return pointer to data in col j
        T* pCol(int j)       {CheckIdx_Col_1(j); return col_[j]+1;}
  const T* pCol(int j) const {CheckIdx_Col_1(j); return col_[j]+1;}

  // return T** array of (1-based) col pointers
        T** COLS()       { return col_ ; }
  const T** COLS() const { return col_ ; }



  //---------------------------------------------
  // set/get entire rows/cols
  //---------------------------------------------

  // NOTE: return value must be assigned or deleted!
  Vector<T>&  get_row(const int i) const;
  Vector<T>&  get_col(const int j) const;
  
  void set_row(const int i, const T &x);
  void set_row(const int i, const Vector<T> &vec);
  void set_col(const int j, const T &x);
  void set_col(const int j, const Vector<T> &vec);
  void add_col(const int j, const Vector<T> &vec);
  void sub_col(const int j, const Vector<T> &vec);
  void extend_col(const int ncol);

  Vector<T>&  col_sums() const;
  Vector<T>&  col_sums_abs() const;
  Vector<T>&  row_sums() const;

  Vector<T>&  max_col_vals_abs() const;
  Vector<T>&  max_col_vals() const;
  Vector<T>&  max_row_vals() const;
  
  Vector<T>&  min_col_vals_abs() const;
  Vector<T>&  min_col_vals() const;
  Vector<T>&  min_row_vals() const;

  Mat_COL<T>& reverse_rows();
  Mat_COL<T>& sort_cols(const IVec& idx);
  Mat_COL<T>& sort(int dim=1);

  void scale_col(const int j, const double &x);
  void scale_col(const int j, const Vector<T> &vec);

  // Kronecker tensor product.
  Mat_COL<T>& kron(const Mat_COL<T>& B) const;
  void setKronBlock(int row, int col, const Mat_COL<T>& B);

public:
  
  //-------------------------------------
  // Adjustable output format.
  //-------------------------------------

  void print (
    FILE* os = stdout, 
    const char* msg=NULL,
    const char* fmt="lf",  // [%e|%lf|%g]
    int  prec=4,    // sig.figs|dec.places
    int  wdth=8,    // output spacing [12]
    int  nline=100, // entries per line
    int  nr=0,      // num rows to write (0 --> all)
    int  nc=0       // num cols to write (0 --> all)
  ) const;

  void print_STREAM (
    std::ostream& os, 
    const char* msg=NULL,
    char fmt='G',   // [%e|%lf|%g]
    int  prec=4,    // sig.figs|dec.places
    int  wdth=12,   // output spacing [12]
    int  nline=100, // entries per line
    int  nr=0,      // num rows to write (0 --> all)
    int  nc=0       // num cols to write (0 --> all)
  ) const;

  // Formatted for Gnuplot
  void print_GP (
    std::ostream& os, 
    int prec=4,         // sig. figs      [ 4] ios::scientific
    int wdth=12,        // output spacing [12]
    int nr=0,           // num rows to write (0 --> all)
    int nc=0) const;    // num cols to write (0 --> all)



  // Save matrix in Matlab ".mat" format (BINARY)
  // Note: For Win32, fp MUST be opened in "wb" mode
  void m_save(FILE* fp, const char* name) const;
  void m_load(FILE *fp, std::string& name);


  //
  // Optional index checking
  //
protected:

#if (CHECK_ARRAY_INDEX)

  // 0-based indices
  void CheckIdx_IJ_0 (int i, int j) const { if (i < 0) throw 1;  if (i >= m_M) throw 2;
                                            if (j < 0) throw 3;  if (j >= m_N) throw 4;}
  void CheckIdx_Row_0(int i) const {if (i<0) throw 1; if (i>=m_M)  throw 2;}
  void CheckIdx_Col_0(int j) const {if (j<0) throw 1; if (j>=m_N)  throw 2;}
  void CheckIdx_Len_0(int k) const {if (k<0) throw 1; if (k>=m_MN) throw 2;}

  // 1-based indices
  void CheckIdx_IJ_1 (int i, int j) const { if (i < 1) throw 1;   if (i > m_M) throw 2;
                                            if (j < 1) throw 3;   if (j > m_N) throw 4;}
  void CheckIdx_Row_1(int i) const {if (i<1) throw 1; if (i>m_M)  throw 2;}
  void CheckIdx_Col_1(int j) const {if (j<1) throw 1; if (j>m_N)  throw 2;}
  void CheckIdx_Len_1(int k) const {if (k<1) throw 1; if (k>m_MN) throw 2;}

#else

  // 0-based indices
  void CheckIdx_IJ_0(int i, int j) const {}
  void CheckIdx_Row_0(int i) const {}
  void CheckIdx_Col_0(int j) const {}
  void CheckIdx_Len_0(int k) const {}

  // 1-based indices
  void CheckIdx_IJ_1(int i, int j) const {}
  void CheckIdx_Row_1(int i) const {}
  void CheckIdx_Col_1(int j) const {}
  void CheckIdx_Len_1(int k) const {}

#endif



  //-------------------------------------------------------
  // member functions involving a subset of the matrix
  //-------------------------------------------------------
public:

  Vector<T>&  get_map(const IVec& map) const;
  Vector<T>&  get_map(const IVec& map, int j) const;
  Vector<T>&  get_map(int i, const IVec& map) const;

  Mat_COL<T>& get_map(const IMat& map) const;
//Mat_COL<T>& operator()(const IMat& map) const;

  Mat_COL<T>& set_map(const IMat& map, const T x);
  Mat_COL<T>& set_map(const IMat& map, const Vector<T>& X);

  Mat_COL<T>& concat_v(const Mat_COL<T>& A1, const Mat_COL<T>& A2);
  Mat_COL<T>& concat_v(const Mat_COL<T>& A1, const Mat_COL<T>& A2, const Mat_COL<T>& A3);


  //-------------------------------------------------------
  // operations involving (contiguous) Region1D
  //-------------------------------------------------------
public:
  Mat_COL(const       Region2D< Mat_COL<T> > &R, OBJ_mode md=OBJ_real, const char *sz="mat");
  Mat_COL(const const_Region2D< Mat_COL<T> > &R, OBJ_mode md=OBJ_real, const char *sz="mat");

  const_Region2D< Mat_COL<T> >  operator()(const Index1D &I, const Index1D &J) const;
        Region2D< Mat_COL<T> >  operator()(const Index1D &I, const Index1D &J);

  const_Region1D< Vector <T> >  operator()(const Index1D &I, int j) const;
        Region1D< Vector <T> >  operator()(const Index1D &I, int j);

  const_Region1D< Vector <T> >  operator()(const Index1D &I) const;
        Region1D< Vector <T> >  operator()(const Index1D &I);

  const_Region1D< Vector <T> >  operator()(const MatDimension&, int j) const;
        Region1D< Vector <T> >  operator()(const MatDimension&, int j);

  const_Region2D< Mat_COL<T> >  operator()(int i, const MatDimension&) const;
        Region2D< Mat_COL<T> >  operator()(int i, const MatDimension&);

  Mat_COL<T>& operator=(const       Region2D< Mat_COL<T> > &B);
  Mat_COL<T>& operator=(const const_Region2D< Mat_COL<T> > &B);
  Mat_COL<T>& operator=(const       Region1D< Vector<T> > &R);
  Mat_COL<T>& operator=(const const_Region1D< Vector<T> > &R);

  Mat_COL<T>& mult_element(const Region2D< Mat_COL<T> > &R);
  Mat_COL<T>& div_element (const Region2D< Mat_COL<T> > &R);

  Mat_COL<T>& operator+=(const Region2D< Mat_COL<T> > &R);
  Mat_COL<T>& operator-=(const Region2D< Mat_COL<T> > &R);
  //-------------------------------------------------------


  //-------------------------------------------------------
  // operations involving MappedRegion2D (and 1D)
  //-------------------------------------------------------
public:
  Mat_COL(const       MappedRegion2D< Mat_COL<T> > &B, OBJ_mode md=OBJ_real, const char *sz="mat");
  Mat_COL(const const_MappedRegion2D< Mat_COL<T> > &B, OBJ_mode md=OBJ_real, const char *sz="mat");


  const_MappedRegion2D< Mat_COL<T> > operator()(const Region1D< Vector<int> > &Ri, int j) const;
        MappedRegion2D< Mat_COL<T> > operator()(const Region1D< Vector<int> > &Ri, int j);
  const_MappedRegion2D< Mat_COL<T> > operator()(const IVec &I, int j) const;
        MappedRegion2D< Mat_COL<T> > operator()(const IVec &I, int j);


  const_MappedRegion2D< Mat_COL<T> > operator()(int i, const Region1D< Vector<int> > &Rj) const;
        MappedRegion2D< Mat_COL<T> > operator()(int i, const Region1D< Vector<int> > &Rj);
  const_MappedRegion2D< Mat_COL<T> > operator()(int i, const IVec &J) const;
        MappedRegion2D< Mat_COL<T> > operator()(int i, const IVec &J);


  const_MappedRegion1D< Vector<T> > operator()(const IVec &I) const;
        MappedRegion1D< Vector<T> > operator()(const IVec &I);

  // Map subset of rows
  const_MappedRegion2D< Mat_COL<T> > operator()(const IVec &I,     const MatDimension& MD) const;
        MappedRegion2D< Mat_COL<T> > operator()(const IVec &I,     const MatDimension& MD);
  const_MappedRegion2D< Mat_COL<T> > operator()(const Index1D &ID, const MatDimension& MD) const;
        MappedRegion2D< Mat_COL<T> > operator()(const Index1D &ID, const MatDimension& MD);

  // Map subset of columns
  const_MappedRegion2D< Mat_COL<T> > operator()(const MatDimension& MD, const IVec &J) const;
        MappedRegion2D< Mat_COL<T> > operator()(const MatDimension& MD, const IVec &J);

  //-------------------------------------
  // Generalized 2D mapping
  //-------------------------------------
  const_MappedRegion2D< Mat_COL<T> > operator()(const IVec &I, const IVec &J) const;
        MappedRegion2D< Mat_COL<T> > operator()(const IVec &I, const IVec &J);

  const_MappedRegion2D< Mat_COL<T> > operator()(const IVec &I, const Index1D &IDX) const;
        MappedRegion2D< Mat_COL<T> > operator()(const IVec &I, const Index1D &IDX);

  const_MappedRegion2D< Mat_COL<T> > operator()(const Index1D &IDX, const IVec &J) const;
        MappedRegion2D< Mat_COL<T> > operator()(const Index1D &IDX, const IVec &J);

  Mat_COL<T>& operator=(const const_MappedRegion2D< Mat_COL<T> > &R);
  Mat_COL<T>& operator=(const       MappedRegion2D< Mat_COL<T> > &R);

  Mat_COL<T>& operator=(const const_MappedRegion1D< Vector<T> > &R);
  Mat_COL<T>& operator=(const MappedRegion1D< Vector<T> > &R);


  Mat_COL<T>& dm(const MappedRegion2D< Mat_COL<T> > &R) const;
  Mat_COL<T>& dd(const MappedRegion2D< Mat_COL<T> > &R) const;

  Mat_COL<T>& mult_element(const MappedRegion2D< Mat_COL<T> > &R);
  Mat_COL<T>& div_element (const MappedRegion2D< Mat_COL<T> > &R);

  Mat_COL<T>& operator+=(const MappedRegion2D< Mat_COL<T> > &R);
  Mat_COL<T>& operator-=(const MappedRegion2D< Mat_COL<T> > &R);
  //-------------------------------------------------------


};


///////////////////////////////////////////////////////////
//
// constructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const char* sz, OBJ_mode md)
//---------------------------------------------------------
: Vector<T>(sz, md), 
  m_M(0), m_N(0), m_MN(0), col_(0),
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{}


// not "explicit"; allows construction from return objects
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const Mat_COL<T> &B, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(sz, md), 
  m_M(0), m_N(0), m_MN(0), col_(0),
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  int Nr = B.m_M, Nc = B.m_N;
  this->m_mode = md;

  // manage copy of real/temp objects
  // deletes B, if temporary.
  Vector<T>::operator= ((const Vector<T>&) B);

  if (Nr>0 && Nc>0) {
    set_pointers(Nr,Nc);
  }
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(int M, int N, const char* sz, OBJ_mode md)
//---------------------------------------------------------
: Vector<T>(M*N, sz, md),
  m_M(0), m_N(0), m_MN(0), col_(0),
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  set_pointers(M,N);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(int M, int N, const T x, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(M*N, x, md, sz),
  m_M(0), m_N(0), m_MN(0), col_(0),
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  set_pointers(M,N);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(int M, int N, const T *data, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(M*N, data, md, sz), 
  m_M(0), m_N(0), m_MN(0), col_(0), 
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  set_pointers(M,N);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const ArrayData& rAD, int M, int N, const char *sdata, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(M*N, T(0), md, sz),
  m_M(0), m_N(0), m_MN(0), col_(0), 
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  set_pointers(M,N);

  try 
  {
    std::istringstream ins(sdata);

    if (SZ_DATA_MAT_ROWS==rAD.mode()) {
      // load rows in ASCII file into matrix ROWS
      for (int i=1; i<=M; ++i)
        for (int j=1; j<=N; ++j)
          ins >> col_[j][i];      // >> (*this)(i,j);
    }
    else {
      // load rows in ASCII file into matrix COLUMNS
      assert(SZ_DATA_MAT_COLS==rAD.mode());
      for (int j=1; j<=N; ++j)
        for (int i=1; i<=M; ++i)
          ins >> col_[j][i];      // >> (*this)(i,j);
    }
  } catch(...) { 
    umERROR("Mat_COL<T>(M,N, char *s)", "problem parsing data.");
  }
}


///////////////////////////////////////////////////////////
//
// destructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::~Mat_COL()
//---------------------------------------------------------
{
  destroy();
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::destroy()
//---------------------------------------------------------
{
  // Base class Vector manages deallocation of data,
  // This class manages only its COLUMN pointers.
  Vector<T>::destroy();

  // Note: restore "col_" to 0-offset
  if (col_) {col_ ++; ::free(col_); col_=NULL;}

  m_M = m_N = m_MN = 0;
  m_fact_mode = FACT_NONE;

  if (m_ipiv) { umIVectorFree(m_ipiv); }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::Free()
//---------------------------------------------------------
{ 
  destroy(); // release registry entry
}  


///////////////////////////////////////////////////////////
//
// manage allocation
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::borrow(int M, int N, T* p)
//---------------------------------------------------------
{
  // "Borrowing" data allows wrapping external 
  // arrays to exploit matrix algorithms while 
  // avoiding any overhead of copying data.

  assert(M*N >=0);    // must be non-negative
  assert(p);          // expecting valid data

  // base class calls virtual destroy() to reset
  // all members, and handles the storage.
  Vector<T>::borrow(M*N, p);

  if (M>0 && N>0) {
    // prepare index arrays
    set_pointers(M, N);
  }

  return (*this);
}


// lend_col sets up col to borrow column N of *this
//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::lend_col(int N, Vector<T> &col)
//---------------------------------------------------------
{
  CheckIdx_Col_1(N);
  col.borrow(m_M, this->pCol(N));
}


// The internal contiguous (0-offset) array v_[M*N] is 
// allocated in base class Vector.  Here we create an 
// internal array of column pointers to enable 1-based, 
// column-major indexing into an (M,N) matrix.
//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::set_pointers(int M, int N)
//---------------------------------------------------------
{
  m_fact_mode = FACT_NONE;

  assert( this->v_ );  // data allocated in Vector::initialize()
  assert( M >= 1);
  assert( N >= 1);

  // clear the old set of column pointers
  // Note: restore "col_" to 0-offset
  if (col_) {col_ ++; ::free(col_); col_=NULL;}

  // allocate a New set of col pointers
  col_ = (T **) calloc((size_t)N, sizeof(T*) );
  assert(col_);

  m_M  = M;     // num rows
  m_N  = N;     // num cols
  m_MN = M*N;   // total elements

  // adjust pointers for 1-based indexing
  T* p = this->v_ - 1;
  for (int i=0; i<N; ++i)
  {
    col_[i] = p;
    p += M;
  }

  col_ -- ;   // adjust for 1-based indexing
}



// The internal contiguous (0-offset) array v_[M*N] is 
// allocated by base class Vector. Here we just call
// set_pointers(M,N) to set up the appropriate 1-based 
// indexing strategy.
//---------------------------------------------------------
template <typename T> inline
bool Mat_COL<T>::resize(int M, int N, bool bInit, T x)
//---------------------------------------------------------
{
  // Resize existing object, optionally setting 
  // the entire array to some given inital value.
  // Return value indicates whether size has changed.

  assert(!this->m_borrowed);  // "borrowed" allocations must not be changed
  assert(M >= 0);             // must be non-negative
  assert(N >= 0);

  bool bResized=false;

  if ((M != m_M) || (N != m_N)) 
  {
    m_fact_mode = FACT_NONE;

    bResized=true;
    this->destroy();      // clear allocation, zero all members
    if ((M>0) && (N>0)) 
    {
      initialize(M*N, bInit, x);
      set_pointers(M, N);
    }
  }
  else if (bInit && this->m_Len>0)
  {
    m_fact_mode = FACT_NONE;  // destroys any factorization
    fill(x);                  // no resize, just fill existing array
  }

  m_M  = M;     // num rows
  m_N  = N;     // num cols
  m_MN = M*N;   // total elements

  return bResized;
}


//---------------------------------------------------------
template <typename T> inline
bool Mat_COL<T>::resize(const Mat_COL<T>& B, bool bInit, T x)
//---------------------------------------------------------
{
  // Resize existing object to match size of matrix B.
  // Optionally set entire array to given inital value.
  // Return value indicates whether size has changed.

  int Nrow=B.num_rows(), Ncol=B.num_cols();
  bool bResized = this->resize(Nrow, Ncol, bInit, x);
  return bResized;
}


//---------------------------------------------------------
template <typename T> inline
bool Mat_COL<T>::realloc(int newM, int newN, bool bInit, T x)
//---------------------------------------------------------
{
  // Re-allocate the matrix, if necessary, to match 
  // new dimensions.  Appropriate range of existing 
  // data is copied (columnwise) into new structure.

  if (newM==m_M && newN==m_N) {
    // no change
    return false;
  } 

  //-------------------------------------
  // Note: retain existing column data
  //-------------------------------------
  bool reset_mode=false;
  if (OBJ_temp == this->get_mode()) {
    this->set_mode(OBJ_real);   // avoid premature deletion
    reset_mode=true;
  }

  Mat_COL<T> old(*this);        // store existing data
  resize(newM, newN, bInit,x);  // resize allocation

  int Nr = std::min(newM, old.num_rows());
  int Nc = std::min(newN, old.num_cols());
  for (int j=1; j<=Nc; ++j) {   // reload old col j
    (*this).load_col(j, Nr, old.pCol(j));
  }
  if (reset_mode) { 
    this->set_mode(OBJ_temp);   // restore mode
  }
  return true;                  // shape & size was changed
}


//---------------------------------------------------------
template <typename T> inline
bool Mat_COL<T>::reshape(int newM, int newN, bool bInit, T x)
//---------------------------------------------------------
{
  // Re-shapes the matrix, expanding or contracting to 
  // match new dimensions.  Existing data is retained, 
  // but logically "wrapped" into new shape.

  if (newM==m_M && newN==m_N) {
    return false;               // no change
  }
  if (newM*newN != m_M*m_N) {
    extend(newM*newN,bInit,x);  // expand or contract
  }
  set_pointers(newM, newN);     // adjust logical indexing
  return true;                  // shape has changed
}


//---------------------------------------------------------
template <typename T> inline
bool Mat_COL<T>::compatible(const Mat_COL<T>& B) const
//---------------------------------------------------------
{
  // check for matching matrix dimensions
  if (B.num_rows() != this->m_M) { return false; }
  if (B.num_cols() != this->m_N) { return false; }
  return true;
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::append_col(const Vector<T>& V)
//---------------------------------------------------------
{
  // enable Matlab: Mat = [Mat, col];

  // To append a column, simply append new data to tail 
  // of existing array, then adjust logical dimensions.
  // Note: handle case of appending 1st col to empty mat

  if (!V.ok()) {
    // empty args may still be "temporary"
    if (V.get_mode() == OBJ_temp) { delete (&V); }
    return;   // nothing to do
  }      

  assert(!this->m_borrowed);    // not change "borrowed" structure

  if (!ok()) {
    resize(V.length(), 1);      // appending "1st" col to empty mat
    this->set_col(1, V);        // load data into new column 
  } else {
    int old_len = size();       // store current length
    int new_len = size()+m_M;   // calculate required length
    this->extend(new_len);      // add space for new column
    set_pointers(m_M, m_N+1);   // adjust logical indexing
    this->set_col(m_N, V);      // load data into new column 
  }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::append_cols(const Mat_COL<T>& B)
//---------------------------------------------------------
{
  // enable Matlab: Mat = [MatA, MatB];

  // To append a matrix as extra columns, simply append 
  // the new col-major data to the tail of the existing 
  // col-major array, then adjust logical dimensions.
  // Note: handle case of "appending" to empty mat

  if (!B.ok()) {
    // empty args may still be "temporary"
    if (B.get_mode() == OBJ_temp) { delete (&B); }
    return;
  }

  assert(!this->m_borrowed);  // don't change "borrowed" structure
  if (!ok()) {
    (*this) = B;              // copy as a new matrix
  } 
  else 
  {
    // assume compatible column length
    if (this->num_rows()!=B.num_rows()) { 
      umERROR("Mat_COL<T>::append_cols", 
              "Expected col.length of %d, found %d.", m_M, B.num_rows()); 
    }

    int newNc = this->num_cols()+B.num_cols();
    Vector<T>::append(B);       // append col-major data to tail
    set_pointers(m_M, newNc);   // adjust logical indexing
  }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::append_row(const Vector<T>& V)
//---------------------------------------------------------
{
  // enable Matlab: Mat = [Mat; row];

  // Appending a row is more expensive than appending 
  // a column, since each element of the new row must
  // be "inserted" into the old data with stride m_M.
  // Here we simply allocate a new (M+1,N) matrix, 
  // copy each old column j into the range (1:M,j), 
  // then insert the new row into range (M+1,:).

  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  ---------
  //  [+][+][+]   <-- new row threaded into old data

  if (!V.ok()) {
    // empty args may still be "temporary"
    if (V.get_mode() == OBJ_temp) { delete (&V); }
    return;   // nothing to do
  }      

  assert(!this->m_borrowed);  // don't change "borrowed" structure

  if (!ok()) {
    int rlen = V.length();    // matrix is empty, so ...
    this->resize(1, rlen);    // set initial shape, and ...
    this->set_row(1,V);       // set first row of new matrix
  } 
  else 
  {
    // allocate an empty matrix
    Mat_COL<T> *tmp = new Mat_COL("tmp: adding row", OBJ_temp);
    
    // resize the new matrix WITHOUT initializing elements
    tmp->resize(m_M+1, std::max(m_N,1), false);

    // load existing (column-oriented) data into new matrix
    for (int j=1; j<=m_N; ++j) {
      tmp->load_col(j, m_M, this->pCol(j));  // load (1:M,j)
    }

    tmp->set_row(m_M, V);   // insert elements of the new row
    (*this) = (*tmp);       // swap ownership of expanded matrix
  }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::append_rows(const Mat_COL<T>& B)
//---------------------------------------------------------
{
  // enable Matlab: Mat = [MatA; MatB];

  // Appending rows is more expensive than appending 
  // columns, since elements of each new row must
  // be "inserted" into the old data with stride m_M.
  // Here we simply allocate a new (M1+M2,N) matrix, 
  // copy each old column j into the range (1:M1,j), 
  // then insert the new rows into range (M1+1:M1+M2,:).

  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  ---------
  //  [+][+][+]   <-- new rows threaded 
  //  [+][+][+]   <-- ... into old data
  //  [+][+][+]   <-- 

  if (!B.ok()) {
    // empty args may still be "temporary"
    if (B.get_mode() == OBJ_temp) { delete (&B); }
    return;
  }    

  assert(!this->m_borrowed);  // don't change "borrowed" structure
  if (!ok()) {
    (*this) = B;              // copy as a new matrix
  } 
  else 
  {
    // assume compatible row length
    if (this->num_cols()!=B.num_cols()) { 
      umERROR("Mat_COL<T>::append_rows", 
              "Expected row length of %d, found %d.", m_N, B.num_cols()); 
    }

    // allocate an empty matrix
    Mat_COL<T> *tmp = new Mat_COL("tmp: adding rows", OBJ_temp);

    // resize the new matrix WITHOUT initializing elements
    int M1=this->num_rows(), M2=B.num_rows();
    tmp->resize(M1+M2, m_N, false);

    // load existing (column-oriented) data into new matrix
    for (int j=1; j<=m_N; ++j) {
      tmp->load_col(j, m_M, this->pCol(j));  // load (1:M1,j)
    }

    // append rows of new matrix to copy of old matrix
    for (int i=1; i<=M2; ++i) {
      tmp->set_row(M1+i, B.get_row(i));
    }
    if (B.get_mode()==OBJ_temp) {delete (&B);} // clean up

    // swap ownership of expanded matrix
    (*this) = (*tmp);
  }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::append_rows(int Nr, int Nc)
//---------------------------------------------------------
{
  // Append num_rows empty rows to existing matrix
  // Allow user to specify Nc = num cols in case 
  // the current matrix in empty.

  // Appending rows is more expensive than appending 
  // columns, since elements of each new row must
  // be "inserted" into the old data with stride m_M.
  // Here we simply allocate a new (M1+M2,N) matrix, 
  // copy each old column j into the range (1:M1,j), 
  // then insert the new rows into range (M1+1:M1+M2,:).

  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  [ ][ ][ ]
  //  ---------
  //  [+][+][+]   <-- new rows threaded 
  //  [+][+][+]   <-- ... into old data
  //  [+][+][+]   <-- 

  if (Nr<1) { return; }       // no rows added

  assert(!this->m_borrowed);  // don't change "borrowed" structure
  if (!ok()) {
    this->resize(Nr, Nc);     // allocate new matrix of user size
  } 
  else 
  {
    // allocate an empty matrix
    Mat_COL<T> *tmp = new Mat_COL("tmp: adding rows", OBJ_temp);

    // resize the new matrix, initializing data to zero
    int M1=this->num_rows();
    tmp->resize(M1+Nr, m_N, true, T(0));

    // load existing (column-oriented) data into new matrix
    for (int j=1; j<=m_N; ++j) {
      tmp->load_col(j, m_M, this->pCol(j));  // load (1:M1,j)
    }

    // swap ownership of expanded matrix
    (*this) = (*tmp);
  }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::merge_rows(IVec map, Mat_COL<T>& B)
//---------------------------------------------------------
{
  // merge the rows of B into the rows of (*this)

  int Nr  = this->num_rows(), Nc = this->num_cols();
  int Nrb = B.num_rows(), id = 0;
  assert(B.num_cols() == Nc);     // compatible?
  int max_row = map.max_val();    // find max. new row index
  int new_rows = max_row-Nr;      // number of rows to append
  if (new_rows>0) {
    this->append_rows(new_rows);
  }

  // inset rows of B into (possibly expanded) (*this)
  for (int i=1; i<=Nrb; ++i) {
    id = map(i);
    this->set_row(id, B.get_row(i));
  }
  if (B.get_mode()==OBJ_temp) {delete (&B);} // clean up
}



///////////////////////////////////////////////////////////
//
// copy/load
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::copy(const T *vec)
//---------------------------------------------------------
{
  // assumes sufficient elements, col-major sequence
  Vector<T>::copy(vec);
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::copy(const Mat_COL<T>& B)
//---------------------------------------------------------
{
  this->operator=(B);
  return (*this);
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::load(int M, int N, const T *vdata)
//---------------------------------------------------------
{
  // load (col-major) data into matrix COLUMNS
  resize(M,N, false);
  copy(vdata);
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::load(int M, int N, const Vector<T>& V)
//---------------------------------------------------------
{
  // load (col-major) data into matrix COLUMNS
  
  resize(M,N);    // initialize to zeros(M,N)

  int len = V.size();
  if (M*N == len) {
    // wrap vector data into existing (M,N) structure.
    Vector<T>::operator= (V);  // deletes V if OBJ_temp
  } else {
    // load section of data into (M,N) structure.
    len = std::min(len, (M*N));
    Vector<T>::copy(len, V.data());
    if (OBJ_temp == V.get_mode()) { delete (&V); }
  }

  // refresh pointers in case we switched allocation.
  set_pointers(m_M, m_N);
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::load_col(int j, int Nr, const T* pdata)
//---------------------------------------------------------
{
  // load values into range (1:Nr,j)

  assert(j >=1 && j <=this->num_cols());  // check col index
  assert(Nr>=0 && Nr<=this->num_rows());  // check col length
  for (int i=1; i<=Nr; ++i)
    this->col_[j][i] = pdata[i-1];
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::load_rows(int M, int N, const char *sdata)
//---------------------------------------------------------
{
  // load ASCII rows into matrix ROWS
  resize(M,N);
  try {
    std::istringstream ins(sdata);
    for (int i=1; i<=M; ++i)    // for each row...
      for (int j=1; j<=N; ++j)  //  for each col...
        ins >> col_[j][i];      // >> (*this)(i,j);
  } catch(...) { 
    umERROR("load(M,N, char *s)", "problem parsing data.");
  }
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::load_cols(int M, int N, const char *sdata)
//---------------------------------------------------------
{
  // load ASCII rows into matrix COLUMNS
  resize(M,N);
  try {
    std::istringstream ins(sdata);
    for (int j=1; j<=N; ++j)    // for each col...
      for (int i=1; i<=M; ++i)  //  for each row...
        ins >> col_[j][i];      // >> (*this)(i,j);
  } catch(...) { 
    umERROR("load(M,N, char *s)", "problem parsing data.");
  }
}




///////////////////////////////////////////////////////////
//
// assignment
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const T &val)
//---------------------------------------------------------
{ 
  fill(val); 
  return (*this);
}


//---------------------------------------------------------
// allow assignment of IMat to DMat
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::assign(const IMat &B)
//---------------------------------------------------------
{
  int M=B.num_rows(), N=B.num_cols();
  m_fact_mode = FACT_NONE;
  if (this->m_name=="mat" || this->m_name.empty()) 
    this->m_name=B.name();

  this->resize(M,N);          // resize array of T
  const int* p = B.data();    // load int data as T

  int Nmod4 = this->m_Len & 3;
  int N4 = this->m_Len - Nmod4, i=0;
  // unroll
  for (i=0; i<N4; i+=4) {
    this->v_[i  ]=T(p[i  ]); this->v_[i+1]=T(p[i+1]);
    this->v_[i+2]=T(p[i+2]); this->v_[i+3]=T(p[i+3]);
  }
  // cleanup
  for (i=N4; i<this->m_Len; ++i) { this->v_[i] = T(p[i]); }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const Mat_COL<T> &B)
//---------------------------------------------------------
{
  if (this->v_ == B.v_) {
    // if both matrices are empty, then check for 
    // NULL to avoid leaking empty OBJ_temp's
    if (NULL != this->v_) {
      return (*this);
    }
  }

  int M=B.m_M, N=B.m_N;
  m_fact_mode = B.get_factmode();
  if (this->m_name=="mat" || this->m_name.empty()) 
    this->m_name=B.name();

  // base class manages the actual allocation
  Vector<T>::operator= ((const Vector<T>&) B);

  if (this->m_Len > 0) {
    // adjust (column) pointers for (M,N) indexing
    set_pointers(M, N);
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const Vector<T> &V)
//---------------------------------------------------------
{
  // load from Vector:
  //
  // Note: loading a vector into a matrix
  // Allow assignment of a vector of matching
  // length into an existing (M,N) matrix

  if (this->v_ == V.data())
    return (*this);

  m_fact_mode = FACT_NONE;
  if (this->m_name=="mat" || this->m_name.empty()) 
    this->m_name=V.name();

  int len = V.size();

  if (this->ok() && (m_M*m_N == len))
  {
    // load a vector into an existing (M,N) structure.
    Vector<T>::operator= (V);

    // refresh pointers in case we switched allocation.
    set_pointers(m_M, m_N);
  }
  else
  {
    // copy vector into a new (N,1) matrix
    Vector<T>::operator= (V);
    set_pointers(len, 1);
  }

  return (*this);
}



///////////////////////////////////////////////////////////
//
// utilities
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator!() const
//---------------------------------------------------------
{
  // returns a "Boolean" result: all non-zeros in 
  // this are "toggled" to zero, and vice-versa.

  // initialize result to ZERO, then toggle...
  Mat_COL<T>* tmp = new Mat_COL(m_M, m_N, this->ZERO, OBJ_temp, "!TMP");
  for (int i=0; i<this->m_Len; ++i){
    if (this->ZERO == this->v_[i]) { 
      tmp->v_[i] = this->ONE; 
    }
  }
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::identity(int N)
//---------------------------------------------------------
{
  // set to (N,N) identity matrix
  resize(N,N, true, this->ZERO);
  for (int i=1; i<=N; ++i)
    col_[i][i] = T(1);  // 1-based col-major version

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::diag(const Vector<T>& d)
//---------------------------------------------------------
{
  // set to (N,N) diagonal matrix
  int N = d.size();
  resize(N,N, true, this->ZERO);
  for (int i=1; i<=N; ++i) 
    col_[i][i] = d(i);

  // if d is temporary, delete it.
  if (d.get_mode() == OBJ_temp) { delete (&d); }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::set_diag
(
  int i,              // index of the [sub/sup] diagonal
  const Vector<T>& d, // the data to write along diagonal
  bool MLmode         // if ("matlab" mode), then fill 
)                     // super-diagonals from tail of d
//---------------------------------------------------------
{
  // replace ith diagonal with elements from d
  // FIXME: calc. max length when rectangular

  int idx = std::abs(i), k=0;
  if (idx < min_mn())           // check valid diagonal
  {
    int Nd  = d.size();         // length of argument
    int Ndi = min_mn() - idx;   // length of ith diagonal
    assert(Ndi <= Nd);          // check enough elements

    if (i>0) 
    {
      if (MLmode) {
        // Matlab "spdiags" mode: fill super-diagonals 
        // starting from the tail of the arg array, d.
        for (k=Nd; k>i;  --k) { col_[k][k-i] = d(k); }
      } else {
        for (k=1;k<=Ndi; ++k) { col_[k+i][k] = d(k); }
      }
    }
    else {
      for (k=1; k<=Ndi; ++k)  col_[k][k-i] = d(k);
    }
  } else {
    umLOG(1, "diagonal %d out of range [0:(+/-)%d]\n", i, min_mn()-1);
  }

  // if d is temporary, delete it.
  if (d.get_mode() == OBJ_temp) { delete (&d); }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::set_diags
(
  const Mat_COL<T>& B,  // each column holds a diagonal
  const IVec& ids,      // ids for placing each diagonal
  int m, int n          // dimensions of final matrix
)
//---------------------------------------------------------
{
  // Build matrix A using the columns of matrix arg B 
  // as the diagonals of A, indexed by IVec arg d.
  // see Matlab "spdiags"
  // A = spdiags([dM1,d0,dP1], [-1,0,1], np, np);

  int Ndiags = ids.size(), Nr=B.num_rows(), di=0; DVec vec;
  if (B.num_cols()<Ndiags) {
    umERROR("Mat_COL<T>::set_diags", "expected %d columns, found %d", Ndiags, B.num_cols());
  }

  this->resize(m,n);
  Mat_COL<T>& D=const_cast<Mat_COL<T>&>(B); // enable borrow()

  for (int i=1; i<=Ndiags; ++i) {
    di = ids(i);                // index for this diagonal
    vec.borrow(Nr, D.pCol(i));  // data to load along diagonal
    set_diag(di, vec, true);    // load diagonal in "spdiags" mode
  }                             // i.e. load super-diags from tail
  if (OBJ_temp == B.get_mode())   { delete (&B); }
  if (OBJ_temp == ids.get_mode()) { delete (&ids); }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::transpose()
//---------------------------------------------------------
{
  int rows = this->num_cols();
  int cols = this->num_rows();

  Mat_COL<T>* tmp = new Mat_COL<T>(rows,cols, this->ZERO, OBJ_temp, "mat");

  for (int j=1; j<=cols; ++j)
    for (int i=1; i<=rows; ++i)
      (*tmp)(i,j) = (*this)(j,i);

  // replace this matrix with its transpose
  (*this) = (*tmp);
  return (*this);
}


//---------------------------------------------------------
template <typename T>
Mat_COL<T>& Mat_COL<T>::hilbert(int N)
//---------------------------------------------------------
{
  // load (N,N) Hilbert matrix
  this->resize(N,N, false);
  for (int j=1; j<=N; ++j)
    for (int i=1; i<=N; ++i)
      col_[j][i] = (1.0 / double(i+j-1));

  return (*this);
}



//---------------------------------------------------------
template <typename T> inline
bool Mat_COL<T>::operator==(const Mat_COL<T> &B) const
//---------------------------------------------------------
{
  // Comparison  ("fuzzy", uses m_EqTol)

  if (this->v_ == B.v_)    return true;   // same vector?
  if (B.num_rows() != m_M) return false;  // diff. shape?
  if (B.num_cols() != m_N) return false;  // diff. shape?

  // compare data in base arrays
  return Vector<T>::operator==((const Vector<T>&)B);
}




///////////////////////////////////////////////////////////
//
// member Boolean operations
//
///////////////////////////////////////////////////////////


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::eq(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = (v(i) == val) ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(x==val)");
  for (int i=1; i<=this->m_Len; ++i) { if (this->vm1_[i] == val) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::eq(Mat_COL<T>& B) const
{
  // return a matrix of [0/1] values:
  // r(i,j) = (A(i,j) == B(i,j)) ? 1:0

  if (! this->compatible(B)) { umERROR("Mat_COL<T>::eq(B)", "matrix dimensions not compatible"); }
 
  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(A==B)");
  const T* pA=this->data(), *pB=B.data(); T* pT=tmp->data();
  for (int i=0; i<this->m_Len; ++i) {
    if (pA[i] == pB[i]) { pT[i] = T(1); } 
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); } // delete temps
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::le(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = (v(i) <= val) ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(x<=val)");
  for (int i=1; i<=this->m_Len; ++i) { if (this->vm1_[i] <= val) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::lt(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = (v(i) < val) ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(x<val)");
  for (int i=1; i<=this->m_Len; ++i) { if (this->vm1_[i] < val) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::lt_abs(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = |v(i)| < val ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(|x|<tol)");
  T fval = std::abs(val);
  for (int i=1; i<=this->m_Len; ++i) { if (std::abs(this->vm1_[i]) < fval) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::ge(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = (v(i) >= val) ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(x>=val)");
  for (int i=1; i<=this->m_Len; ++i) { if (this->vm1_[i] >= val) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::gt(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = (v(i) > val) ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(x>val)");
  for (int i=1; i<=this->m_Len; ++i) { if (this->vm1_[i] > val) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::gt_abs(T val) const
{
  // return a matrix of [0/1] values:
  //   r(i) = |v(i)| > val ? 1:0

  // initialize with zeros.
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->ZERO, OBJ_temp, "(|x|>tol)");
  T fval = std::abs(val);
  for (int i=1; i<=this->m_Len; ++i) { if (std::abs(this->vm1_[i]) > fval) { tmp->vm1_[i] = this->ONE; } }
  return (*tmp);
}



//---------------------------------------------------------
// get MAX row/col values without copying data
//---------------------------------------------------------
template <typename T> inline
T Mat_COL<T>::max_row_val(const int i) const {
  CheckIdx_Row_1(i);  if (m_N<1) return this->ZERO;
  T res = col_[1][i];
  for (int j=2; j<=m_N; ++j) res = std::max(res, col_[j][i]);
  return res;
}

template <typename T> inline
T Mat_COL<T>::max_row_val_abs(const int i) const {
  CheckIdx_Row_1(i);  if (m_N<1) return this->ZERO;
  T res = std::abs(col_[1][i]);
  for (int j=2; j<=m_N; ++j) res = std::max(res, std::abs(col_[j][i]));
  return res;
}

template <typename T> inline
T Mat_COL<T>::max_col_val(const int j) const {
  CheckIdx_Col_1(j);  if (m_M<1) return this->ZERO;
  T res = col_[j][1];
  for (int i=2; i<=m_M; ++i) res = std::max(res, col_[j][i]);
  return res;
}

template <typename T> inline
T Mat_COL<T>::max_col_val_abs(const int j) const {
  CheckIdx_Col_1(j);  if (m_M<1) return this->ZERO;
  T res = std::abs(col_[j][1]);
  for (int i=2; i<=m_M; ++i) res = std::max(res, std::abs(col_[j][i]));
  return res;
}

//---------------------------------------------------------
// get MIN row/col values without copying data
//---------------------------------------------------------
template <typename T> inline
T Mat_COL<T>::min_row_val(const int i) const {
  CheckIdx_Row_1(i);  if (m_N<1) return this->ZERO;
  T res = col_[1][i];
  for (int j=2; j<=m_N; ++j) res = std::min(res, col_[j][i]);
  return res;
}

template <typename T> inline
T Mat_COL<T>::min_row_val_abs(const int i) const {
  CheckIdx_Row_1(i);  if (m_N<1) return this->ZERO;
  T res = std::abs(col_[1][i]);
  for (int j=2; j<=m_N; ++j) res = std::min(res, std::abs(col_[j][i]));
  return res;
}

template <typename T> inline
T Mat_COL<T>::min_col_val(const int j) const {
  CheckIdx_Col_1(j);  if (m_M<1) return this->ZERO;
  T res = col_[j][1];
  for (int i=2; i<=m_M; ++i) res = std::min(res, col_[j][i]);
  return res;
}

template <typename T> inline
T Mat_COL<T>::min_col_val_abs(const int j) const {
  CheckIdx_Col_1(j);  if (m_M<1) return this->ZERO;
  T res = std::abs(col_[j][1]);
  for (int i=2; i<=m_M; ++i) res = std::min(res, std::abs(col_[j][i]));
  return res;
}


///////////////////////////////////////////////////////////
//
// matrix norms
//
///////////////////////////////////////////////////////////


template <typename T>
double Mat_COL<T>::norm1() const
{
  // matrix 1-norm: max column sum
  assert(ok());

  T maxval = this->ZERO;
  for (int j=1; j<=m_N; ++j) {
    T sum = this->ZERO;
    for (int i=1;i<=m_M;++i) {sum += std::abs(col_[j][i]);}
    maxval = std::max(maxval,sum);
  }
  return (double) maxval;
}


template <typename T>
double Mat_COL<T>::norm_inf() const
{
  // matrix infinity-norm: max row sum
  assert(ok());

  T maxval = this->ZERO;
  for (int i=1; i<=m_M; ++i) {
    T sum = this->ZERO;
    for (int j=1;j<=m_N;++j) {sum += std::abs(col_[j][i]);}
    maxval = std::max(maxval,sum);
  }
  return (double) maxval;
}


template <typename T>
double Mat_COL<T>::norm_frob() const
{
  // matrix frobenius-norm

  assert(ok());
  double ss = (double) this->sumsquares();
  return sqrt(ss);
}


///////////////////////////////////////////////////////////
//
// element by element operations.  Most call optimized 
// Vector<T> versions, deleting temporary args
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
// The first set update (*this) in place
//---------------------------------------------------------

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::mult_element(const Mat_COL<T> &B)
{
  // A = A .* B
  Vector<T>::operator *= ((const Vector<T>&)B);
  return (*this);
}

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::mult_element(const Vector<T>& b)
{
  // A = A .* b
  Vector<T>::operator *= (b);
  return (*this);
}

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::mult_element(const T* data)
{
  // A = A .* data
  Vector<T>::operator *= (data);
  return (*this);
}

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::div_element (const Mat_COL<T> &B)
{
  // A = A ./ B
  Vector<T>::operator /= ((const Vector<T>&)B);
  return (*this);
}

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::div_element (const Vector<T>& b)
{
  // A = A ./ b
  Vector<T>::operator /= (b);
  return (*this);
}


//---------------------------------------------------------
// The following variations do not change (*this)
//---------------------------------------------------------

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::dd(const Mat_COL<T> &B) const
{
  // (*this) is not changed: C = A ./ B

  std::string sz; tmp_op_name(this->name(), "./", B.name(), sz);
  // NOT USING COPY CONSTRUCTOR: side-effects if (*this)==OBJ_temp
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->data(), OBJ_temp, sz.c_str());
  (*tmp).div_element(B);
  if (OBJ_temp == this->m_mode) {
    delete (this);
  }
  return (*tmp);
}
  
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::dm(const Mat_COL<T> &B) const
{
  // (*this) is not changed: C = A .* B

  std::string sz; tmp_op_name(this->name(), ".*", B.name(), sz);
  // NOT USING COPY CONSTRUCTOR: side-effects if (*this)==OBJ_temp
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->data(), OBJ_temp, sz.c_str());
  (*tmp).mult_element(B);
  if (OBJ_temp == this->m_mode) {
    delete (this);
  }
  return (*tmp);
}

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::dm(const Vector<T> &V) const
{
  // (*this) is not changed: C = A .* V

  std::string sz; tmp_op_name(this->name(), ".*", V.name(), sz);
  // NOT USING COPY CONSTRUCTOR: side-effects if (*this)==OBJ_temp
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->data(), OBJ_temp, sz.c_str());
  (*tmp).mult_element(V);
  if (OBJ_temp == this->m_mode) {
    delete (this);
  }
  return (*tmp);
}

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::dm(const T* data) const
{
  // (*this) is not changed: C = A .* data

  // assumes input array has sufficient elements
  std::string sz; tmp_op_name(this->name(), ".*", "v", sz);

  // NOT USING COPY CONSTRUCTOR: side-effects if (*this)==OBJ_temp
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->data(), OBJ_temp, sz.c_str());
  (*tmp).mult_element(data);
  if (OBJ_temp == this->m_mode) {
    delete (this);
  }
  return (*tmp);
}




/////////////////////////////////////////////////////////
//
// numerical routines:  BLAS 3 matrix/matrix ops
//
/////////////////////////////////////////////////////////


//---------------------------------------------------------
// matrix multiplication
//---------------------------------------------------------

template <> inline  // specialization for <T>=<double>
DMat& DMat::operator*=(const DMat &B)
{
  // calls BLAS DGEMM

  DMat* C = new DMat("op*=TMP", OBJ_temp);
  umAxB((*this), B, (*C));
  (*this) = (*C);
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*this);
}

template <> inline  // specialization for <T>=< complex<double> >
ZMat& ZMat::operator*=(const ZMat &B)
{
  // calls BLAS ZGEMM

  ZMat* C = new ZMat("op*=TMP", OBJ_temp);
  umAxB((*this), B, (*C));
  (*this) = (*C);
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*this);
}


template <typename T>
Mat_COL<T>& Mat_COL<T>::operator*=(const Mat_COL<T> &B)
{
  // default n^3 algorithm

  Mat_COL<T>* C = new Mat_COL<T>("op*=TMP", OBJ_temp);

  OBJ_mode old_md = this->get_mode(); // (*this) may be OBJ_temp
  this->set_mode(OBJ_real);           // stop matmult deleting (*this).
  matmult((*this), B, (*C));          // default n^3 algorithm
  this->set_mode(old_md);             // restore original mode
  (*this) = (*C);
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*this);
}


//---------------------------------------------------------
// matrix "division": Matlab's A/B, A\B
//---------------------------------------------------------

template <typename T> 
Mat_COL<T>& Mat_COL<T>::operator/=(const Mat_COL<T> &A)
{
  //--------------------------------------------------
  // Set (*this) to be a matrix of LU solution vectors
  // See Matlab: mrdivide(B,A)  ==  B/A  ==  B*inv(A)
  // X = B/A is the solution to the equation XA = B
  //--------------------------------------------------
  assert(sizeof(T) == sizeof(double));


  // Matlab's A\B and B/A
  //-------------------------------------------------------
  //  B/A ->    B*inv(A) ->   B/A =   ( A'    \   B'  )'
  // ------    -----------    -----------------------------
  //   X                          = tr( tr(A) | tr(B) );
  //-------------------------------------------------------
  //  (*this)  = tr( tr(A) | tr(*this) );
  //-------------------------------------------------------
#if (1)    
  OBJ_mode old_mode = this->m_mode;
  this->m_mode = OBJ_real;  // avoid premature deletion
  (*this)  = trans( trans(A) | trans(*this) );
  this->m_mode = old_mode;
#else
  (*this) *= inv(A);
#endif

  return (*this);
}


template <typename T> 
Mat_COL<T>& Mat_COL<T>::operator|=(const Mat_COL<T> &B)
{
  //--------------------------------------------------
  // Set (*this) to be a matrix of LU solution vectors
  // See Matlab: mldivide(A,B)  ==  A\B  == inv(A)*B
  // X = A\B is the solution to the equation AX = B 
  //--------------------------------------------------

  assert(sizeof(T) == sizeof(double));

  // Do not use copy ctor here: would delete B if 
  // OBJ_temp, and we need B below in solve_LU().
  Mat_COL<T>* X = new Mat_COL<T>(B.num_rows(),B.num_cols(),this->ZERO,OBJ_temp,"Xsol.TMP");

  try {
    fact_LU();
    solve_LU(B, (*X));    // B deleted here if OBJ_temp
    (*this)=(*X);         // transfer ownership of data
  } catch (...) {
    umWARNING("Mat_COL::operator |= (B)", "caught exception.");
    // TODO: clean up
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T>
Mat_COL<T>& Mat_COL<T>::invert()
//---------------------------------------------------------
{
  assert(sizeof(T) == sizeof(double));
  assert(this->is_square());

  if (1 == m_MN) {
    if (fabs(this->v_[0]) <= DBL_MIN) {umERROR("Mat_COL::invert()", "matrix has 1 (zero) element.");}
    this->v_[0] = 1.0 / this->v_[0]; return (*this);
  }

  int info=0, N = m_N;  IVec ipiv(N, 0, OBJ_temp);
  Mat_COL<T> tmp(N,N, this->data(), OBJ_temp);
  
  // factorize a copy of this matrix
  GETRF (N,N, tmp.data(), N, ipiv.data(), info);
  if (info) {umERROR("Mat_COL::invert()", "dgetrf reports: info = %d", info);}

  char op = 'N';      // Mat_COL is COL major: [N]o transpose
  this->identity(N);  // make identity, then overwrite with inverse

  // use LU factors to overwrite this with inverse
  GETRS (op, N, N, tmp.data(), N, ipiv.data(), this->data(), N, info);
  if (info) {umERROR("Mat_COL::invert()", "dgetrs reports: info = %d", info);}
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
void Mat_COL<T>::zero_below_diag()
//---------------------------------------------------------
{
  // set all values below diagonal to zero.
  // Called by chol() to make factor match Matlab's
  for (int j=1; j<m_N; ++j) {
    for (int i=j+1; i<=m_M; ++i) {
      this->col_[j][i] = (T)0;
    }
  }
}


//---------------------------------------------------------
// Cholesky factor/solve
//---------------------------------------------------------

template <typename T> inline
Mat_COL<T>& Mat_COL<T>::fact_Chol()
{
  assert(sizeof(T) == sizeof(double));

  // check if matrix is already factored
  if (FACT_CHOL == this->m_fact_mode) {return (*this);}

  chol(*this, true);  // in-place Cholesky factorization
  return (*this);     // Note: sets fact_mode
}


template <typename T> inline
void Mat_COL<T>::solve_Chol(const Mat_COL<T>& B, Mat_COL<T>& X) 
{
  // Solve a set of linear systems using Cholesky-factored 
  // symmetric positive-definite matrix, A = U^T U.

  assert(sizeof(T) == sizeof(double));
  if (FACT_CHOL != m_fact_mode) { fact_Chol(); }
  assert (FACT_CHOL == m_fact_mode);

  chol_solve((*this), B, X);
}


template <typename T>
void Mat_COL<T>::solve_Chol(const Vector<T>& b, Vector<T>& x)
{
  // Solves a linear system using Cholesky-factored 
  // symmetric positive-definite matrix, A = U^T U.

  assert(sizeof(T) == sizeof(double));
  if (FACT_CHOL != m_fact_mode) { fact_Chol(); }
  assert (FACT_CHOL == m_fact_mode);

  x = chol_solve((*this), b);
}



//---------------------------------------------------------
// LU factor/solve
//---------------------------------------------------------

template <typename T>
Mat_COL<T>& Mat_COL<T>::fact_LU()
{
  assert(sizeof(T) == sizeof(double));

  // check if matrix is already factored
  if (FACT_LUP == this->m_fact_mode) {return (*this);}

  // For later estimation of condition number, store 
  // a norm matching the mode for cond.est in GECON.

  m_norm_1 = norm1();
//m_norm_inf = norm_inf();

  lu(*this, true);    // in-place LU factorization
  return (*this);     // sets fact_mode and pivots
}


//---------------------------------------------------------
template <typename T>
double Mat_COL<T>::solve_LU
(
  const Mat_COL<T>& B,  // matrix of RHS's
        Mat_COL<T>& X,  // matrix of solutions
        bool bTrans,    // is this transposed?
        bool bCond      // estimate condition number?
)
//---------------------------------------------------------
{
  // Given that this matrix has been factored by fact_LU
  // solve_LU() solves:  A   X = B
  // If bTrans, solves:  A^T X = B

  assert(sizeof(T) == sizeof(double));

  if (FACT_LUP != m_fact_mode) { fact_LU(); }
  assert (FACT_LUP == m_fact_mode);
  assert (m_ipiv);

  int rows=m_M, LDA=m_M, NRHS=B.num_cols(), LDB=B.num_rows(), info=0;
  int ldwork=4*rows; double rcond=0.0;  assert(LDB==rows);
  char NT = bTrans ? 'T' : 'N';

  X = B;    // initialize solution with RHS

  GETRS (NT, rows, NRHS, this->data(), LDA, m_ipiv, X.data(), LDB, info);

  if (info) { umERROR("DMat::solve_LU(B,X)", "dgetrs reports: info = %d", info); }

  if (bCond) {
    DVec work(ldwork,0.0,OBJ_temp); IVec iwork(rows,0,OBJ_temp);
    // get reciprocal condition estimate
    // Note: 1-norm was calculated in fact_LU
    GECON('1',rows, this->data(), LDA, m_norm_1, rcond, work.data(), iwork.data(), info);
    return 1.0/rcond;
  }
  return -1.0;  // condition estimate not requested
}


//---------------------------------------------------------
template <typename T>
double Mat_COL<T>::solve_LU
(
  const Vector<T>& b,   // RHS
        Vector<T>& x,   // solution vector
        bool bTrans,    // is this transposed?
        bool bCond      // estimate condition number?
)
//---------------------------------------------------------
{
  // Given that this matrix has been factored by fact_LU
  // solve_LU() solves:  A   x = b
  // If bTrans, solves:  A^T x = b

  assert(sizeof(T) == sizeof(double));

  if (FACT_LUP != m_fact_mode) { fact_LU(); }
  assert (FACT_LUP == m_fact_mode);
  assert (m_ipiv);

  int rows=m_M, LDA=m_M, NRHS=1, LDB=b.size(), info=0;
  int ldwork=4*rows; double rcond=0.0; assert(LDB==rows);
  char NT = bTrans ? 'T' : 'N';

  x = b;    // initialize solution with RHS

  GETRS (NT, rows, NRHS, this->data(), LDA, m_ipiv, x.data(), LDB, info);

  if (info) { umERROR("DMat::solve_LU(b,x)", "dgetrs reports: info = %d", info); }

  if (bCond) {
    DVec work(ldwork,0.0,OBJ_temp); IVec iwork(rows,0,OBJ_temp);
    // get reciprocal condition estimate
    // Note: 1-norm was calculated in fact_LU
    GECON('1',rows, this->data(), LDA, m_norm_1, rcond, work.data(), iwork.data(), info);
    return 1.0/rcond;
  }
  return -1.0;  // condition estimate not requested
}



///////////////////////////////////////////////////////////
//
// set/get entire rows/cols
//
///////////////////////////////////////////////////////////


template <typename T>
Vector<T>& Mat_COL<T>::get_row(const int i) const
{
  // NOTE: return value must be assigned or deleted!

  // return row as a vector
  CheckIdx_Row_1(i);
  int N = this->num_cols();
  Vector<T>* tmp = new Vector<T>(N, T(0), OBJ_temp, "row");
  T* p = tmp->d_m1();
  for (int j=1; j<=N; ++j) 
    p[j] = this->col_[j][i];
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::get_col(const int j) const
{
  // NOTE: return value must be assigned or deleted!

  // return column as a vector
  CheckIdx_Col_1(j);
  int M = this->num_rows();
  Vector<T>* tmp = new Vector<T>(M, T(0), OBJ_temp, "col");
  T* p = tmp->d_m1();
  for (int i=1; i<=M; ++i) 
    p[i] = this->col_[j][i];
  return (*tmp);
}


template <typename T>
void Mat_COL<T>::set_row(const int i, const T &x)
{
  // Fill row i with a value
  CheckIdx_Row_1(i);
  for (int j=1; j<=m_N; ++j) 
    this->col_[j][i] = x;
}


template <typename T>
void Mat_COL<T>::set_row(const int i, const Vector<T> &vec)
{
  // Fill row i with a vector 
  CheckIdx_Row_1(i);
  assert(vec.size() >= m_N);    // sufficient values?
  for (int j=1; j<=m_N; ++j) 
    this->col_[j][i] = vec(j);

  if (vec.get_mode()==OBJ_temp) {delete (&vec);}
}


template <typename T>
void Mat_COL<T>::set_col(const int j, const T &x)
{
  // Fill col j with a value
  CheckIdx_Col_1(j);
  for (int i=1; i<=m_M; ++i) 
    this->col_[j][i] = x;
}


template <typename T>
void Mat_COL<T>::set_col(const int j, const Vector<T> &vec)
{
  // Fill col j with a vector
  CheckIdx_Col_1(j);
  assert(vec.size() >= m_M);    // sufficient values?
  for (int i=1; i<=m_M; ++i) 
    this->col_[j][i] = vec(i);

  if (vec.get_mode()==OBJ_temp) {delete (&vec);}
}


template <typename T>
void Mat_COL<T>::add_col(const int j, const Vector<T> &vec)
{
  // Add a vector to col(j)
  CheckIdx_Col_1(j);
  assert(vec.size() >= m_M);    // sufficient values?
  for (int i=1; i<=m_M; ++i) 
    this->col_[j][i] += vec(i);

  if (vec.get_mode()==OBJ_temp) {delete (&vec);}
}


template <typename T>
void Mat_COL<T>::sub_col(const int j, const Vector<T> &vec)
{
  // Subtract a vector from col(j)
  CheckIdx_Col_1(j);
  assert(vec.size() >= m_M);    // sufficient values?
  for (int i=1; i<=m_M; ++i) 
    this->col_[j][i] -= vec(i);

  if (vec.get_mode()==OBJ_temp) {delete (&vec);}
}


template <typename T>
void Mat_COL<T>::extend_col(const int ncol)
{
  // This will extend (or contract) the matrix to 
  // have ncol columns, keeping exisitng data.
  assert(ncol>0);
  this->extend(m_M*ncol);
  this->set_pointers(m_M, ncol);
}


/////////////////////////////////////////
//
// Return vectors of {col/row} sums
//
/////////////////////////////////////////


template <> inline  // specialization for T=double
DVec& Mat_COL<double>::col_sums() const
{
  // return row vector "tmp", where tmp(j) is 
  // the sum of all elements in column(j)
  DVec* tmp = new DVec(m_N, 0.0, OBJ_temp, "row");
  double one = 1.0;
  for (int j=1; j<=m_N; ++j) {
    (*tmp)(j) = DOT(m_M, &one, 0, pCol(j), 1);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::col_sums() const
{
  // return row vector "tmp", where tmp(j) 
  // is the sum of all elements in column(j)
  Vector<T>* tmp = new Vector<T>(m_N, T(0), OBJ_temp, "row");
  T* p = tmp->d_m1();
  for (int j=1; j<=m_N; ++j) {
    T sum = this->ZERO;
    for (int i=1; i<=m_M; ++i) {
      sum += col_[j][i];
    }
    p[j] = sum;
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::col_sums_abs() const
{
  // return row vector "tmp", where tmp(j) is the sum
  // of absolute values of all elements in column(j)
  Vector<T>* tmp = new Vector<T>(m_N, T(0), OBJ_temp, "row");
  T* p = tmp->d_m1();
  for (int j=1; j<=m_N; ++j) {
    T sum = this->ZERO;
    for (int i=1; i<=m_M; ++i) {
      sum += std::abs(col_[j][i]);
    }
    p[j] = sum;
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::row_sums() const
{
  // return column vector "tmp", where tmp(i) 
  // is the sum of all elements in row(i)
  Vector<T>* tmp = new Vector<T>(m_M, T(0), OBJ_temp, "col");
  T* p = tmp->d_m1();
  for (int i=1; i<=m_M; ++i) {
    T sum = this->ZERO;
    for (int j=1; j<=m_N; ++j) {
      sum += col_[j][i];
    }
    p[i] = sum;
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


/////////////////////////////////////////
//
// Return vectors of max {col/row} data
//
/////////////////////////////////////////


template <typename T>
Vector<T>& Mat_COL<T>::max_col_vals_abs() const
{
  // return row vector "tmp", where tmp(j) is the 
  // max absolute value of elements in column(j)
  Vector<T>* tmp = new Vector<T>(m_N, T(0), OBJ_temp, "|max_col_vals|");
  T* p = tmp->d_m1();
  for (int j=1; j<=m_N; ++j) {
    p[j]=max_col_val_abs(j);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}

template <typename T>
Vector<T>& Mat_COL<T>::max_col_vals() const
{
  // return row vector "tmp", where tmp(j) 
  // is the maximum element in column(j)
  Vector<T>* tmp = new Vector<T>(m_N, T(0), OBJ_temp, "max_col_vals");
  T* p = tmp->d_m1();
  for (int j=1; j<=m_N; ++j) {
    p[j] = max_col_val(j);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::max_row_vals() const
{
  // return column vector "tmp", where tmp(i) 
  // is the maximum element in row(i)
  Vector<T>* tmp = new Vector<T>(m_M, T(0), OBJ_temp, "max_row_vals");
  T* p = tmp->d_m1();
  for (int i=1; i<=m_M; ++i) {
    p[i] = max_row_val(i);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


/////////////////////////////////////////
//
// Return vectors of min {col/row} data
//
/////////////////////////////////////////

template <typename T>
Vector<T>& Mat_COL<T>::min_col_vals_abs() const
{
  // return row vector "tmp", where tmp(j) is the 
  // min absolute value of elements in column(j)
  Vector<T>* tmp = new Vector<T>(m_N, T(0), OBJ_temp, "|min_col_vals|");
  T* p = tmp->d_m1();
  for (int j=1; j<=m_N; ++j) {
    p[j]=min_col_val_abs(j);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::min_col_vals() const
{
  // return row vector "tmp", where tmp(j) 
  // is the minimum element in column(j)
  Vector<T>* tmp = new Vector<T>(m_N, T(0), OBJ_temp, "min_col_vals");
  T* p = tmp->d_m1();
  for (int j=1; j<=m_N; ++j) {
    p[j] = min_col_val(j);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}


template <typename T>
Vector<T>& Mat_COL<T>::min_row_vals() const
{
  // return column vector "tmp", where tmp(i) 
  // is the minimum element in row(i)
  Vector<T>* tmp = new Vector<T>(m_M, T(0), OBJ_temp, "min_row_vals");
  T* p = tmp->d_m1();
  for (int i=1; i<=m_M; ++i) {
    p[i] = min_row_val(i);
  }
  if (OBJ_temp == this->m_mode) {delete this;}
  return (*tmp);
}



/////////////////////////////////////////
//
// Utility routines: reverse, sort, scale
//
/////////////////////////////////////////


//---------------------------------------------------------
template <typename T>
Mat_COL<T>& Mat_COL<T>::reverse_rows()
//---------------------------------------------------------
{
  // reverse the row order, i.e. reproduce Matlab 
  // operation: (*this) = (*this)(end:-1:1,:);

  int Nr = num_rows(), Nc = num_cols();
  Mat_COL<T>* tmp = new Mat_COL<T>(Nr,Nc, this->ZERO, OBJ_temp);
  for (int i=1; i<=Nr; ++i) {
    tmp->set_row(i, this->get_row((Nr-i)+1));
  }
  (*this) = (*tmp); // replace this with row-reversed version
  return (*this);
}


//---------------------------------------------------------
template <typename T>
Mat_COL<T>& Mat_COL<T>::sort_cols(const IVec& idx)
//---------------------------------------------------------
{
  // Permute matrix columns: reorder the columns 
  // of this matrix according to permution in idx:

  int Nr=num_rows(), Nc=num_cols();
  assert(idx.size() == Nc);
  Mat_COL<T> *tmp = new Mat_COL<T>(Nr,Nc,T(0),OBJ_temp);
  for (int j=1; j<=Nc; ++j) {
    tmp->set_col(j, this->get_col(idx(j)));
  }
  (*this) = (*tmp); // replace this with permuted version
  return (*this);
}


//---------------------------------------------------------
template <typename T>
Mat_COL<T>& Mat_COL<T>::sort(int dim)
//---------------------------------------------------------
{
  // sort each column or row according to dim
  //  1 : ascending by cols  (-1 descending)
  //  2 : ascending by rows  (-2 descending)

  int Nr=num_rows(), Nc=num_cols(), i,j;
  VecSort<T> tmp;  IVec idx;

  switch (dim) {
  case  1:  // sort columns in ascending order (in situ)
    for (j=1; j<=Nc; ++j) {
      tmp.borrow(Nr, pCol(j));
      tmp.mixedheapsort();
    }
    break;

  case  2:  // sort rows in ascending order
    for (i=1; i<=Nr; ++i) {
      tmp = this->get_row(i);
      tmp.mixedheapsort();
      this->set_row(i, tmp);
    }
    break;

  case -1:  // sort columns in decending order (in situ)
    umERROR("Mat_COL<T>::sort(-1)", "Note yet implemented");
    break;

  case -2:  // sort rows in decending order
    umERROR("Mat_COL<T>::sort(-2)", "Note yet implemented");
    break;

  default:
    umERROR("Mat_COL<T>::sort(%d)", "unexpected dim", dim);
    break;
  }

  return (*this);
}


//---------------------------------------------------------
template <typename T>
Mat_COL<T>& sortrows(const Mat_COL<T>& A, int j)
//---------------------------------------------------------
{
  // reorder the rows of matrix A using the 
  // permution implied by sorting column j

  int Nr=A.num_rows(), Nc=A.num_cols();
  assert(0<j && j<=Nc);

  // create the sort permutation
  VecSort<T> srt = A.get_col(j);
  IVec idx;  srt.makeIndex(idx);
  Mat_COL<T> *tmp = new Mat_COL<T>(Nr,Nc,T(0),OBJ_temp);
  for (int i=1; i<=Nr; ++i) {
    tmp->set_row(i, A.get_row(idx(i)));
  }
  return (*tmp);
}



//---------------------------------------------------------
// Scaling rows and columns:
//
// Since this array is column-major, we want to scale 
// both rows and columns by working along the columns.
// 
// Multiplication by a diagonal matrix from the right (M=A*D)
// reduces to scaling each column by a single element from 
// the diagonal of D;  whereas multiplication from the left
// (M=D*A) is a vector (.*) operation.
//---------------------------------------------------------

template <typename T>
void Mat_COL<T>::scale_col(const int j, const double &x)
{
  // scale all elements of column j by a scalar
  CheckIdx_Col_1(j);

  // unroll loop (down column j)
  int Nmod4 = m_M & 3;
  int N4 = m_M - Nmod4, i=0;
  T* pJ = col_[j] + 1;  // pointer to 0-offset data in col j
  for (i=0; i<N4; i+=4) {
    pJ[i  ] *= x;
    pJ[i+1] *= x;
    pJ[i+2] *= x;
    pJ[i+3] *= x;
  }
  // cleanup
  for (i=N4; i<m_M; ++i)
    pJ[i] *= x;
}


template <typename T>
void Mat_COL<T>::scale_col(const int j, const Vector<T> &vec)
{
  // "dot-mult" elements of col j by elements of vec
  CheckIdx_Col_1(j);
  assert(vec.size() >= m_M);    // sufficient values?

  // unroll loop (down column j)
  int Nmod4 = m_M & 3;
  int N4 = m_M - Nmod4, i=0;

  const T* pV = vec.data();  // pointer to 0-offset data in vec
        T* pJ = col_[j] + 1; // pointer to 0-offset data in col j

  for (i=0; i<N4; i+=4) {
    pJ[i  ] *= pV[i  ];
    pJ[i+1] *= pV[i+1];
    pJ[i+2] *= pV[i+2];
    pJ[i+3] *= pV[i+3];
  }
  // cleanup
  for (i=N4; i<m_M; ++i)
    pJ[i] *= pV[i];

  if (vec.get_mode()==OBJ_temp) {delete (&vec);}
}





//---------------------------------------------------------
// Kronecker tensor product.
// X.kron(Y) is the Kronecker tensor product of X and Y.
// The result is a large matrix formed by taking all 
// products between the elements of X and those of Y.   
// Example: if X is 2 by 3, then X.kron(Y) returns:
// 
//    [ X(1,1)*Y  X(1,2)*Y  X(1,3)*Y
//      X(2,1)*Y  X(2,2)*Y  X(2,3)*Y ]
// 
//---------------------------------------------------------

template <typename T>
void Mat_COL<T>::setKronBlock(int row, int col, const Mat_COL<T>& B)
{
  int  M = B.num_rows();
  int  N = B.num_cols();
  assert((row+M-1) <= this->num_rows());
  assert((col+N-1) <= this->num_cols());
  int rdx=0, cdx=0;

  for (int i=1; i<=M; ++i) {
    rdx = row+i-1;
    for (int j=1; j<=N; ++j) {
      cdx = col+j-1;
      (*this)(rdx,cdx) = B(i,j);
    }
  }
}


template <typename T>
Mat_COL<T>& Mat_COL<T>::kron(const Mat_COL<T>& B) const
{
  int M=this->num_rows(), N=this->num_cols();
  int brows=B.num_rows(), bcols=B.num_cols();
  int totM=M*brows, totN=N*bcols;
  int i,j,rowI,colJ; double scal=0.0;

  Mat_COL<T>* tmp = new Mat_COL<T>(totM, totN, this->ZERO, OBJ_temp, "kron.TMP");

  for (i=1; i<=M; ++i) {
    for (j=1; j<=N; ++j) {
      rowI = ((i-1) * brows) + 1;
      colJ = ((j-1) * bcols) + 1;
      scal = (*this)(i,j);
      tmp->setKronBlock(rowI, colJ, B*scal);
    }
  }

  return (*tmp);
}




///////////////////////////////////////////////////////////
//
// I/O: matrix input/output
//
///////////////////////////////////////////////////////////


// Write a format that can be read by Mat_COL<T>
template <typename T>
std::ostream& operator<<(std::ostream &s, const Mat_COL<T> &A)
{
  int M = A.num_rows(), N = A.num_cols();
  s << M << "  " << N << "\n";

  // I/O iosflags
  // s << std::setiosflags(std::ios::scientific);
  // s << std::setiosflags(std::ios::fixed);
  // s << std::setprecision(3);

  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) {
      s << A(i,j) << "  ";
      if (0==(j%20)) 
        s << "\n";
    }
    s << "\n";
  }
  s << std::endl;
  return s;
}


template <typename T>
std::istream& operator>>(std::istream &s, Mat_COL<T> &A)
{
  int M=0, N=0;
  s >> M >> N;
  A.resize(M,N);
  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) {
      s >>  A(i,j);
    }
  }
  return s;
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::print
(
  FILE* os, 
  const char* msg,
  const char* fmt, // [%e|%lf]
  int  prec,    // sig.figs|dec.places
  int  wdth,    // output spacing [12]
  int  nline,   // entries per line
  int  nr,      // num rows to write (0 --> all)
  int  nc       // num cols to write (0 --> all)
) const
//---------------------------------------------------------
{
  static char buf[20] = {""};

  // handle integer data types
  if (sizeof(T) == sizeof(double))
       sprintf(buf, "%c%d.%d%s ", '%',wdth, prec,fmt);
  else sprintf(buf, "%c%dd ", '%',wdth);


  int M = this->num_rows();
  int N = this->num_cols();

  // Select which rows, cols to write
  if (nr > 0) M = (nr<=M ? nr : M); // write min(nr,M) rows
  if (nc > 0) N = (nc<=N ? nc : N); // write min(nc,N) cols

#if (1)
  if (msg) { fprintf(os, "%s\n", msg); }
  fprintf(os, "(%d,%d)\n", M,N);
#endif

  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) 
    {
      fprintf(os, buf, (*this)(i,j));
      if (j && (0 == (j%nline)))
        fprintf(os, "\n");
    }
    fprintf(os, "\n");
  }
  fprintf(os, "\n");
  fflush(os);
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::print_STREAM
(
  std::ostream& os, 
  const char* msg,
  char fmt,     // [%e|%lf]
  int  prec,    // sig.figs|dec.places
  int  wdth,    // output spacing [12]
  int  nline,   // entries per line
  int  nr,      // num rows to write (0 --> all)
  int  nc       // num cols to write (0 --> all)
) const
//---------------------------------------------------------
{
  // save current settings
  std::ios_base::fmtflags flgs = os.flags();
  if ('E' == toupper(fmt)) 
  { os << std::setiosflags(std::ios::scientific); } 
  else if ('F' == toupper(fmt)) 
//{ os << std::setiosflags(std::ios::fixed); } 
  { os << std::setiosflags(std::ios::fixed|std::ios::showpoint); }
  else { } // general format

  os << std::setprecision(prec);

  int M = this->num_rows();
  int N = this->num_cols();

  // Select which rows, cols to write
  if (nr > 0) M = (nr<=M ? nr : M); // write min(nr,M) rows
  if (nc > 0) N = (nc<=N ? nc : N); // write min(nc,N) cols

#if (1)
  if (msg) { os << msg << "\n"; }
  os << M << "  " << N << "\n";
#endif

  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) {
      os << std::setw(wdth) <<  (*this)(i,j) << " ";
      if (j && (0 == (j%nline)))
        os << "\n";
    }
    os << "\n";
  }
  os << std::endl;
  os.setf(flgs);  // restore stream settings
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::print_GP
(
  std::ostream& os, 
  int prec,           // sig. figs      [ 4] ios::scientific
  int wdth,           // output spacing [12]
  int nr,             // num rows to write (0 --> all)
  int nc              // num cols to write (0 --> all)
) const
//---------------------------------------------------------
{
  // save settings
  std::ios_base::fmtflags flgs = os.flags();

  os << std::setiosflags(std::ios::scientific);
//os << std::setiosflags(std::ios::fixed);
  os << std::setprecision(prec);

  int M = this->num_rows();
  int N = this->num_cols();

  if (nr > 0) M = (nr<=M ? nr : M); // write min(nr,M) rows
  if (nc > 0) N = (nc<=N ? nc : N); // write min(nc,N) cols

  for (int i=1; i<=M; ++i) {
    for (int j=1; j<=N; ++j) {
      os << std::setw(wdth) <<  (*this)(i,j) << " ";
    }
    os << "\n";
  }
  os << std::endl;
  // restore settings
  os.setf(flgs);
}



// I/O: Matlab (binary) .mat format
//#######################################
// Define Matlab constants
//#######################################
#define COL_ORDER   0   // col-major data
#define ROW_ORDER   1   // row-major data
#define DOUBLE_PREC 0   // double precision
#define SINGLE_PREC 1   // single precision

#define MACH_ID     0     // 80x87 format   (small-endian?)
//#define MACH_ID   1     // 6888x format   (big-endian?)
#define ORDER       COL_ORDER
#define PRECISION   DOUBLE_PREC
//#######################################

//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::m_save(FILE* fp, const char* name) const
//---------------------------------------------------------
{
  // Save matrix in Matlab ".mat" format (BINARY)
  // Note: For Win32, fp MUST be opened in "wb" mode

  if (!this->ok()) { umWARNING("Mat_COL<T>::m_save","Empty matrix"); return; }

  int M = this->num_rows();
  int N = this->num_cols();
  int i=0, j=0;

  umMATLAB mat;
  mat.type = 1000*MACH_ID + 100*ORDER + 10*PRECISION + 0;
  mat.m = M;
  mat.n = N;
  mat.imag = 0; // FALSE;
  mat.namlen = (name? (long)(strlen(name)+1) : (long)1);

  // write header
  fwrite(&mat,sizeof(umMATLAB),1,fp);
  // write name
  if (! name)
    fwrite("",sizeof(char),1,fp);
  else
    fwrite(name,sizeof(char),(int)(mat.namlen),fp);

  // write actual data
  if (false         && sizeof(T) == sizeof(double)) {
    fwrite(this->v_, sizeof(double),M*N,fp);
  } else {
    double val=0.0;
    for (j=1; j<=N; ++j) {
      for (i=1; i<=M; ++i) {
        // Write element(i,j) as a double
        val = (double)(col_[j][i]);
        fwrite(&val, sizeof(double),1,fp);
      }
    }
  }
}


//---------------------------------------------------------
template <typename T>
void Mat_COL<T>::m_load(FILE *fp, std::string& name)
//---------------------------------------------------------
{
  // loads a ".mat" file variable (MATLAB format)
  // imaginary parts ignored 

  umMATLAB mat;

  if (fread(&mat,sizeof(umMATLAB),1,fp) != 1)
    umERROR("E_FORMAT","m_load");
  if (mat.type >= 10000)          // don't load a sparse matrix
    umERROR("E_FORMAT","m_load");

  int m_flag = (mat.type/1000) % 10;
  int o_flag = (mat.type/100) % 10;
  int p_flag = (mat.type/10) % 10;
  int t_flag = (mat.type) % 10;
  if (m_flag != MACH_ID)          umERROR("E_FORMAT","m_load");
  if ( t_flag != 0 )              umERROR("E_FORMAT","m_load");
  if ( p_flag != DOUBLE_PREC && 
       p_flag != SINGLE_PREC )    umERROR("E_FORMAT","m_load");

  int len = mat.namlen+1;
  char* buf = (char*)malloc((unsigned)(len*sizeof(char)));
  if (!fread(buf,sizeof(char),(unsigned)mat.namlen,fp))
    umERROR("E_FORMAT","m_load");
  buf[len]='\0';
  name = buf;

  int M=mat.m, N=mat.n;
  this->resize(M,N);

  float  f_temp=0.0f;
  double d_temp=0.0;
  int i=0, iR=0, jC=0;

  if (false         && (p_flag == DOUBLE_PREC) && 
      (o_flag == COL_ORDER)   &&
      (sizeof(T) == sizeof(double)))
  {
    // read data directly into allocation
    fread(this->v_, sizeof(double),M*N,fp);
  }
  else
  {
    // load one element at a time
    for (i=0; i<M*N; ++i)
    {
      if (p_flag == DOUBLE_PREC) {
        fread(&d_temp,sizeof(double),1,fp);
      } else {
        fread(&f_temp,sizeof(float),1,fp);
        d_temp = double(f_temp);          // cast float to double
      }

      if (o_flag == ROW_ORDER)
      {
        iR = 1 + i/N;   // 1-based row index
        jC = 1 + i%N;   // 1-based col index

        this->col_[jC][iR] = (T)(d_temp); // cast double to <T>
      //(*this)(iR,jC)     = (T)(d_temp); // cast double to <T>
      }
      else if ( o_flag == COL_ORDER ) 
      {
        this->v_[i] = (T)(d_temp);        // cast double to <T>
      } 
      else 
      {
        umERROR("E_FORMAT","m_load");
      }
    }
  }

  // skip imaginary part
  if (mat.imag) 
  {
    if (p_flag == DOUBLE_PREC) {
      for (i=0; i<M*N; ++i) {fread(&d_temp,sizeof(double),1,fp);}
    } else {
      for (i=0; i<M*N; ++i) {fread(&f_temp,sizeof(float),1,fp);}
    }
  }
}

//#######################################
// Undefine Matlab constants
//#######################################
#undef COL_ORDER
#undef ROW_ORDER
#undef DOUBLE_PREC
#undef SINGLE_PREC

#undef MACH_ID
#undef ORDER
#undef PRECISION
//#######################################



///////////////////////////////////////////////////////////
//
// Basic matrix algorithms
//
///////////////////////////////////////////////////////////


// DMat& inv  (const DMat& A);
// DMat& trans(const DMat& A);
// D...& abs  (const DMat& A) | DVec V | MappedRegion[1,2]);
// D...& sqrt (const DMat& A) | DVec V);
// D...& sqr  (const DMat& A) | DVec V);
// D...& exp  (const DMat& A) | DVec V);
// D...& pow  (const DMat& A) | DVec V);
// D...& max  (Mat A,B)       | (Vec A,B);
//
// DVec& {sin,cos,asin,acos}(const DVec& V);
//
// D...& max  + mappedregion2D
// 


//---------------------------------------------------------
// only defined for <T> = double
inline DMat& inv(const DMat& A)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "inv(%s)", A.name()); 

  DMat *tmp=new DMat(A, OBJ_temp, buf);
  tmp->invert();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& trans(const Mat_COL<T> &A)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "trans(%s)", A.name()); 

  // constructor deletes A (if temporary)
  Mat_COL<T>* tmp = new Mat_COL<T>(A, OBJ_temp, buf);
  tmp->transpose();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& abs(const Mat_COL<T> &A)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "abs(%s)", A.name()); 

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, buf);
  tmp->set_abs();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& abs(const Vector<T>& V)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "abs(%s)", V.name()); 

  Vector<T> *tmp=new Vector<T>(V, OBJ_temp, buf);
  tmp->set_abs();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& abs(const MappedRegion1D< Vector<T> >& R)
//---------------------------------------------------------
{
  static char buf[20]={""};
  snprintf(buf, (size_t)19, "abs(map)"); 

  Vector<T> *tmp=new Vector<T>(R, OBJ_temp, buf);
  tmp->set_abs();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& abs(const MappedRegion2D< Mat_COL<T> >& R)
//---------------------------------------------------------
{
  static char buf[20]={""};
  snprintf(buf, (size_t)19, "abs(map)"); 

  Mat_COL<T> *tmp=new Mat_COL<T>(R, OBJ_temp, buf);
  tmp->set_abs();
  return (*tmp);
}



//---------------------------------------------------------
inline DMat& sqrt(const DMat& A)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "sqrt(%s)", A.name()); 

  DMat *tmp=new DMat(A, OBJ_temp, buf);
  tmp->SQRT();
  return (*tmp);
}


//---------------------------------------------------------
inline DVec& sqrt(const DVec& V)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "sqrt(%s)", V.name()); 

  DVec *tmp=new DVec(V, OBJ_temp, buf);
  tmp->SQRT();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& sqr(const Mat_COL<T>& A)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "(%s)^2", A.name()); 

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, buf);
  tmp->SQR();
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
Vector<T>& sqr(const Vector<T>& V)
//---------------------------------------------------------
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "(%s)^2", V.name()); 

  Vector<T> *tmp=new Vector<T>(V, OBJ_temp, buf);
  tmp->SQR();
  return (*tmp);
}


//---------------------------------------------------------
inline DMat& exp(const DMat& A)
//---------------------------------------------------------
{
  static char buf[100]={""}; snprintf(buf, (size_t)90, "exp(%s)", A.name()); 
  DMat *tmp=new DMat(A, OBJ_temp, buf);
  tmp->exp_val();
  return (*tmp);
}


//---------------------------------------------------------
inline DVec& exp(const DVec& V)
//---------------------------------------------------------
{
  static char buf[100]={""}; snprintf(buf, (size_t)90, "exp(%s)", V.name()); 
  DVec *tmp=new DVec(V, OBJ_temp, buf);
  tmp->exp_val();
  return (*tmp);
}


//---------------------------------------------------------
inline DMat& log(const DMat& A)
//---------------------------------------------------------
{
  static char buf[100]={""}; snprintf(buf, (size_t)90, "log(%s)", A.name()); 
  DMat *tmp=new DMat(A, OBJ_temp, buf);
  tmp->log();
  return (*tmp);
}


//---------------------------------------------------------
inline DVec& log(const DVec& V)
//---------------------------------------------------------
{
  static char buf[100]={""}; snprintf(buf, (size_t)90, "log(%s)", V.name()); 
  DVec *tmp=new DVec(V, OBJ_temp, buf);
  tmp->log();
  return (*tmp);
}


//---------------------------------------------------------
inline DMat& pow(const DMat& A, double x)
//---------------------------------------------------------
{
  static char buf[100]={""}; snprintf(buf, (size_t)90, "(%s)^%g", A.name(), x); 
  DMat *tmp=new DMat(A, OBJ_temp, buf);
  tmp->pow_val(x);
  return (*tmp);
}


//---------------------------------------------------------
inline DVec& pow(const DVec& V, double x)
//---------------------------------------------------------
{
  static char buf[100]={""}; snprintf(buf, (size_t)90, "(%s)^%g", V.name(), x); 
  DVec *tmp=new DVec(V, OBJ_temp, buf);
  tmp->pow_val(x);
  return (*tmp);
}


// max/min DMat
// Note: templated version confuses mapped regions
//---------------------------------------------------------
inline DMat& max(const DMat& A, const DMat& B)
//---------------------------------------------------------
{
  int M=A.num_rows(), N=A.num_cols(), len=A.size();
  assert(B.num_rows()==M && B.num_cols()==N);  // assume matching dimensions

  // constructor deletes A if OBJ_temp
  DMat *tmp=new DMat(A, OBJ_temp, "MAX(A,B)");
  double *a=tmp->data(); const double *b=B.data();
  // operate over vector data
  for (int i=0; i<len; ++i) { 
    a[i] = std::max(a[i],b[i]);
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}
//---------------------------------------------------------
inline DMat& min(const DMat& A, const DMat& B)
//---------------------------------------------------------
{
  int M=A.num_rows(), N=A.num_cols(), len=A.size();
  assert(B.num_rows()==M && B.num_cols()==N);  // assume matching dimensions

  // constructor deletes A if OBJ_temp
  DMat *tmp=new DMat(A, OBJ_temp, "MIN(A,B)");

  double *a=tmp->data(); const double *b=B.data();
  // operate over vector data
  for (int i=0; i<len; ++i) {
    a[i] = std::min(a[i],b[i]);
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


// max/min DVec
// Note: templated version confuses mapped regions
//---------------------------------------------------------
inline DVec& max(const DVec& A, const DVec& B)
//---------------------------------------------------------
{
  int len=A.size();
  DVec *tmp=new DVec(A, OBJ_temp, "MAX(A,B)");
  assert(B.size()==len);  // assume matching dimension
  double *a=tmp->data(); const double *b=B.data();
  for (int i=0; i<len; ++i) {
    a[i] = std::max(a[i],b[i]);
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}
//---------------------------------------------------------
inline DVec& min(const DVec& A, const DVec& B)
//---------------------------------------------------------
{
  int len=A.size();
  DVec *tmp=new DVec(A, OBJ_temp, "MIN(A,B)");
  assert(B.size()==len);  // assume matching dimension
  double *a=tmp->data(); const double *b=B.data();
  for (int i=0; i<len; ++i) {
    a[i] = std::min(a[i],b[i]);
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


// max/min IVec
// Note: templated version confuses mapped regions
//---------------------------------------------------------
inline IVec& max(const IVec& A, const IVec& B)
//---------------------------------------------------------
{
  int len=A.size();
  IVec *tmp=new IVec(A, OBJ_temp, "MAX(A,B)");
  assert(B.size()==len);  // assume matching dimension
  int *a=tmp->data(); const int *b=B.data();
  for (int i=0; i<len; ++i) {
    a[i] = std::max(a[i],b[i]);
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}
//---------------------------------------------------------
inline IVec& min(const IVec& A, const IVec& B)
//---------------------------------------------------------
{
  int len=A.size();
  IVec *tmp=new IVec(A, OBJ_temp, "MIN(A,B)");
  assert(B.size()==len);  // assume matching dimension
  int *a=tmp->data(); const int *b=B.data();
  for (int i=0; i<len; ++i) {
    a[i] = std::min(a[i],b[i]);
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


//---------------------------------------------------------
inline DVec& max(double x, const DVec& A)
//---------------------------------------------------------
{
  int len=A.size();
  DVec *tmp=new DVec(A, OBJ_temp, "MAX(x,A)");
  double *a=tmp->data();
  for (int i=0; i<len; ++i) {
    a[i] = std::max(a[i],x);
  }
  return (*tmp);
}
//---------------------------------------------------------
inline DVec& min(double x, const DVec& A)
//---------------------------------------------------------
{
  int len=A.size();
  DVec *tmp=new DVec(A, OBJ_temp, "MIN(x,A)");
  double *a=tmp->data();
  for (int i=0; i<len; ++i) {
    a[i] = std::min(a[i],x);
  }
  return (*tmp);
}


//---------------------------------------------------------
// Add trig functions as required
//---------------------------------------------------------

//
// sin
//
inline DMat& sin(const DMat& X)
{
  char buf[100]={""}; snprintf(buf, (size_t)90, "sin(%s)", X.name());
  DMat *tmp=new DMat(X, OBJ_temp, buf); 
  tmp->apply(sin);
  return (*tmp);
}
inline DVec& sin(const DVec& X)
{
  char buf[100]={""}; snprintf(buf, (size_t)90, "sin(%s)", X.name());
  DVec *tmp=new DVec(X, OBJ_temp, buf); 
  tmp->apply(sin);
  return (*tmp);
}


//
// cos
//
inline DMat& cos(const DMat& X)
{
  char buf[100]={""}; snprintf(buf, (size_t)90, "cos(%s)", X.name());
  DMat *tmp=new DMat(X, OBJ_temp, buf); 
  tmp->apply(cos);
  return (*tmp);
}
inline DVec& cos(const DVec& X)
{
  char buf[100]={""}; snprintf(buf, (size_t)90, "cos(%s)", X.name());
  DVec *tmp=new DVec(X, OBJ_temp, buf); 
  tmp->apply(cos);
  return (*tmp);
}


inline DVec& asin(const DVec& X)
{
  char buf[100]={""}; snprintf(buf, (size_t)90, "asin(%s)", X.name());
  DVec *tmp=new DVec(X, OBJ_temp, buf); 
  tmp->apply(asin);
  return (*tmp);
}

inline DVec& acos(const DVec& X)
{
  char buf[100]={""}; snprintf(buf, (size_t)90, "acos(%s)", X.name());
  DVec *tmp=new DVec(X, OBJ_temp, buf); 
  tmp->apply(acos);
  return (*tmp);
}


inline DVec& atan2(const DVec& Y, const DVec& X)
{
  char buf[]={"atan2(y,x)"};
  int N=Y.size(); 
  assert((X.size()==N) && (X.min_val_abs()>0.0));

  DVec *tmp=new DVec(N, buf, OBJ_temp); 
  for (int i=1; i<=N; ++i) {
    (*tmp)(i) = atan2(Y(i),X(i));
  }
  return (*tmp);
}




//---------------------------------------------------------
// Addition:
//---------------------------------------------------------
//   matrix + matrix
//   matrix + vector
//   vector + matrix
//   matrix + scalar
//   scalar + matrix
//
//   region + region            (for Regions, see below)
//   matrix + region
//   region + matrix
//   matrix + mapped region
//   mapped region + matrix
//
//   mapped region2D + scalar   (R+x)
//   scalar + mapped region2D   (x+R)
//
//   mapped region2D + vector   (R+V)
//   vector + mapped region2D   (V+R)
//---------------------------------------------------------



// matrix + matrix
template <typename T> inline
Mat_COL<T>& operator+(const Mat_COL<T> &A, const Mat_COL<T> &B)
{
#if (USE_ARRAY_NAMES)
  //
  // NBN: eventually we may want to avoid using names.
  //      Use simple stubs during transition?
  //
  std::string sz; tmp_op_name(A.name(),"+",B.name(), sz);
#else
  static std::string sz("mat");
#endif

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}

// matrix + vector
template <typename T> inline 
Mat_COL<T>& operator+(const Mat_COL<T> &A, const Vector<T> &V)
{
  std::string sz; tmp_op_name(A.name(),"+",V.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += V;
  return (*tmp);
}

// vector + matrix
template <typename T> inline 
Mat_COL<T>& operator+(const Vector<T> &V, const Mat_COL<T> &A)
{
  std::string sz; tmp_op_name(V.name(),"+",A.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += V;
  return (*tmp);
}

// matrix + scalar
template <typename T> inline 
Mat_COL<T>& operator+(const Mat_COL<T> &A, const T &x)
{
  std::string sz; tmp_op_name(A.name(),"+","x", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}

// scalar + matrix
template <typename T> inline 
Mat_COL<T>& operator+(const T &x, const Mat_COL<T> &A)
{
  std::string sz; tmp_op_name("x","+",A.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


//---------------------------------------------------------
// Subtraction:
//---------------------------------------------------------
//   matrix - matrix
//   matrix - vector
//   vector - matrix
//   matrix - scalar
//   scalar - matrix
//
//   region - region            (for Regions, see below)
//
//   matrix - mapped region
//   mapped region - matrix
//
//   mapped region2D - scalar
//   scalar - mapped region2D
//
//   mapped region2D - vector
//   vector - mapped region2D
//---------------------------------------------------------


// matrix - matrix
template <typename T> inline 
Mat_COL<T>& operator-(const Mat_COL<T> &A, const Mat_COL<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"-",B.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}

// matrix - vector
template <typename T> inline 
Mat_COL<T>& operator-(const Mat_COL<T> &A, const Vector<T> &V)
{
  std::string sz; tmp_op_name(A.name(),"-",V.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= V;
  return (*tmp);
}

// vector - matrix
template <typename T> inline 
Vector<T>& operator-(const Vector<T> &V, const Mat_COL<T> &A)
{
  std::string sz; tmp_op_name(V.name(),"-",A.name(), sz);

  Vector<T> *tmp=new Vector<T>(V, OBJ_temp, sz.c_str());
  (*tmp) -= (const Vector<T>&)(A);
  return (*tmp);
}

// matrix - scalar
template <typename T> inline 
Mat_COL<T>& operator-(const Mat_COL<T> &A, const T &x)
{
  std::string sz; tmp_op_name(A.name(),"-","x", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= x;
  return (*tmp);
}

// scalar - matrix
template <typename T> inline 
Mat_COL<T>& operator-(const T &x, const Mat_COL<T> &A) 
{ 
  //
  // create a matrix filled with scalar x; subtract A
  //
  std::string sz; tmp_op_name("x","-",A.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A.num_rows(), A.num_cols(), x, OBJ_temp, sz.c_str());
  (*tmp) -= A;
  return (*tmp);
}



//---------------------------------------------------------
// Division :
//---------------------------------------------------------
// 1a :   matrix | matrix : "left division"  A\B via LU factorization
// 1b :   matrix | vector : "left division"  A\b via LU factorization
//
// 2a :   matrix / matrix : "right division" B/A ==> B*inv(A)
// 2b :   vector / matrix : "right division" b/A ==> b*inv(A)

// 3a :   matrix / scalar
// 3b :   scalar / matrix
//
// 4a:    Region2D ./  Region2D   (for Regions, see below)
// 4b:    Region1D ./  Region1D
//
// 5a:    MappedRegion2D ./ MappedRegion2D
// 5b:    MappedRegion1D ./ MappedRegion1D
//---------------------------------------------------------

// 1a :  matrix | matrix
template <typename T> inline
Mat_COL<T>& operator| (const Mat_COL<T> &A, const Mat_COL<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"|",B.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) |= B;
  return (*tmp);
}


// 1b :  matrix | Vector
template <typename T> inline
Vector<T>& operator| (const Mat_COL<T> &A, const Vector<T> &b)
{
  std::string sz; tmp_op_name(A.name(),"|",b.name(), sz);

  Vector<T> *x=new Vector<T>(sz.c_str(), OBJ_temp);
  umSOLVE(A, b, (*x));
  return (*x);
}


// 2a :  matrix / matrix
template <typename T> inline 
Mat_COL<T>& operator/ (const Mat_COL<T> &A, const Mat_COL<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"/",B.name(), sz);

  // Matlab's A\B and B/A
  //-------------------------------------------------------
  //  B/A ->    B*inv(A) ->   B/A =   ( A'    \   B'  )'
  // ------    -----------    -----------------------------
  //   X                          = tr( tr(A) | tr(B) );
  //-------------------------------------------------------

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= B;
  return (*tmp);
}


// 2b :  vector / matrix : "right division" b/A ==> b*inv(A)

inline  // specialization for <T>=<double>
DVec& operator/ (const DVec &b, const DMat &A)
{
  // Matlab's A\B and B/A
  //-------------------------------------------------------
  //  b/A ->    b*inv(A) ->   b/A =   ( A'    \ b )'
  // ------    -----------    -----------------------------
  //   x                          = tr( tr(A) | b );
  //-------------------------------------------------------

  std::string sz; tmp_op_name(b.name(),"/",A.name(), sz);
  DVec *tmp=new DVec(sz.c_str(), OBJ_temp);
  (*tmp) = trans(A)|b;
  return (*tmp);
}



// 3a :  matrix / scalar
template <typename T> inline
Mat_COL<T>& operator/ (const Mat_COL<T> &A, const T &x) 
{
  assert(fabs(x)>0.0); 

  std::string sz; tmp_op_name(A.name(),"/","x", sz);
  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= x;
  return (*tmp);
}


// 3b :  scalar / matrix
template <typename T> inline 
Mat_COL<T>& operator/ (const T &x, const Mat_COL<T> &A) 
{ 
  //
  // create a matrix filled with scalar x; div_element(A)
  //
  std::string sz; tmp_op_name("x","/",A.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A.num_rows(), A.num_cols(), x, OBJ_temp, sz.c_str());
  tmp->div_element(A);
  return (*tmp);
}


//---------------------------------------------------------
// Multiplication :
//---------------------------------------------------------
// 1a :  DMat   * DMat
// 1b :  mat<T> * mat<T>
//
// 2 :  matrix * scalar
// 3 :  scalar * matrix
//
// 4a:  matrix * vector : C = A  * v  ... if (rows,cols) match, 
// 4b:  matrix * vector : C = A .* v  ... else if size() match.
//
// 5a:  vector * matrix : c = v  * A  ... if (rows,cols) match, 
// 5b:  vector * matrix : c = v .* A  ... else if size() match.
//
//
// 6a:       Region2D .* Region2D   (for Regions, see below)
// 6b:       Region1D .* Region1D
// 6c:       matrix    * Region1D       (i.e. matrix*vector)
// 6d:       matrix    * MappedRegion1D (i.e. matrix*vector)
//
// 7a: MappedRegion2D .* MappedRegion2D
// 7b: MappedRegion1D .* MappedRegion1D
//
//
//---------------------------------------------------------

// 1a :  DMat * DMat
inline 
DMat& operator*(const DMat& A, const DMat& B)
{
  std::string sz; tmp_op_name(A.name(),"*",B.name(), sz);
  DMat* C = new DMat(sz.c_str(), OBJ_temp);
  umAxB (A, B, (*C));  // calls BLAS DGEMM

  if (A.get_mode() == OBJ_temp) { delete (&A);  }
  if (B.get_mode() == OBJ_temp) { delete (&B);  }
  return (*C);
}


// 1b :  mat<T> * mat<T>
template <typename T> inline 
Mat_COL<T>& operator*(const Mat_COL<T> &A, const Mat_COL<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"*",B.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= B;
  return (*tmp);
}


// 2 :  matrix * scalar
template <typename T> inline 
Mat_COL<T>& operator*(const Mat_COL<T> &A, const T &x)
{
  std::string sz; tmp_op_name(A.name(),"*","x", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// 3 :  scalar * matrix
template <typename T> inline 
Mat_COL<T>& operator*(const T &x, const Mat_COL<T> &A) 
{
  std::string sz; tmp_op_name("x","*",A.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// 4a:  matrix * vector : C = A  * v  ... if (rows,cols) match, 
// 4b:  matrix * vector : C = A .* v  ... else if size() match.
template <typename T> inline
Vector<T>& operator* (const Mat_COL<T> &A, const Vector<T> &V) 
{
  int M=A.num_rows(), K=A.num_cols(), N=V.size();

  if (K==N) 
  {
    return matmult(A,V);                // usual A * v
  } 
  else if ( M*K == N ) 
  {
    return ((const Vector<T> &)A) * V;  // Matlab A .* v
  //return (Vector<T>&) (A.dm(V)); // Matlab A .* v
  }
  else
  { 
    umERROR("matrix * vector", "incompatible dimensions"); 
    Vector<T>* dum = new Vector<T>(1);
    return (*dum);
  }
}


// 5a:  vector * matrix : c = v  * A  ... if (rows,cols) match, 
// 5b:  vector * matrix : c = v .* A  ... else if size() match.
template <typename T> inline
Vector<T>& operator* (const Vector<T> &V, const Mat_COL<T> &A) 
{ 
  int M=A.num_rows(), K=A.num_cols(), N=V.size();

  if (N==M) 
  {
    return matmult(V,A);                // usual v * A
  } 
  else if ( N == M*K ) 
  {
    return (V * ((const Vector<T> &)A));  // Matlab v .* A
  //return (Vector<T>&)  (A.dm(V));  // Matlab v .* A
  }
  else
  { 
    umERROR("vector * matrix", "incompatible dimensions"); 
    Vector<T>* dum = new Vector<T>(1);
    return (*dum);
  }
}


///////////////////////////////////////////////////////////
//
// outer product of vectors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline 
Mat_COL<T>& outer(const Vector<T> &A, const Vector<T> &B)
//---------------------------------------------------------
{
  std::string sz = umOFORM("outer(%s,%s)", A.name(), B.name());

  // Each column of result is a scalar multiple of the
  // vector A. If A is OBJ_temp, we need to delay its 
  // deletion until after all columns have been set. 
  // Do this by temporarily adjusting its mode:

  Vector<T>& Aref = const_cast< Vector<T>& >(A);
  bool reset_temp_A=false;
  if (OBJ_temp == A.get_mode()) {
    reset_temp_A = true;      // set flag
    Aref.set_mode(OBJ_real);  // avoid early deletion
  }


  int M = A.size(), N = B.size();
  Mat_COL<T> *tmp=new Mat_COL<T>(M,N, sz.c_str(), OBJ_temp);
  T x=0;
  for (int j=1; j<=N; ++j) {
    x=B(j);
    if (x != T(0))
    {
      if (T(1) == x) {
        (*tmp).set_col(j, A  );   // case of outer(V, ones(K))
      } else {
        (*tmp).set_col(j, A*x);   // general case
      }
    }
  }

  if (reset_temp_A) { 
    Aref.set_mode(OBJ_temp);      // A was temporary,
    delete (&A);                  // so delete it here.
  }

  if (B.get_mode() == OBJ_temp) {delete (&B);}
  return (*tmp);
}


// negation (unary operator)
template <typename T> inline 
Mat_COL<T>& operator- (const Mat_COL<T> &A) 
{
  std::string sz; tmp_op_name(" ","-",A.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= T(-1);
  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// Matlab "element-wise" operations:  C = A .* B
//                                    C = A ./ B
//
///////////////////////////////////////////////////////////

template <typename T> inline 
void mult_element(const Mat_COL<T> &A, const Mat_COL<T> &B,  Mat_COL<T> &C) 
{ 
  C = A; 
  C.mult_element(B); 
}


template <typename T> inline 
void div_element(const Mat_COL<T> &A,  const Mat_COL<T> &B,  Mat_COL<T> &C) 
{
  C = A; 
  C.div_element(B); 
}



///////////////////////////////////////////////////////////
//
// Matlab "find" operations
//
///////////////////////////////////////////////////////////


template <typename T> inline 
IMat& find2D(const Mat_COL<T> &A, char op, T val)
{
  // Return both row and column info.
  // See also Vector<T>::find(...)

  std::string sz;
  if (sizeof(T) == sizeof(double)) {
    sz=umOFORM("find(%s %c %g)", A.name(), op, val);
  } else {
    sz=umOFORM("find(%s %c %d)", A.name(), op, val);
  }

  int Nr=A.num_rows(), Nc=A.num_cols();
  IMat mask(Nr,Nc);
//IVec rowmask(Nr);
  int count=0, i=0, j=0, rh=0;

  switch (op) {

  case '<':   // find ids of elements less than val
    //-----------------------------------------------------
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (A(i,j) < val) {
          mask(i,j) = 1;    // elem (i,j) satisfies condition
        //++rowmask(i);     // row i has another match
          ++count;          // increment total hits
        }
      }
    }
    break;

  case '=':   // find ids of elements that equal val
    //-----------------------------------------------------
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (A(i,j) == val) {
          mask(i,j) = 1;    // elem (i,j) satisfies condition
        //++rowmask(i);     // row i has another match
          ++count;          // increment total hits
        }
      }
    }
    break;

  case '>':   // find ids of elements greater than val
    //-----------------------------------------------------
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (A(i,j) > val) {
          mask(i,j) = 1;    // elem (i,j) satisfies condition
        //++rowmask(i);     // row i has another match
          ++count;          // increment total hits
        }
      }
    }
    break;

  }

  IMat *tmp = new IMat(sz.c_str(), OBJ_temp);
  if (count>0)
  {

/*
    // extract find() results
    tmp->resize(count, 2);
    int sk=0;
    for (i=1; i<=Nr; ++i) {
      if (rowmask(i) > 0) {
        // extract hits in ith row
        for (j=1; j<=Nc; ++j) {
          if (mask(i,j)) {
            ++sk;
            (*tmp)(sk, 1) = i;
            (*tmp)(sk, 2) = j;
          }
        }
      }
    }
*/
    //#####################################################

    //-------------------------------------------
    // to match Matlab, return results by column
    //-------------------------------------------

    // extract find() results
    tmp->resize(count, 2);
    int sk=0;
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (mask(i,j)) {
          ++sk;
          (*tmp)(sk, 1) = i;
          (*tmp)(sk, 2) = j;
        }
      }
    }
    //#####################################################

    assert(count == sk);
  }

  if (A.get_mode() == OBJ_temp) { delete (&A); }
  return (*tmp);
}


template <typename T> inline 
IMat& find2D(const Mat_COL<T> &A, char op, const Mat_COL<T> &B)
{
  // Return both row and column info.
  // See also Vector<T>::find(...)

  std::string sz;
  sz=umOFORM("find(%s %c %s)", A.name(), op, B.name());

  int Nr=A.num_rows(), Nc=A.num_cols();
  IMat mask(Nr,Nc);
//IVec rowmask(Nr);
  int count=0, i=0, j=0, rh=0;

  switch (op) {

  case '<':   // find ids of elements less than val
    //-----------------------------------------------------
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (A(i,j) < B(i,j)) {
          mask(i,j) = 1;    // elem (i,j) satisfies condition
        //++rowmask(i);     // row i has another match
          ++count;          // increment total hits
        }
      }
    }
    break;

  case '=':   // find ids of elements that equal val
    //-----------------------------------------------------
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (A(i,j) == B(i,j)) {
          mask(i,j) = 1;    // elem (i,j) satisfies condition
        //++rowmask(i);     // row i has another match
          ++count;          // increment total hits
        }
      }
    }
    break;

  case '>':   // find ids of elements greater than val
    //-----------------------------------------------------
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (A(i,j) > B(i,j)) {
          mask(i,j) = 1;    // elem (i,j) satisfies condition
        //++rowmask(i);     // row i has another match
          ++count;          // increment total hits
        }
      }
    }
    break;

  }

  IMat *tmp = new IMat(sz.c_str(), OBJ_temp);
  if (count>0)
  {

    //-------------------------------------------
    // to match Matlab, return results by column
    //-------------------------------------------
    // extract find() results
    tmp->resize(count, 2);
    int sk=0;
    for (j=1; j<=Nc; ++j) {
      for (i=1; i<=Nr; ++i) {
        if (mask(i,j)) {
          ++sk;
          (*tmp)(sk, 1) = i;
          (*tmp)(sk, 2) = j;
        }
      }
    }
    assert(count == sk);
  }

  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


//---------------------------------------------------------
template <typename T> inline
void matmult
( 
  const Mat_COL<T> &A, 
  const Mat_COL<T> &B,
        Mat_COL<T> &C 
)
//---------------------------------------------------------
{
  assert(A.num_cols() == B.num_rows());
  int M = A.num_rows();
  int N = A.num_cols();
  int K = B.num_cols();

  T ZERO = T(0), sc = T(0);
  T* colk = NULL;

  // Adjust shape of C, and set to zero
  C.resize(M,K, true, ZERO);

  // Col major:  each column of C is a 
  // vector sum, updated by saxpy ops
  for (int k=1; k<=K; ++k) 
  {
    colk = C[k]; // pointer to start of C(:,k)

    for (int j=1; j<=N; ++j) {
      sc = B(j,k);
      if (sc != ZERO) {
        for (int i=1; i<=M; ++i) {
        // C(i,k) += sc * A(i,j);
          colk[i] += sc * A(i,j);
        }
      }
    }
  }

  // This overload is called by A*=B for (T != double)
  // In that case, A is the calling object and so we 
  // must NOT delete A.  (Note: to make sure, we now
  // manipulate A's mode so that A.mode = OBJ_real.)

//if (A.get_mode() == OBJ_temp) delete (&A);
  if (B.get_mode() == OBJ_temp) delete (&B);
}


// matrix-vector product: BLAS 2
//---------------------------------------------------------
inline  // specialization for <T>=<double>
DVec& matmult(const DMat &A, const DVec &V)
//---------------------------------------------------------
{
  // Returns "col" vector: x = A*v

  int rows=A.num_rows(), cols=A.num_cols();
  if (V.size() != cols) { umERROR("Mat*Vec", "wrong dimensions"); }
  DVec* X = new DVec(rows, 0.0, OBJ_temp, "A*v");
  double alpha=1.0, beta=0.0; int inc = 1;
  
  GEMV ('N', rows, cols, alpha, 
        A.data(),  rows, 
        V.data(),  inc, beta, 
        X->data(), inc);

  // delete temporary objects
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }

  return (*X);
}


// matrix-vector product: general
//---------------------------------------------------------
template <typename T> inline 
Vector<T>& matmult(const Mat_COL<T> &A, const Vector<T> &V)
//---------------------------------------------------------
{
  // Returns "col" vector: x = A*v

  int rows=A.num_rows(), cols=A.num_cols(); T ZERO=T(0);
  if (V.size() != cols) { umERROR("Mat*Vec", "wrong dimensions"); }
  Vector<T>* X = new Vector<T>(rows, ZERO, OBJ_temp, "A*v");

  // Col major: X=Sum(v(j)*A(j)). X is 
  // a vector sum, updated by saxpy ops
  double* px = X->d_m1();
  T sc = ZERO;
  for (int j=1; j<=cols; ++j) {
    sc = V(j);
    if (sc != ZERO) {
      for (int i=1; i<=rows; ++i) {
        //(*X)(i) += sc * A(i,j);
        px[i]   += sc * A(i,j);
      }
    }
  }

  // delete temporary objects
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }

  return (*X);
}


// vector-matrix product: BLAS 2
//---------------------------------------------------------
inline  // specialization for <T>=<double>
DVec& matmult(const DVec &V, const DMat &A)
//---------------------------------------------------------
{
  // Returns "row" vector: x =  v*A  =  A' * v

  int rows=A.num_rows(), cols=A.num_cols();
  if (V.size() != rows) { umERROR("umVec * umMat", "wrong dimensions"); }
  DVec* X = new DVec(cols, 0.0, OBJ_temp, "v*A");
  double alpha=1.0, beta=0.0; int inc = 1;

  GEMV ('T', rows, cols, alpha, 
        A.data(),  rows, 
        V.data(),  inc, beta, 
        X->data(), inc);

  // delete temporary objects
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }

  return (*X);
}


// vector-matrix product: general
//---------------------------------------------------------
template <typename T> inline 
Vector<T>& matmult (const Vector<T> &V, const Mat_COL<T> &A)
//---------------------------------------------------------
{
  // Returns "row" vector: x =  v*A  =  A' * v

  int rows=A.num_rows(), cols=A.num_cols(); T ZERO = T(0);
  if (V.size() != rows) { umERROR("umVec * umMat", "wrong dimensions"); }
  Vector<T>* X = new Vector<T>(cols, ZERO, OBJ_temp, "v*A");

  // Col major: X(i) = dot product
  T* px = X->d_m1();
  for (int j=1; j<=cols; ++j) {
    const T* Aj = A[j]+1; // shift to 0-based data
    px[j] = V.inner(rows, Aj); 
  }

  // delete temporary objects
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }

  return (*X);
}


// Returns Kronecker tensor product of A and B.
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& kron(const Mat_COL<T>& A, const Mat_COL<T>& B)
//---------------------------------------------------------
{
  return A.kron(B);
}

//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& reverse_rows(const Mat_COL<T>& A)
//---------------------------------------------------------
{
  // reverse the row order, i.e. reproduce Matlab 
  // operation: B = A(end:-1:1,:);

#if (1)

  Mat_COL<T>* tmp = new Mat_COL<T>(A, OBJ_temp);
  tmp->reverse_rows();
  return (*tmp);

#else

  int Nr = A.num_rows(), Nc = A.num_cols();
  Mat_COL<T>* tmp = new Mat_COL<T>(Nr,Nc, T(0), OBJ_temp);
  for (int i=1; i<=Nr; ++i) {
    tmp->set_row(i, A.get_row((Nr-i)+1));
  }
  // delete temporary objects
  if (A.get_mode() == OBJ_temp) { delete (&A); }
  return (*tmp);

#endif

}



// In-place update of user-provided matrix C = A * B
// Calls to each permutation of transposed GEMM
void umAtransxB(const DMat& A,const DMat& B, DMat& C); // C = A^T* B
void umAxBtrans(const DMat& A,const DMat& B, DMat& C); // C = A  * B^T


///////////////////////////////////////////////////////////
//
// LAPACK: add drivers as required
//
///////////////////////////////////////////////////////////


// Routines that use LU factorization
void    umSOLVE(const DMat& mat, const DVec& b, DVec& x);
void    umSOLVE(const DMat& mat, const DMat& B, DMat& X);

// Routines that use Cholesky factorization
void    umSOLVE_CH(const DMat& mat, const DVec& b, DVec& x);
void    umSOLVE_CH(const DMat& mat, const DMat& B, DMat& X);

// Generalized least squares solution
void    umSOLVE_LS(const DMat& mat, const DMat& B, DMat& X);

// Singular value decomposition
DVec&   svd(const DMat& mat, DMat& U, DMat& VT, char ju='A', char jvt='A');
DVec&   svd(const DMat& mat);

double  norm(const DMat& mat);
double  cond(const DMat& mat);

void    eig(const DMat& A, DVec& Re);           // DGEEV
void    eig(const DMat& A, DVec& Re, DMat& VR); // DGEEV
void    eig(const DMat& A, DVec& Re, DMat& VL, DMat& VR, bool bL, bool bR); 
void    eig_sym(const DMat& A, DVec& ev, DMat& Q,  bool bDoEVecs=false); // DSYEV

//---------------------------------------
// lu factorization/solve
//---------------------------------------
DMat& lu(DMat& A, bool in_place=false);
bool  lu_solve(DMat& LU, const DMat& B, DMat& X);
DVec& lu_solve(DMat& LU, const DVec& b);

// Region1D
DVec& lu_solve(DMat& ch, Region1D<DVec> R);

//---------------------------------------
// Cholesky factorization/solve
//---------------------------------------
DMat& chol(DMat& A, bool in_place=false);
bool  chol_solve(const DMat& ch, const DMat& B, DMat& X);
DVec& chol_solve(const DMat& ch, const DVec& rhs);

// Region1D
DVec& chol_solve(const DMat& ch, Region1D<DVec> R);


//---------------------------------------
// qu factorization/solve
//---------------------------------------
DMat& qr(DMat& A, bool in_place=false);


// remove "numerical noise" from array elements
void umPOLISH(DVec& V, double eps = 1e-10);
void umPOLISH(DMat& A, double eps = 1e-10);


///////////////////////////////////////////////////////////
//
// member functions involving a subset of the matrix
//
///////////////////////////////////////////////////////////


template <typename T> inline
Vector<T>& Mat_COL<T>::get_map(const IVec& map) const
{
  // Return a vector of elements indexed by map
  // Enables the following syntax:
  //
  // VEC:  vec = Q.get_map(um->vmapR) - Q.get_map(um->vmapL);
  //
  // MAT:  Q_pec.set_col(j, fQ.get_map(um->mapPEC, j));

  return Vector<T>::vmap(map);
}


template <typename T> inline
Vector<T>& Mat_COL<T>::get_map(const IVec& map, int j) const
{
  // Return a vector of values mapped from column j

  static char buf[100]={""};
  snprintf(buf, (size_t)90, "vmap(%s,%d)", this->name(),j);
  CheckIdx_Col_1(j);
  int idx=0, len=map.size();
  Vector<T> *tmp=new Vector<T>(len, this->ZERO, OBJ_temp, buf);

  // Load elements indexed by map into result
  if (len > 0) {
    T* p = (*tmp).d_m1();   // 1-offset pointer
    for (int k=1; k<=len; ++k) {
      idx = map(k);
      CheckIdx_Row_1(idx);  // check (idx <= num_rows())
      p[k] = col_[j][idx];  // copy mapped element
    }
  }

  return (*tmp);
}


template <typename T> inline
Vector<T>& Mat_COL<T>::get_map(int i, const IVec& map) const
{
  // Return a vector of values mapped from row i

  static char buf[100]={""};
  snprintf(buf, (size_t)90, "vmap(%d,%s)", i,this->name());
  CheckIdx_Row_1(i);
  int idx=0, len=map.size();
  Vector<T> *tmp=new Vector<T>(len, this->ZERO, OBJ_temp, buf);

  // Load elements indexed by map into result
  if (len > 0) {
    T* p = (*tmp).d_m1();   // 1-offset pointer
    for (int k=1; k<=len; ++k) {
      idx = map(k);
      CheckIdx_Col_1(idx);  // check (idx <= num_cols())
      p[k] = col_[idx][i];  // copy mapped element
    }
  }

  return (*tmp);
}



template <typename T> inline
Mat_COL<T>& Mat_COL<T>::get_map(const IMat& map) const
{
  // Return selected elements as a new matrix
  // Elements are indexed by matrix map arg.

  int Nr=map.num_rows(), Nc=map.num_cols(), idx=0;
  Mat_COL<T> *tmp = new Mat_COL<T>(Nr, Nc, this->ZERO, OBJ_temp);
  Mat_COL<T>& ref = (*tmp);
  if ((Nr<1)||(Nc<1)) {ref.Free(); return ref;} // empty map

  // Load elements indexed by map into result
  // Note: Map contains 1-based indices into base vector
  for (int j=1; j<=Nc; ++j) {
    for (int i=1; i<=Nr; ++i) {
      idx = map(i,j);             // index into base array
      this->Check_index_1(idx);   // make sure index is valid
      ref(i,j) = this->vm1_[idx]; // copy mapped element to New matrix
    }
  }
  return (*tmp);
}


/*
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator()(const IMat& map) const
{
  // Return selected elements as a new matrix
  // Elements are indexed by matrix map arg.
  return get_map(map);
}
*/


// Set elements indicated by map to given value
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::set_map(const IMat& map, const T x)
{
  // Use base-class version
  Vector<T>::set_map((const IVec&)map, x);
  return (*this);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::set_map(const IMat& map, const Vector<T>& X)
{
  // Set elements indicated by map to corresponding 
  // elements in X.  Result is same shape as map.

  // check whether the integer entries in an IMat 
  // are being "re-mapped", e.g. EToV = gnum(EToV);
  bool remap_self = false;
  IMat* THIS = dynamic_cast<IMat*>(this);
  if (THIS) { 
    if ((&map) == THIS) { 
      remap_self = true; 
    } 
  }

  if (!remap_self) 
  {
    // make shape of result match shape of the map
    this->resize(map.num_rows(),map.num_cols(),false);
    // base-class version loads the mapped entries
    Vector<T>::set_map((const IVec&)map, X);
  }
  else
  {
    // The map is the same object as "this" matrix.
    // To re-map the elements in a matrix, we build 
    // a temporary matrix, then swap with self.

    OBJ_mode old_md = this->get_mode();       // (*this) may be OBJ_temp
    this->set_mode(OBJ_real);                 // avoid premature deletion

    IMat *tmp = new IMat(m_M,m_N,0,OBJ_temp); // alloc buffer
    IVec& B = dynamic_cast<IVec&>(*tmp);      // -> base class

    B.set_map((const IVec&)map, (IVec&)X);    // call base class version
    (*THIS) = (*tmp);                         // switch ownership
    this->set_mode(old_md);                   // restore original mode
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::concat_v
(
  const Mat_COL<T>& A1, 
  const Mat_COL<T>& A2
)
//---------------------------------------------------------
{
  // example of "vertical" concatenation

  int M1=A1.num_rows(), M2=A2.num_rows();
  int Nc=A1.num_cols();
  assert(A2.num_cols() == Nc); // num cols must match
  int totM = M1 + M2;
  Index1D I1(1, M1), I2(M1+1, totM), J(1,Nc);
  this->resize(totM, Nc);
  (*this)(I1,J) = A1;
  (*this)(I2,J) = A2;
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::concat_v
(
  const Mat_COL<T>& A1, 
  const Mat_COL<T>& A2, 
  const Mat_COL<T>& A3
)
//---------------------------------------------------------
{
  // example of "vertical" concatenation

  int M1=A1.num_rows(), M2=A2.num_rows(), M3=A3.num_rows();
  int Nc=A1.num_cols();
  assert(A2.num_cols() == Nc); // num cols must match
  assert(A3.num_cols() == Nc); // num cols must match
  int totM = M1 + M2 + M3;
  Index1D I1(1, M1), I2(M1+1, M1+M2), I3(M1+M2+1, totM), J(1,Nc);
  this->resize(totM, Nc);
  (*this)(I1,J) = A1;
  (*this)(I2,J) = A2;
  (*this)(I3,J) = A3;
  return (*this);
}



///////////////////////////////////////////////////////////
//
// member functions involving (contiguous) Region[1,2]D
//
///////////////////////////////////////////////////////////

// construct from Region2D
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const Region2D< Mat_COL<T> > &R, OBJ_mode md, const char *sz)
//---------------------------------------------------------
: Vector<T>(sz, md),
  m_M(0), m_N(0), m_MN(0), col_(0), 
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  (*this)=R;  // matrix = region
}

//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const const_Region2D< Mat_COL<T> > &R, OBJ_mode md, const char *sz)
//---------------------------------------------------------
: Vector<T>(sz, md),
  m_M(0), m_N(0), m_MN(0), col_(0), 
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  (*this)=R;  // matrix = region
}




template <typename T> inline
const_Region2D< Mat_COL<T> > 
Mat_COL<T>::operator()(const Index1D &I, const Index1D &J) const
{
  // return a const 2D region of (*this)
  return const_Region2D< Mat_COL<T> >(*this, I,J);
}


template <typename T> inline
Region2D< Mat_COL<T> > 
Mat_COL<T>::operator()(const Index1D &I, const Index1D &J)
{
  // return a 2D region of (*this)
  return Region2D< Mat_COL<T> >(*this, I,J);
}


template <typename T> inline
const_Region1D< Vector<T> > 
Mat_COL<T>::operator()(const Index1D& I, int j) const
{
  // return part of column j as a Region1D< V<T> >
  // (optimize assignment of a vector to this Region1D)
  int i1 = (j-1)*m_M + I.lo();
  int i2 = i1 + I.N() - 1;
  return const_Region1D< Vector<T> >( (const Vector<T>&)(*this), i1, i2);
}

template <typename T> inline
Region1D< Vector<T> > 
Mat_COL<T>::operator()(const Index1D& I, int j)
{
  // return part of column j as a Region1D< V<T> >
  // (optimize assignment of a vector to this Region1D)
  int i1 = (j-1)*m_M + I.lo();
  int i2 = i1 + I.N() - 1;
  return Region1D< Vector<T> >( (Vector<T>&)(*this), i1, i2);
}


template <typename T> inline
const_Region1D< Vector<T> > 
Mat_COL<T>::operator()(const Index1D &I) const
{ 
  // Return range of values as a Region1D< V<T> >
  //  A(I) = V = F(I);
  return const_Region1D< Vector<T> >( (const Vector<T>&)(*this), I);
}

template <typename T> inline
Region1D< Vector<T> > 
Mat_COL<T>::operator()(const Index1D &I)
{ 
  // Return range of values as a Region1D< V<T> >
  //  A(I) = V = F(I);
  return Region1D< Vector<T> >( (Vector<T>&)(*this), I);
}


template <typename T> inline
const_Region1D< Vector<T> > 
Mat_COL<T>::operator()(const MatDimension&, int j) const
{
  // return entire column j as a Region1D< V<T> >
  // (optimize assignment of a vector to this Region1D)
  int i1 = (j-1)*m_M + 1;
  int i2 = (j  )*m_M;
  return const_Region1D< Vector<T> >( (const Vector<T>&)(*this), i1, i2);
}

template <typename T> inline
Region1D< Vector<T> > 
Mat_COL<T>::operator()(const MatDimension&, int j)
{
  // return entire column j as a Region1D< V<T> >
  // (optimize assignment of a vector to this Region1D)
  int i1 = (j-1)*m_M + 1;
  int i2 = (j  )*m_M;
  return Region1D< Vector<T> >( (Vector<T>&)(*this), i1, i2);
}


template <typename T> inline
const_Region2D< Mat_COL<T> >
Mat_COL<T>::operator()(int i, const MatDimension&) const
{
  // return entire row i as a Region2D< Mat<T> >
  //  A(i,:) = A(i,All) = V;   <==  A.set_row(i, V);
  //  V = A(i,All) = A(i,:);   <==  V = A.get_row(i);

  Index1D I(i,i);           // select row i
  Index1D J(1,num_cols());  // select all columns
  return const_Region2D< Mat_COL<T> >(*this, I,J);
}

template <typename T> inline
Region2D< Mat_COL<T> > 
Mat_COL<T>::operator()(int i, const MatDimension&)
{
  // return entire row i as a Region2D< Mat<T> >
  //  A(i,:) = A(i,All) = V;   <==  A.set_row(i, V);
  //  V = A(i,All) = A(i,:);   <==  V = A.get_row(i);

  Index1D I(i,i);           // select row i
  Index1D J(1,num_cols());  // select all columns
  return Region2D< Mat_COL<T> >(*this, I,J);
}



//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const Region2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  // load from Region2D< Mat<T> >
  // TODO: BLAS?

  int M = R.num_rows(), N = R.num_cols();
  resize(M,N);
  for (int j=1; j<=N; ++j) {      // for each col...
    for (int i=1; i<=M; ++i) {    //  for each row...
      this->col_[j][i] = R(i,j);  //   load R(offi+i,offj+j)
    }
  }
  return (*this);
}
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const const_Region2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  // load from Region2D< Mat<T> >
  // TODO: BLAS?

  int M = R.num_rows(), N = R.num_cols();
  resize(M,N);
  for (int j=1; j<=N; ++j) {      // for each col...
    for (int i=1; i<=M; ++i) {    //  for each row...
      this->col_[j][i] = R(i,j);  //   load R(offi+i,offj+j)
    }
  }
  return (*this);
}




//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const Region1D< Vector<T> > &R)
//---------------------------------------------------------
{
  // load from Region1D< Vec<T> >
  //
  // Note: loading a Region1D into a matrix
  // Allow assignment of a vector of matching
  // length into an existing (M,N) matrix

  m_fact_mode = FACT_NONE;
  if (this->m_name=="mat" || this->m_name.empty()) 
    this->m_name=R.name();

  int len = R.size();
  if (this->ok() && (m_M*m_N == len)) {
    // load Region1D into existing (M,N) structure.
    Vector<T>::operator= (R);
    // refresh pointers in case we switched allocation.
    set_pointers(m_M, m_N);
  } else {
    // load region into a new (N,1) matrix
    Vector<T>::operator= (R);
    set_pointers(len, 1);
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const const_Region1D< Vector<T> > &R)
//---------------------------------------------------------
{
  // load from Region1D< Vec<T> >
  //
  // Note: loading a Region1D into a matrix
  // Allow assignment of a vector of matching
  // length into an existing (M,N) matrix

  m_fact_mode = FACT_NONE;
  if (this->m_name=="mat" || this->m_name.empty()) 
    this->m_name=R.name();

  int len = R.size();
  if (this->ok() && (m_M*m_N == len)) {
    // load Region1D into existing (M,N) structure.
    Vector<T>::operator= (R);
    // refresh pointers in case we switched allocation.
    set_pointers(m_M, m_N);
  } else {
    // load region into a new (N,1) matrix
    Vector<T>::operator= (R);
    set_pointers(len, 1);
  }
  return (*this);
}


// A = A .* R
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::mult_element(const Region2D< Mat_COL<T> > &R)
{
  assert(m_M == R.num_rows());  // assume both sides conform
  assert(m_N == R.num_cols());
  for (int j=1; j<=m_N; ++j)
    for (int i=1; i<=m_M; ++i)
      (*this)(i,j) *= R(i,j);

  return (*this);
}


// A = A ./ R
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::div_element(const Region2D< Mat_COL<T> > &R)
{
  assert(m_M == R.num_rows());  // assume both sides conform
  assert(m_N == R.num_cols());
  for (int j=1; j<=m_N; ++j)
    for (int i=1; i<=m_M; ++i)
      (*this)(i,j) /= R(i,j);

  return (*this);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator+=(const Region2D< Mat_COL<T> > &R)
{
  int M1 = this->num_rows(),  N1 = this->num_cols();
  int M2 =     R.num_rows(),  N2 =     R.num_cols();

  // Allow addition of smaller block to (*this), but 
  // treat addition of a larger block as an error
  // TODO: allow call to expand(M2,N2);

  if ((M2 > M1) || (N2 > N1)) {
    umERROR("Mat_COL<T>::operator += (Region2D<> R)", 
            "Region2D (%d,%d) is larger than (*this) (%d,%d)\n"
            "TODO: expand (*this).", M2,N2, M1,N1);
  }
  for (int j=1; j<=N2; ++j) {
    for (int i=1; i<=M2; ++i) {
      this->col_[j][i] += R(i,j); // adds R(offi+i,offj+j)
    }
  }
  return (*this);
}


template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator-=(const Region2D< Mat_COL<T> > &R)
{
  int M1 = this->num_rows(),  N1 = this->num_cols();
  int M2 =     R.num_rows(),  N2 =     R.num_cols();

  // Allow subtraction of smaller block to (*this), but 
  // treat subtraction of a larger block as an error
  // TODO: allow call to expand(M2,N2);

  if ((M2 > M1) || (N2 > N1)) {
    umERROR("Mat_COL<T>::operator -= (Region2D<> R)", 
            "Region2D (%d,%d) is larger than (*this) (%d,%d)\n"
            "TODO: expand (*this).", M2,N2, M1,N1);
  }
  for (int j=1; j<=N2; ++j) {
    for (int i=1; i<=M2; ++i) {
      this->col_[j][i] -= R(i,j); // subtracts R(offi+i,offj+j)
    }
  }
  return (*this);
}



///////////////////////////////////////////////////////////
//
// global functions involving (contiguous) Region[1,2]D
//
//   region + region
//   matrix + region
//   region + matrix
//
//          - region   (negation, unary operator)
//   region - region
//
// Multiplication :
//---------------------------------------
// 6a:  Region2D .* Region2D  =>  dm(R1,R2)
// 6b:  Region1D .* Region1D  =>  dm(R1,R2)
// 6c:  matrix    * Region1D  =>  A*r
//
// Division :
//---------------------------------------
// 4a:  Region2D ./ Region2D  =>  dd(R1,R2)
// 4b:  Region1D ./ Region1D  =>  dd(R1,R2)
//
//
// outer (vector, Region1D)
// outer (Region1D, vector)
//
///////////////////////////////////////////////////////////

// region + region
template <typename T> inline 
Mat_COL<T>& operator+(const Region2D< Mat_COL<T> > &RA, 
                      const Region2D< Mat_COL<T> > &RB)
{
  std::string sz("rgn(A)+rgn(B)");

  Mat_COL<T> *tmp=new Mat_COL<T>(RA, OBJ_temp, sz.c_str());
  (*tmp) += RB;
  return (*tmp);
}


// matrix + region
template <typename T> inline 
Mat_COL<T>& operator+(const           Mat_COL<T>   &M, 
                      const Region2D< Mat_COL<T> > &R)
{
  std::string sz; tmp_op_name(M.name(),"+","rgn()", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(M, OBJ_temp, sz.c_str());
  (*tmp) += R;
  return (*tmp);
}


// region + matrix
template <typename T> inline 
Mat_COL<T>& operator+(const Region2D< Mat_COL<T> > &R, 
                      const           Mat_COL<T>   &M)
{
  std::string sz; tmp_op_name("rgn()","+",M.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(M, OBJ_temp, sz.c_str());
  (*tmp) += R;
  return (*tmp);
}


//  - region (negation, unary operator)
template <typename T> inline 
Mat_COL<T>& operator-(const Region2D< Mat_COL<T> > &R) 
{
  std::string sz; tmp_op_name(" ","-","rgn(A)", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(R, OBJ_temp, sz.c_str());
  (*tmp) *= T(-1);
  return (*tmp);
}


// region - region
template <typename T> inline 
Mat_COL<T>& operator-(const Region2D< Mat_COL<T> > &RA, 
                      const Region2D< Mat_COL<T> > &RB)
{
  std::string sz("rgn(A)-rgn(B)");

  Mat_COL<T> *tmp=new Mat_COL<T>(RA, OBJ_temp, sz.c_str());
  (*tmp) -= RB;
  return (*tmp);
}



//---------------------------------------------------------
// Multiplication :
//---------------------------------------------------------
// 6a:   Region2D .* Region2D
// 6b:   Region1D .* Region1D
//---------------------------------------------------------


// 6a. Region2D .* Region2D
template <typename T> inline 
Mat_COL<T>& dm(const Region2D< Mat_COL<T> > &A, 
               const Region2D< Mat_COL<T> > &B)
{
  std::string sz; tmp_op_name("reg2D(A)",".*","reg2D(B)", sz);

  Mat_COL<T> *tmpA=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  tmpA->mult_element(B);
  return (*tmpA);
}


// 6b. Region1D .* Region1D
template <typename T> inline 
Vector<T>& dm(const Region1D< Vector<T> > &A, 
              const Region1D< Vector<T> > &B)
{
  std::string sz; tmp_op_name("reg1D(A)",".*","reg1D(B)", sz);

  Vector<T> *tmpA=new Vector<T>(A, OBJ_temp, sz.c_str());
  tmpA->mult_element(B);
  return (*tmpA);
}


// 6c:  matrix * Region1D
template <typename T> inline 
Vector<T>& operator*(const Mat_COL<T> &A, const Region1D< Vector<T> > &R)
{
  int M=A.num_rows(), K=A.num_cols(), N=R.size();
  assert(K==N);
  Vector<T> V(R);
  return matmult(A,V); // usual A * v
}


// 6d:  matrix * MappedRegion1D
template <typename T> inline 
Vector<T>& operator*(const Mat_COL<T> &A, 
                     const MappedRegion1D< Vector<T> > &R)
{
  int M=A.num_rows(), K=A.num_cols(), N=R.size();
  assert(K==N);
  Vector<T> V(R);
  return matmult(A,V); // usual A * v
}



//---------------------------------------------------------
// Division :
//---------------------------------------------------------
// 6a:  Region2D ./ Region2D
// 6b:  Region1D ./ Region1D
//---------------------------------------------------------

// 6a: Region2D ./ Region2D
template <typename T> inline 
Mat_COL<T>& dd(const Region2D< Mat_COL<T> > &A, 
               const Region2D< Mat_COL<T> > &B)
{
  std::string sz; tmp_op_name("reg2D(A)","./","reg2D(B)", sz);

  Mat_COL<T> *tmpA=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  tmpA->div_element(B);
  return (*tmpA);
}


// 6b: Region1D ./ Region1D
template <typename T> inline 
Vector<T>& dd(const Region1D< Vector<T> > &A, 
              const Region1D< Vector<T> > &B)
{
  std::string sz; tmp_op_name("reg1D(A)","./","reg1D(B)", sz);

  Vector<T> *tmpA=new Vector<T>(A, OBJ_temp, sz.c_str());
  tmpA->div_element(B);
  return (*tmpA);
}




//---------------------------------------------------------
// outer products of vectors with Region1D
//---------------------------------------------------------
//
// Each column of result is a scaled copy of LHS,
// e.g.:
// 
//      [ R(1)     R(2)     R(3)     ...  ]
//      -----------------------------------
// A(1) | R1 A(1)  R2 A(1)  R3 A(1)  ...  |
// A(2) | R1 A(2)  R2 A(2)  R3 A(2)  ...  |
// A(3) | R1 A(3)  R2 A(3)  R3 A(3)  ...  |
// A(4) | R1 A(4)  R2 A(4)  R3 A(4)  ...  |
//---------------------------------------------------------

template <typename T> inline 
//Mat_COL<T>& operator*
Mat_COL<T>& outer
(
  const Vector<T> &V, 
  const Region1D< Vector<T> > &R
)
{
  std::string sz("outer(V,reg(B))");

  int M = V.size(), N = R.size();
  Mat_COL<T> *tmp=new Mat_COL<T>(M,N, sz.c_str(), OBJ_temp);
  Vector<T> vec(V);   // avoid issue of temporary V
  for (int j=1; j<=N; ++j) { (*tmp).set_col(j, vec*R(j)); }
  return (*tmp);
}


template <typename T> inline 
Mat_COL<T>& operator*
(
  const Region1D< Vector<T> > &R, 
  const Vector<T> &V
)
{
  std::string sz("outer(reg(B),V)");

  int M = R.size(), N = V.size();
  Mat_COL<T> *tmp=new Mat_COL<T>(M,N, sz.c_str(), OBJ_temp);
  Vector<T> B(R);   // convert map to vector
  for (int j=1; j<=N; ++j) { (*tmp).set_col(j, B*V(j)); }
  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// member functions involving MappedRegion1D
//
///////////////////////////////////////////////////////////


// construct from MappedRegion2D
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const MappedRegion2D< Mat_COL<T> > &R, OBJ_mode md, const char *sz)
//---------------------------------------------------------
: Vector<T>(sz, md),
  m_M(0), m_N(0), m_MN(0), col_(0), 
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  (*this)=R;  // matrix = mapped region
}

//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>::Mat_COL(const const_MappedRegion2D< Mat_COL<T> > &R, OBJ_mode md, const char *sz)
//---------------------------------------------------------
: Vector<T>(sz, md),
  m_M(0), m_N(0), m_MN(0), col_(0), 
  m_fact_mode(FACT_NONE), m_ipiv(NULL)
{
  (*this)=R;  // matrix = mapped region
}



//---------------------------------------------------------
// return mapped region of 1 column as a MappedRegion2D< Mat<T> >
//---------------------------------------------------------

template <typename T> inline
const_MappedRegion2D< Mat_COL<T> > 
Mat_COL<T>::operator()(const Region1D< Vector<int> > &Ri, int j) const
{
  Vector<int> I(Ri);            // extract a IVec
  IMap im(I.size(), I.data());  // repack as IMap
  Index1D JDX(j,j);
  return const_MappedRegion2D< Mat_COL<T> >( (*this), im, JDX);
}

template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Region1D< Vector<int> > &Ri, int j)
{
  Vector<int> I(Ri);            // extract a IVec
  IMap im(I.size(), I.data());  // repack as IMap
  Index1D JDX(j,j);
  return MappedRegion2D< Mat_COL<T> >( (*this), im, JDX);
}


template <typename T> inline
const_MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Vector<int> &I, int j) const
{
  Index1D JDX(j,j);
  IMap im(I.size(), I.data());  // repack as IMap
  return const_MappedRegion2D< Mat_COL<T> >( (*this), im, JDX);
}


template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Vector<int> &I, int j)
{
  Index1D JDX(j,j);
  IMap im(I.size(), I.data());  // repack as IMap
  return MappedRegion2D< Mat_COL<T> >( (*this), im, JDX);
}



//---------------------------------------------------------
// return mapped region of 1 row as a MappedRegion2D< Mat<T> >
//---------------------------------------------------------

template <typename T> inline
const_MappedRegion2D< Mat_COL<T> > 
Mat_COL<T>::operator()(int i, const Region1D< Vector<int> > &Rj) const
{
  Vector<int> J(Rj);            // extract a IVec
  IMap jn(J.size(), J.data());  // repack as IMap
  Index1D IDX(i,i);
  return const_MappedRegion2D< Mat_COL<T> >( (*this), IDX, jn);
}

template <typename T> inline
MappedRegion2D< Mat_COL<T> > 
Mat_COL<T>::operator()(int i, const Region1D< Vector<int> > &Rj)
{
  Vector<int> J(Rj);            // extract a IVec
  IMap jn(J.size(), J.data());  // repack as IMap
  Index1D IDX(i,i);
  return MappedRegion2D< Mat_COL<T> >( (*this), IDX, jn);
}


template <typename T> inline
const_MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(int i, const Vector<int> &J) const
{
  Index1D IDX(i,i);
  IMap jn(J.size(), J.data());  // repack as IMap
  return const_MappedRegion2D< Mat_COL<T> >( (*this), IDX, jn);
}


template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(int i, const Vector<int> &J)
{
  Index1D IDX(i,i);
  IMap jn(J.size(), J.data());  // repack as IMap
  return MappedRegion2D< Mat_COL<T> >( (*this), IDX, jn);
}


//---------------------------------------------------------
// Return mapped values as a MappedRegion1D< V<T> >
//
//  A(mapI) = V = F(mapI);
//
//---------------------------------------------------------

template <typename T> inline
const_MappedRegion1D< Vector<T> >
Mat_COL<T>::operator()(const IVec &I) const
{
  // return a (1D) const mapped region of (*this)
  IMap im(I.size(), I.data());  // repack as IMap
  return const_MappedRegion1D< Vector<T> >( (const Vector<T>&)(*this), im); 
}


template <typename T> inline
MappedRegion1D< Vector<T> >
Mat_COL<T>::operator()(const IVec &I)
{
  // return a (1D) mapped region of (*this)
  IMap im(I.size(), I.data());  // repack as IMap
  return MappedRegion1D< Vector<T> >( (Vector<T>&)(*this), im); 
}


//---------------------------------------------------------
// Map subset of Rows in this matrix
//---------------------------------------------------------

template <typename T> inline
const_MappedRegion2D< Mat_COL<T> > 
Mat_COL<T>::operator()(const IVec &I, const MatDimension& MD) const
{
  IMap im(I.size(), I.data());  // repack as IMap
  return const_MappedRegion2D< Vector<T> >(*this, im, MD);
}


template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const IVec &I, const MatDimension& MD) 
{
  IMap im(I.size(), I.data());  // repack as IMap
  if (I.get_mode() == OBJ_temp) { delete (&I); }
  return MappedRegion2D< Mat_COL<T> >(*this, im, MD);
}


template <typename T> inline
const_MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Index1D &ID, const MatDimension& MD) const
{
  return const_MappedRegion2D< Vector<T> >(*this, ID, MD);
}


template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Index1D &ID, const MatDimension& MD) 
{
  return MappedRegion2D< Mat_COL<T> >(*this, ID, MD);
}


//---------------------------------------------------------
// Map subset of Columns in this matrix
//---------------------------------------------------------

template <typename T> inline
const_MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const MatDimension& MD, const IVec &J) const
{
  IMap jm(J.size(), J.data());  // repack as IMap
  return const_MappedRegion2D< Vector<T> >(*this, MD, jm);
}


template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const MatDimension& MD, const IVec &J) 
{
  IMap jm(J.size(), J.data());  // repack as IMap
  return MappedRegion2D< Mat_COL<T> >(*this, MD, jm);
}


//---------------------------------------------------------
// Generalized 2D mapping
//---------------------------------------------------------

//---------------------------------------
// (Map, Map)
//---------------------------------------
template <typename T> inline
const_MappedRegion2D< Mat_COL<T> > 
Mat_COL<T>::operator()(const IVec &I, const IVec &J) const
{
  IMap im(I.size(), I.data());  // repack as IMap
  IMap jm(J.size(), J.data());
  return const_MappedRegion2D< Vector<T> >(*this, im, jm);
}

template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const IVec &I, const IVec &J) 
{
  IMap im(I.size(), I.data());  // repack as IMap
  IMap jm(J.size(), J.data());
  return MappedRegion2D< Mat_COL<T> >(*this, im, jm);
}


//---------------------------------------
// (Map, Range)
//---------------------------------------
template <typename T> inline
const_MappedRegion2D< Mat_COL<T> > 
Mat_COL<T>::operator()(const IVec &I, const Index1D &IDX) const
{
  IMap im(I.size(), I.data());  // repack as IMap
  return const_MappedRegion2D< Vector<T> >(*this, im, IDX);
}

template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const IVec &I, const Index1D &IDX) 
{
  IMap im(I.size(), I.data());  // repack as IMap
  return MappedRegion2D< Mat_COL<T> >(*this, im, IDX);
}


//---------------------------------------
// (Range, Map)
//---------------------------------------
template <typename T> inline
const_MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Index1D &IDX, const IVec &J) const
{
  IMap jm(J.size(), J.data());  // repack as IMap
  return const_MappedRegion2D< Vector<T> >(*this, IDX, jm);
}

template <typename T> inline
MappedRegion2D< Mat_COL<T> >
Mat_COL<T>::operator()(const Index1D &IDX, const IVec &J) 
{
  IMap jm(J.size(), J.data());  // repack as IMap
  return MappedRegion2D< Mat_COL<T> >(*this, IDX, jm);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const const_MappedRegion2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  // load from a MappedRegion2D< Mat<T> >

  int M = R.num_rows(), N = R.num_cols();
  resize(M,N);
  for (int j=1; j<=N; ++j) {      // for each col...
    for (int i=1; i<=M; ++i) {    //  for each row...
      this->col_[j][i] = R(i,j);  //   load R(map(i),j)
    }
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const MappedRegion2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  // load from a MappedRegion2D< Mat<T> >

  int M = R.num_rows(), N = R.num_cols();
  resize(M,N);
  for (int j=1; j<=N; ++j) {      // for each col...
    for (int i=1; i<=M; ++i) {    //  for each row...
      this->col_[j][i] = R(i,j);  //   load R(map(i),j)
    }
  }
  return (*this);
}


// Note: loading a MappedRegion1D into a 2D matrix.
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const const_MappedRegion1D< Vector<T> > &R)
//---------------------------------------------------------
{
  // Allow assignment of a vector of matching
  // length into an existing (M,N) matrix

  m_fact_mode = FACT_NONE;
  if (this->m_name=="mat" || this->m_name.empty()) {this->m_name=R.name();}
  int len = R.size();
  if (this->ok() && (m_M*m_N == len)) {
    Vector<T>::operator= (R);   // load MappedRegion1D into existing (M,N) structure.
    set_pointers(m_M, m_N);     // refresh pointers in case we switched allocation.
  } else {
    Vector<T>::operator= (R);   // load region into a new (N,1) matrix
    set_pointers(len, 1);
  }
  return (*this);
}


// Note: loading a MappedRegion1D into a 2D matrix.
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator=(const MappedRegion1D< Vector<T> > &R)
//---------------------------------------------------------
{
  // Allow assignment of a vector of matching
  // length into an existing (M,N) matrix

  m_fact_mode = FACT_NONE;
  if (this->m_name=="mat" || this->m_name.empty()) {this->m_name=R.name();}
  int len = R.size();
  if (this->ok() && (m_M*m_N == len)) {
    Vector<T>::operator= (R);   // load MappedRegion1D into existing (M,N) structure.
    set_pointers(m_M, m_N);     // refresh pointers in case we switched allocation.
  } else {
    Vector<T>::operator= (R);   // load region into a new (N,1) matrix
    set_pointers(len, 1);
  }
  return (*this);
}



// (*this) is not changed: C = A .* R(map)
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::dm(const MappedRegion2D< Mat_COL<T> > &R) const
//---------------------------------------------------------
{
  int M2=R.num_rows(), N2=R.num_cols();
  if ((m_M != M2) || (m_N != N2)) {
    umERROR("Mat_COL<T> dm (MappedRegion2D)", 
            "Dimensions of MappedRegion2D (%d,%d) must match (*this) (%d,%d)\n", M2,N2, m_M,m_N);
  }

  std::string sz; tmp_op_name(this->name(), ".*", "R(map)", sz);
  // NOT USING COPY CONSTRUCTOR: side-effects if (*this)==OBJ_temp
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->data(), OBJ_temp, sz.c_str());

  for (int j=1; j<=m_N; ++j) {
    for (int i=1; i<=m_M; ++i) {
      tmp->col_[j][i] *= R(i,j); // multiplies by R( map(i), j )
    }
  }

  return (*tmp);
}
  

// (*this) is not changed: C = A ./ R(map)
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::dd(const MappedRegion2D< Mat_COL<T> > &R) const
//---------------------------------------------------------
{
  int M2=R.num_rows(), N2=R.num_cols();
  if ((m_M != M2) || (m_N != N2)) {
    umERROR("Mat_COL<T> dd (MappedRegion2D)", 
            "Dimensions of MappedRegion2D (%d,%d) must match (*this) (%d,%d)\n", M2,N2, m_M,m_N);
  }

  std::string sz; tmp_op_name(this->name(), "./", "R(map)", sz);
  // NOT USING COPY CONSTRUCTOR: side-effects if (*this)==OBJ_temp
  Mat_COL<T> *tmp=new Mat_COL<T>(m_M, m_N, this->data(), OBJ_temp, sz.c_str());

  for (int j=1; j<=m_N; ++j) {
    for (int i=1; i<=m_M; ++i) {
      tmp->col_[j][i] /= R(i,j); // divides by R( map(i), j )
    }
  }

  return (*tmp);
}


// A = A .* R
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::mult_element(const MappedRegion2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  assert(m_M == R.num_rows());  // assume both sides conform
  assert(m_N == R.num_cols());
  for (int j=1; j<=m_N; ++j)
    for (int i=1; i<=m_M; ++i)
      (*this)(i,j) *= R(i,j);

  return (*this);
}


// A = A ./ R
//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::div_element(const MappedRegion2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  assert(m_M == R.num_rows());  // assume both sides conform
  assert(m_N == R.num_cols());
  for (int j=1; j<=m_N; ++j)
    for (int i=1; i<=m_M; ++i)
      (*this)(i,j) /= R(i,j);

  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator+=(const MappedRegion2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  int M1 = this->num_rows(), M2 = R.num_rows();
  int N1 = this->num_cols(), N2 = R.num_cols();

  // Allow addition of smaller block to (*this), but 
  // treat addition of a larger block as an error
  // TODO: allow call to expand(M2,N2);

  if ((M2 > M1) || (N2 > N1)) {
    umERROR("Mat_COL<T>::operator += (MappedRegion2D<> R)", 
      "MappedRegion2D (%d,%d) is larger than (*this) (%d,%d)\n"
      "TODO: expand (*this).", M2,N2, M1,N1);
  }
  for (int j=1; j<=N2; ++j) {
    for (int i=1; i<=M2; ++i) {
      this->col_[j][i] += R(i,j); // adds R( map(i), j )
    }
  }
  return (*this);
}


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& Mat_COL<T>::operator-=(const MappedRegion2D< Mat_COL<T> > &R)
//---------------------------------------------------------
{
  int M1 = this->num_rows(), M2 = R.num_rows();
  int N1 = this->num_cols(), N2 = R.num_cols();

  // Allow subtraction of smaller block to (*this), but 
  // treat subtraction of a larger block as an error
  // TODO: allow call to expand(M2,N2);

  if ((M2 > M1) || (N2 > N1)) {
    umERROR("Mat_COL<T>::operator -= (MappedRegion2D<> R)", 
      "MappedRegion2D (%d,%d) is larger than (*this) (%d,%d)\n"
      "TODO: expand (*this).", M2,N2, M1,N1);
  }
  for (int j=1; j<=N2; ++j) {
    for (int i=1; i<=M2; ++i) {
      this->col_[j][i] -= R(i,j); // subtracts R( map(i), j )
    }
  }
  return (*this);
}


///////////////////////////////////////////////////////////
//
// global functions involving MappedRegion[1,2]D
//
///////////////////////////////////////////////////////////
//
// Addition:
//
//          matrix + mapped region
//   mapped region + matrix
//
//   mapped region + scalar         (R+x)
//          scalar + mapped region  (x+R)
//
//          vector + mapped region
//   mapped region + vector
//
//   mapped region + mapped region
//
//
// Subtraction:
//
//          matrix - mapped region
//   mapped region - matrix
//
//   mapped region - scalar
//          scalar - mapped region
//
//          vector - mapped region
//   mapped region - vector
//
//   mapped region - mapped region
//                 - mapped region  (negation, unary op)
//
///////////////////////////////////////////////////////////



//---------------------------------------------------------
// Addition:
//---------------------------------------------------------

// matrix + mapped region
template <typename T> inline 
Mat_COL<T>& operator+(const                 Mat_COL<T>   &M, 
                      const MappedRegion2D< Mat_COL<T> > &R)
{
  std::string sz; tmp_op_name(M.name(),"+","map()", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(M, OBJ_temp, sz.c_str());
  (*tmp) += R;
  return (*tmp);
}


// mapped region + matrix
template <typename T> inline 
Mat_COL<T>& operator+(const MappedRegion2D< Mat_COL<T> > &R, 
                      const                 Mat_COL<T>   &M)
{
  std::string sz; tmp_op_name("rgn()","+",M.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(M, OBJ_temp, sz.c_str());
  (*tmp) += R;
  return (*tmp);
}


// mapped region + scalar
template <typename T> inline 
Mat_COL<T>& operator+(const MappedRegion2D< Mat_COL<T> > &R, T x)
{
  std::string sz; tmp_op_name("map2D()","+","x", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(R, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}

// scalar + mapped region
template <typename T> inline 
Mat_COL<T>& operator+(T x, const MappedRegion2D< Mat_COL<T> > &R)
{
  // create a matrix filled with scalar x; add R
  std::string sz; tmp_op_name("x","+","map2D()", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(R.num_rows(), R.num_cols(), x, OBJ_temp, sz.c_str());
  (*tmp) += R;
  return (*tmp);
}


//---------------------------------------------------------
// *** treat a Mapped 2D region as a Vector ***
//---------------------------------------------------------
// vector + mapped region2D
template <typename T> inline 
Vector<T>& operator+(const                  Vector<T>   &V, 
                     const MappedRegion2D< Mat_COL<T> > &R)
{
  std::string sz; tmp_op_name(V.name(),"+","map2D()", sz);

  // NBN: expecting Nr||Nc to be 1
  int Nr=R.num_rows();  // num mapped rows
  int Nc=R.num_cols();  // num mapped cols 

  // use a Matrix to extract elements of the 2D map
  Mat_COL<T> tmpM(R);
  Vector<T> *tmp=new Vector<T>(V, OBJ_temp, sz.c_str());

  // cast mapped elements to a Vector, then add
  (*tmp) += ((const Vector<T>&) tmpM);
  return (*tmp);
}


//---------------------------------------------------------
// *** treat a Mapped 2D region as a Vector ***
//---------------------------------------------------------
// mapped region2D + vector
template <typename T> inline 
Vector<T>& operator+(const MappedRegion2D< Mat_COL<T> > &R, 
                     const                  Vector<T>   &V)
{
  std::string sz; tmp_op_name("map2D()","+",V.name(), sz);

  // NBN: expecting Nr||Nc to be 1
  int Nr=R.num_rows();  // num mapped rows
  int Nc=R.num_cols();  // num mapped cols 

  // use a Matrix to extract elements of the 2D map
  Mat_COL<T> tmpM(R);

  // Convert mapped elements to a Vector, then add
  Vector<T> *tmp=new Vector<T>((const Vector<T>&)tmpM, OBJ_temp, sz.c_str());
  (*tmp) += V;
  return (*tmp);
}


// MappedRegion2D + MappedRegion2D
template <typename T> inline 
Mat_COL<T>& operator+(const MappedRegion2D< Mat_COL<T> > &A, 
                      const MappedRegion2D< Mat_COL<T> > &B)
{
  std::string sz("A(map) + B(map)");
  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}



//---------------------------------------------------------
// Subtraction:
//---------------------------------------------------------


// matrix - mapped region
template <typename T> inline 
Mat_COL<T>& operator-(const                 Mat_COL<T>   &M, 
                      const MappedRegion2D< Mat_COL<T> > &R)
{
  std::string sz; tmp_op_name(M.name(),"-","map2D()", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(M, OBJ_temp, sz.c_str());
  (*tmp) -= R;
  return (*tmp);
}

// mapped region - matrix
template <typename T> inline 
Mat_COL<T>& operator-(const MappedRegion2D< Mat_COL<T> > &R, 
                      const                 Mat_COL<T>   &M)
{
  std::string sz; tmp_op_name("map2D()","-",M.name(), sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(M, OBJ_temp, sz.c_str());
  (*tmp) -= R;
  return (*tmp);
}


// mapped region - scalar
template <typename T> inline 
Mat_COL<T>& operator-(const MappedRegion2D< Mat_COL<T> > &R, T x)
{
  std::string sz; tmp_op_name("map2D()","-","x", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(R, OBJ_temp, sz.c_str());
  (*tmp) -= x;
  return (*tmp);
}

// scalar - mapped region
template <typename T> inline 
Mat_COL<T>& operator-(T x, const MappedRegion2D< Mat_COL<T> > &R)
{
  //
  // create a matrix filled with scalar x; subtract R
  //
  std::string sz; tmp_op_name("x","-","map2D()", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(R.num_rows(), R.num_cols(), x, OBJ_temp, sz.c_str());

  (*tmp) -= R;
  return (*tmp);
}


//---------------------------------------------------------
// *** treat a Mapped 2D region as a Vector ***
//---------------------------------------------------------
// vector - mapped region2D
template <typename T> inline 
Vector<T>& operator-(const                  Vector<T>   &V, 
                     const MappedRegion2D< Mat_COL<T> > &R)
{
  std::string sz; tmp_op_name(V.name(),"-","map2D()", sz);

  // NBN: expecting Nr||Nc to be 1
  int Nr=R.num_rows();  // num mapped rows
  int Nc=R.num_cols();  // num mapped cols 

  // use a Matrix to extract elements of the 2D map
  Mat_COL<T> tmpM(R);

  Vector<T> *tmp=new Vector<T>(V, OBJ_temp, sz.c_str());

  // cast mapped elements to a Vector, then subtract
  (*tmp) -= ((const Vector<T>&) tmpM);
  return (*tmp);
}

//---------------------------------------------------------
// *** treat a Mapped 2D region as a Vector ***
//---------------------------------------------------------
// mapped region2D - vector
template <typename T> inline 
Vector<T>& operator-(const MappedRegion2D< Mat_COL<T> > &R, 
                     const                  Vector<T>   &V)
{
  std::string sz; tmp_op_name("map2D()","-",V.name(), sz);

  // NBN: expecting Nr||Nc to be 1
  int Nr=R.num_rows();  // num mapped rows
  int Nc=R.num_cols();  // num mapped cols 

  // use a Matrix to extract elements of the 2D map
  Mat_COL<T> tmpM(R);

  // Convert mapped elements to a Vector, then subtract
  Vector<T> *tmp=new Vector<T>((const Vector<T>&)tmpM, OBJ_temp, sz.c_str());
  (*tmp) -= V;
  return (*tmp);
}


// MappedRegion2D - MappedRegion2D
template <typename T> inline 
Mat_COL<T>& operator-(const MappedRegion2D< Mat_COL<T> > &A, 
                      const MappedRegion2D< Mat_COL<T> > &B)
{
  std::string sz("A(map) - B(map)");
  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}


// negation (unary operator) of a MappedRegion2D
template <typename T> inline 
Mat_COL<T>& operator-(const MappedRegion2D< Mat_COL<T> > &R) 
{
  std::string sz; tmp_op_name(" ","-","A(map)", sz);

  Mat_COL<T> *tmp=new Mat_COL<T>(R, OBJ_temp, sz.c_str());
  (*tmp) *= T(-1);
  return (*tmp);
}




//---------------------------------------------------------
// Multiplication :
//---------------------------------------------------------
// 7a: MappedRegion2D .* MappedRegion2D
// 7b: MappedRegion1D .* MappedRegion1D
//---------------------------------------------------------


// 7a: MappedRegion2D .* MappedRegion2D
template <typename T> inline 
Mat_COL<T>& dm(const MappedRegion2D< Mat_COL<T> > &A, 
               const MappedRegion2D< Mat_COL<T> > &B)
{
  std::string sz; tmp_op_name("map2D(A)",".*","map2D(B)", sz);

  Mat_COL<T> *tmpA=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  tmpA->mult_element(B);
  return (*tmpA);
}


// 7b: MappedRegion1D .* MappedRegion1D
template <typename T> inline 
Vector<T>& dm(const MappedRegion1D< Vector<T> > &A, 
              const MappedRegion1D< Vector<T> > &B)
{
  std::string sz; tmp_op_name("map1D(A)",".*","map1D(B)", sz);

  Vector<T> *tmpA=new Vector<T>(A, OBJ_temp, sz.c_str());
  tmpA->mult_element(B);
  return (*tmpA);
}


//---------------------------------------------------------
// Division :
//---------------------------------------------------------
// 5a:   MappedRegion2D ./ MappedRegion2D
// 5b:   MappedRegion1D ./ MappedRegion1D
//---------------------------------------------------------


// 7a: MappedRegion2D ./ MappedRegion2D
template <typename T> inline 
Mat_COL<T>& dd(const MappedRegion2D< Mat_COL<T> > &A, 
               const MappedRegion2D< Mat_COL<T> > &B)
{
  std::string sz; tmp_op_name("map2D(A)","./","map2D(B)", sz);

  Mat_COL<T> *tmpA=new Mat_COL<T>(A, OBJ_temp, sz.c_str());
  tmpA->div_element(B);
  return (*tmpA);
}


// 7b: MappedRegion1D ./ MappedRegion1D
template <typename T> inline 
Vector<T>& dd(const MappedRegion1D< Vector<T> > &A, 
              const MappedRegion1D< Vector<T> > &B)
{
  std::string sz; tmp_op_name("map1D(A)","./","map1D(B)", sz);

  Vector<T> *tmpA=new Vector<T>(A, OBJ_temp, sz.c_str());
  tmpA->div_element(B);
  return (*tmpA);
}


// 7c: MappedRegion1D ./ vector
template <typename T> inline 
Vector<T>& dd(const MappedRegion1D< Vector<T> > &A, 
                                    Vector<T>   &B)
{
  std::string sz; tmp_op_name("map1D(A)","./","vector", sz);

  Vector<T> *tmpA=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmpA) /= B;
  return (*tmpA);
}

// 7d: vector ./ MappedRegion1D
template <typename T> inline 
Vector<T>& dd(                      Vector<T>   &A,
              const MappedRegion1D< Vector<T> > &B)
{
  std::string sz; tmp_op_name("vector","./","map1D(B)", sz);

  Vector<T> *tmpA=new Vector<T>(A, OBJ_temp, sz.c_str());
  (*tmpA).div_element(B);
  return (*tmpA);
}



//---------------------------------------------------------
// outer products of vectors with MappedRegion1D
//---------------------------------------------------------
//
// Each column of result is a scaled copy of LHS, 
// e.g.:
// 
//      [ R(1)     R(2)     R(3)     ...  ]
//      -----------------------------------
// A(1) | R1 A(1)  R2 A(1)  R3 A(1)  ...  |
// A(2) | R1 A(2)  R2 A(2)  R3 A(2)  ...  |
// A(3) | R1 A(3)  R2 A(3)  R3 A(3)  ...  |
// A(4) | R1 A(4)  R2 A(4)  R3 A(4)  ...  |
//---------------------------------------------------------

template <typename T> inline 
Mat_COL<T>& operator*
(
  const Vector<T> &V, 
  const MappedRegion1D< Vector<T> > &R
)
{
  std::string sz("outer(V,R(map))");

  int M = V.size(), N = R.size();
  Mat_COL<T> *tmp=new Mat_COL<T>(M,N, sz.c_str(), OBJ_temp);
  Vector<T> vec(V);   // avoid issue of temporary V
  for (int j=1; j<=N; ++j) {
    (*tmp).set_col(j, R(j) * vec);
  }

  return (*tmp);
}


template <typename T> inline 
Mat_COL<T>& operator*
(
  const MappedRegion1D< Vector<T> > &R, 
  const Vector<T> &V
)
{
  std::string sz("outer(R(map),V)");

  int M = R.size(), N = V.size();
  Mat_COL<T> *tmp=new Mat_COL<T>(M,N, sz.c_str(), OBJ_temp);
  Vector<T> B(R);   // convert map to vector

  for (int j=1; j<=N; ++j) {
    (*tmp).set_col(j, B*V(j));
  }

  if (V.get_mode() == OBJ_temp) { delete (&V); }
  return (*tmp);
}



//---------------------------------------------------------
// Miscellaneous operators involving MappedRegion2D :
//---------------------------------------------------------
// max (mapped region, mapped region)
// 
// mapped region += vector
// mapped region -= vector
// ...
// ...
// ...
//---------------------------------------------------------


//---------------------------------------------------------
template <typename T> inline
Mat_COL<T>& max
(
  const MappedRegion2D< Mat_COL<T> >& A, 
  const MappedRegion2D< Mat_COL<T> >& B
)
//---------------------------------------------------------
{
  // assume matching dimensions
  int Nr=A.num_rows(), Nc=A.num_cols(), len=A.size();
  assert( B.num_rows()==Nr  && B.num_cols()==Nc );

  // constructor deletes A if OBJ_temp
  DMat *tmp=new DMat(A, OBJ_temp, "MAX(A,B)");
  double **p = tmp->col_;
  for (int j=1; j<=Nc; ++j) {
    for (int i=1; i<=Nr; ++i) {
      p[j][i] = std::max(p[j][i], B(i,j));
    }
  }

  if (B.get_mode() == OBJ_temp) { delete (&B); }
  return (*tmp);
}


#endif  // NDG__Matrix_COL_H__INCLUDED
