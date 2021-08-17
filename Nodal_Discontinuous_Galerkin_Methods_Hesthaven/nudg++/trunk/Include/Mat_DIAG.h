// Mat_DIAG.h
// Matlab-compatible diagonal matrix
// 1-based (i,j) indexing
// 2007/10/08
//---------------------------------------------------------
#ifndef NDG__Mat_DIAG_H__INCLUDED
#define NDG__Mat_DIAG_H__INCLUDED

#include "Vec_Type.h"

#define ALLOW_NON_DIAG_IDX 1


// typedef versions for common data types
template <typename T> class Mat_DIAG;

typedef Mat_DIAG<double>  DMat_Diag;
typedef Mat_DIAG<dcmplx>  ZMat_Diag;
typedef Mat_DIAG<int>     IMat_Diag;
typedef Mat_DIAG<long>    LMat_Diag;
typedef Mat_DIAG<bool>    BMat_Diag;


//---------------------------------------------------------
template <typename T> 
class Mat_DIAG : public virtual Vector<T>
//---------------------------------------------------------
{
  //
  // Member data
  //
protected:

  int   m_M;        // num rows
  int   m_N;        // num cols
  int   m_MN;       // num elements = (m*n)
  int   m_D;        // num diagonal elements = min(M,N)

  double  m_norm_1,       // max col sum
          m_norm_inf,     // max row sum
          m_norm_frob;    // sqrt(sum(squares))


public:

  // constructors
  explicit Mat_DIAG(const char* sz="diag", OBJ_mode md=OBJ_real);
           Mat_DIAG(const Mat_DIAG<T> &B, OBJ_mode md=OBJ_real, const char* sz="diag");
           Mat_DIAG(const Vector<T> &V, OBJ_mode md=OBJ_real, const char* sz="diag");
  explicit Mat_DIAG(int N, const char* sz);
  explicit Mat_DIAG(int N, const T x=T(0), OBJ_mode md=OBJ_real, const char* sz="diag");
  explicit Mat_DIAG(int N, const T *data, OBJ_mode md=OBJ_real, const char* sz="diag");


  // destructor
  virtual ~Mat_DIAG();
  virtual void destroy();

  void borrow(int M, int N, T* p);
  void borrow(int N, T* p) { borrow(N,N, p); }
  void borrow(const Vector<T>& CV);

  void set_counters(int M, int N);

  bool resize(int M, int N, bool bInit=true, T x=T(0));
  bool resize(int N, bool bInit=true, T x=T(0)) { return resize(N,N, bInit, x); }


  //-------------------------------------------------------
  void load(int D, const T *vdata)
  //-------------------------------------------------------
  {
    // load data into matrix DIAGONAL
    resize(D,D, false);
    copy(vdata);
  }


  //-------------------------------------------------------
  void load(int M, int N, const T *vdata)
  //-------------------------------------------------------
  {
    // load data into matrix DIAGONAL
    resize(M,N, false);
    copy(vdata);
  }


  //-------------------------------------------------------
  void load(int M, int N, const char *sdata)
  //-------------------------------------------------------
  {
    // load ASCII data into matrix DIAGONAL
    resize(M,N);
    try {
      std::istringstream ins(sdata);
      for (int i=0; i<m_D; ++i)
        ins >> this->v_[i];             // just loading diagonal
    } catch(...) { 
      umERROR("load(M,N, char *s)", "problem parsing data.");
    }
  }


  // assignment
  Mat_DIAG<T>& operator=(const T &val) { fill(val); return (*this); }
  Mat_DIAG<T>& operator=(const Mat_DIAG<T> &B);
  Mat_DIAG<T>& operator=(const Vector<T> &V);
  

  Mat_DIAG<T>& copy(const T *vec)
  {
    // assumes sufficient elements, col-major sequence
    Vector<T>::copy(vec); 
    return (*this);
  }

  Mat_DIAG<T>& copy(const Mat_DIAG<T>& B)
  {
    this->operator=(B);
    return (*this);
  }


  //
  // Utilities
  //
  bool  ok() const        { return ((this->v_!=NULL) && (m_D>0)); }
  bool  is_square() const { return (this->ok() && (m_M==m_N) && (m_D==m_N)); }

  int   size() const { return this->m_Len; }
  void  size(int& M, int& N) const { M=m_M; N=m_N; }
  int   dim(int d) const { return (d==1)?m_M:((d==2)?m_N:0); }
  int   num_rows() const { return m_M; }
  int   num_cols() const { return m_N; }
  int   num_diag() const { return m_D; }
  int         M () const { return m_M; }
  int         N () const { return m_N; }
  int         D () const { return m_D; }
  int    max_mn () const { return (m_M > m_N) ? m_M : m_N; }
  int    min_mn () const { return (m_M < m_N) ? m_M : m_N; }

  Mat_DIAG<T>& zeros()             {Vector<T>::fill(T(0)); return (*this);} 
  Mat_DIAG<T>& zeros(int M, int N) {resize(M,N,true,T(0)); return (*this);} 

  Mat_DIAG<T>& ones()              {Vector<T>::fill(T(1)); return (*this);} 
  Mat_DIAG<T>& ones(int M, int N)  {resize(M,N,true,T(1)); return (*this);} 

  Mat_DIAG<T>& fill(const T &x)    {Vector<T>::fill(x);    return (*this);} 
  Mat_DIAG<T>& set_abs()           {Vector<T>::set_abs();  return (*this);} 


  Mat_DIAG<T>& identity(int N)
  {
    // set to (N,N) identity matrix
    resize(N,N, true, this->ONE);
    return (*this);
  }


  Mat_DIAG<T>& diag(const Vector<T>& V)
  {
    // Set vector arg to be matrix diagonal
    (*this) = V;
    return (*this);
  }


  Mat_DIAG<T>& transpose()
  {
    // allow for this to be (logically) rectangular, 
    // but diagonal remains unchanged.
    int tmp=m_N;  m_N=m_M;  m_M=tmp;
    return (*this);
  }


  //
  // Comparison  ("fuzzy", uses m_EqTol)
  //
  bool operator == (const Mat_DIAG<T> &B) const
  {
    if (this->v_ == B.v_)          return true;   // same vector?
    if (B.num_rows() != m_M) return false;  // diff. shape?
    if (B.num_cols() != m_N) return false;  // diff. shape?
    if (B.min_mn()   != m_D) return false;  // diff. shape?

    // compare data in base array
    return Vector<T>::operator==((const Vector<T>&)B);
  }

  bool operator != (const Mat_DIAG<T> &B) const
  {
    bool bRet = this->operator==(B);
    return (!bRet);
  }

  Mat_DIAG<T> operator! () const
  {
    // return a "boolean inverse"
    Mat_DIAG<T>* tmp = new Mat_DIAG<T>((*this), OBJ_temp, "!TMP");

    T* p = tmp->data();
    for (int i=0; i<this->m_Len; ++i)
    {
      // consider fuzzy m_EqTol
      p[i] = (this->ZERO == this->v_[i] ? this->ONE : this->ZERO);
    }
    return (*tmp);
  }


  //
  // Numerical routines.
  //
  // Simple routines are provided to accommodate all 
  // data types, plus calls to BLAS for T=<double>.
  //


  //
  // apply scalar to all elements
  //

  /*virtual*/ void add_val (const T &x) { Vector<T>::add_val (x); }
  /*virtual*/ void mult_val(const T &x) { Vector<T>::mult_val(x);}
  /*virtual*/ void div_val (const T &x) { Vector<T>::div_val (x); }
  /*virtual*/ void pow_val (const T &x) { Vector<T>::pow_val (x); }
  

  //
  // Overloaded arithmetic operators.
  //

  // Pattern for global operators: 
  //  tmp(A); tmp+=x; return tmp;

  Mat_DIAG<T>& operator += (const T &x) {add_val ( x); return (*this);}
  Mat_DIAG<T>& operator -= (const T &x) {add_val (-x); return (*this);}
  Mat_DIAG<T>& operator *= (const T &x) {mult_val( x); return (*this);}
  Mat_DIAG<T>& operator /= (const T &x) {div_val ( x); return (*this);}


  //
  // element-by-element operations (A.+B) all call 
  // unrolled vector version (deleting arg if temp)
  //

  // A .+ B
  Mat_DIAG<T>& operator += (const Mat_DIAG<T> &B) {
    Vector<T>::operator += ((const Vector<T>&)B);
    return (*this);
  }

  // A .- B
  Mat_DIAG<T>& operator -= (const Mat_DIAG<T> &B) {
    Vector<T>::operator -= ((const Vector<T>&)B);
    return (*this);
  }

  // A .+ V
  Mat_DIAG<T>& operator += (const Vector<T> &V) {
    Vector<T>::operator += (V);
    return (*this);
  }

  // A .- V
  Mat_DIAG<T>& operator -= (const Vector<T> &V) {
    Vector<T>::operator -= (V);
    return (*this);
  }


  Mat_DIAG<T>& operator *= (const Mat_DIAG<T> &B)
  {
    assert(B.min_mn()==m_D);  // must be compatible
    m_M = m_N = this->m_D;    // FIXME: forcing to be (logically) square

    // standard matrix multiplication, which in this case reduces 
    // to a vector operation, v1 .* v2.  We can use the unrolled 
    // vector version (which deletes B if B is a "temp" object)
    Vector<T>::operator *= ((const Vector<T>&)B);
    return (*this);
  }


  Mat_DIAG<T>& operator /= (const Mat_DIAG<T> &B)
  {
    assert(B.min_mn()==m_D);  // must be compatible
    m_M = m_N = this->m_D;    // FIXME: force to be (logically) square

    // multiply (*this) by inv(B), which in this case reduces 
    // to a vector operation, v1 ./ v2.  We can use the unrolled 
    // vector version (which deletes B if B is a "temp" object)
    Vector<T>::operator /= ((const Vector<T>&)B);
    return (*this);
  }


  Mat_DIAG<T>& operator |= (const Mat_DIAG<T> &B)
  {
    return this->operator /= (B);
  }


  // Return LU solution to Ax=b
  Vector<T>& operator | (const Vector<T> &b)
  {
    // Return the "LU" solution vector, which in this case 
    // reduces to a vector operation, x(i)=b(i)/A(i,i)

    // must be compatible
    assert( m_D == b.size() );
    
    Vector<T> *x=new Vector<T>(b, OBJ_temp, "inv(A)*b");
    (*x) /= dynamic_cast< Vector<T>& >(*this);
    
    return (*x);
  }


  // A = A .* B
  Mat_DIAG<T>& mult_element(const Mat_DIAG<T> &B)
  {
    // Unrolled vector version (deletes B if temp)
    Vector<T>::operator *= ((const Vector<T>&)B);
    return (*this);
  }

  // A = A .* b
  Mat_DIAG<T>& mult_element(const Vector<T>& b)
  {
    // Unrolled vector version (deletes b if temp)
    Vector<T>::operator *= (b);
    return (*this);
  }

  // A = A .* data
  Mat_DIAG<T>& mult_element(const T* data)
  {
    Vector<T>::operator *= (data);
    return (*this);
  }

  // A = A ./ B
  Mat_DIAG<T>& div_element (const Mat_DIAG<T> &B)
  {
    // Unrolled vector version (deletes B if temp)
    Vector<T>::operator /= ((const Vector<T>&)B);
    return (*this);
  }

  // A = A ./ b
  Mat_DIAG<T>& div_element (const Vector<T>& b)
  {
    // Unrolled vector version (deletes b if temp)
    Vector<T>::operator /= (b);
    return (*this);
  }

  // (*this) is not changed: C = A ./ B
  Mat_DIAG<T>& dd(const Mat_DIAG<T> &B) const
  {
    // FIXME: check side-effects if OBJ_temp
    // FIXME: tmp ctor may delete (*this)

    std::string sz; tmp_op_name(this->name(), "./", B.name(), sz);
    Mat_DIAG<T> *tmp=new Mat_DIAG<T>((*this), OBJ_temp, sz.c_str());
    (*tmp).div_element(B);
    return (*tmp);
  }
  
  // (*this) is not changed: C = A .* B
  Mat_DIAG<T>& dm(const Mat_DIAG<T> &B) const
  {
    // FIXME: check side-effects if OBJ_temp
    // FIXME: tmp ctor may delete (*this)

    std::string sz; tmp_op_name(this->name(), ".*", B.name(), sz);
    Mat_DIAG<T> *tmp=new Mat_DIAG<T>((*this), OBJ_temp, sz.c_str());
    (*tmp).mult_element(B);
    return (*tmp);
  }

  // (*this) is not changed: C = A .* V
  Mat_DIAG<T>& dm(const Vector<T> &V) const
  {
    std::string sz; tmp_op_name(this->name(), ".*", V.name(), sz);
    Mat_DIAG<T> *tmp=new Mat_DIAG<T>((*this), OBJ_temp, sz.c_str());
    (*tmp).mult_element(V);
    return (*tmp);
  }

  // (*this) is not changed: C = A .* data
  Mat_DIAG<T>& dm(const T* data) const
  {
    // assumes data has sufficinet elements
    std::string sz; tmp_op_name(this->name(), ".*", "v", sz);
    Mat_DIAG<T> *tmp=new Mat_DIAG<T>((*this), OBJ_temp, sz.c_str());
    (*tmp).mult_element(data);
    return (*tmp);
  }



  //
  // element access
  //

  //
  // single (1-based) index into base array
  //
        T& operator()(int i)       { return Vector<T>::operator()(i); }
  const T& operator()(int i) const { return Vector<T>::operator()(i); }

  //
  // 1-based element access (make "virtual" if required)
  //
  T& operator()(int i, int j)
  {
    CheckIdx_IJ_1(i,j); 
#if (ALLOW_NON_DIAG_IDX)
    return (i==j) ? this->vm1_[i] : this->ZERO;
#else
    assert(i==j);
    return this->vm1_[i];
#endif
  }

  const T& operator()(int i, int j) const
  {
    CheckIdx_IJ_1(i,j); 
#if (ALLOW_NON_DIAG_IDX)
    return (i==j) ? this->vm1_[i] : this->ZERO;
#else
    assert(i==j);
    return this->vm1_[i];
#endif
  }


  //
  // get MAX row/col values
  //
  T max_row_val    (const int i) const {CheckIdx_Row_1(i); if(m_N<1) return this->ZERO; return          this->vm1_[i]; }
  T max_row_val_abs(const int i) const {CheckIdx_Row_1(i); if(m_N<1) return this->ZERO; return std::abs(this->vm1_[i]);}
  T max_col_val    (const int j) const {CheckIdx_Col_1(j); if(m_M<1) return this->ZERO; return          this->vm1_[j]; }
  T max_col_val_abs(const int j) const {CheckIdx_Col_1(j); if(m_M<1) return this->ZERO; return std::abs(this->vm1_[j]);}

  //
  // get MIN row/col values
  //
  T min_row_val    (const int i) const {CheckIdx_Row_1(i); if(m_N<1) return this->ZERO; return          this->vm1_[i]; }
  T min_row_val_abs(const int i) const {CheckIdx_Row_1(i); if(m_N<1) return this->ZERO; return std::abs(this->vm1_[i]);}
  T min_col_val    (const int j) const {CheckIdx_Col_1(j); if(m_M<1) return this->ZERO; return          this->vm1_[j]; }
  T min_col_val_abs(const int j) const {CheckIdx_Col_1(j); if(m_M<1) return this->ZERO; return std::abs(this->vm1_[j]);}


  //
  // set/get entire rows/cols
  //

  // return row as a ZERO vector with "diagonal" element set
  // NOTE: return value must be assigned or deleted!
  Vector<T>& get_row(const int i) const
  {
    CheckIdx_Row_1(i);
    int N = this->num_cols();
    Vector<T>* tmp = new Vector<T>(N, T(0), OBJ_temp, "row");
    tmp(i) = this->vm1_[i];
    return (*tmp);
  }

  // return column as a ZERO vector with "diagonal" element set
  // NOTE: return value must be assigned or deleted!
  Vector<T>& get_col(const int j) const
  {
    CheckIdx_Col_1(j);
    int M = this->num_rows();
    Vector<T>* tmp = new Vector<T>(M, T(0), OBJ_temp, "col");
    tmp(j) = this->vm1_[j];
    return (*tmp);
  }
  
  void set_row(const int i, const double &x)
  {
    // set value of diagonal element in row i
    CheckIdx_Row_1(i);
    this->vm1_[i] = x;
  }

  void set_row(const int i, const Vector<T> &vec)
  {
    // set value of diagonal element in row i
    CheckIdx_Row_1(i);
    assert(vec.size() >= m_N);    // sufficient values?
    this->vm1_[i] = vec(i);

    if (vec.get_mode()==OBJ_temp) {
      delete (&vec);
    }
  }

  void set_col(const int j, const double &x)
  {
    // set value of diagonal element in column j
    CheckIdx_Col_1(j);
    this->vm1_[j] = x;
  }

  void set_col(const int j, const Vector<T> &vec)
  {
    // set value of diagonal element in column j
    CheckIdx_Col_1(j);
    assert(vec.size() >= m_M);    // sufficient values?
    this->vm1_[j] = vec(j);

    if (vec.get_mode()==OBJ_temp) {
      delete (&vec);
    }
  }

  void add_col(const int j, const Vector<T> &vec)
  {
    // Add a value to diagonal element in column j
    CheckIdx_Col_1(j);
    assert(vec.size() >= m_M);    // sufficient values?
    this->vm1_[j] += vec(j);

    if (vec.get_mode()==OBJ_temp) {
      delete (&vec);
    }
  }

  void sub_col(const int j, const Vector<T> &vec)
  {
    // Subtract a value from diagonal element in column j
    CheckIdx_Col_1(j);
    assert(vec.size() >= m_M);    // sufficient values?
    this->vm1_[j] -= vec(j);

    if (vec.get_mode()==OBJ_temp) {
      delete (&vec);
    }
  }


  //
  // matrix norms
  //


  // matrix 1-norm: max column sum
  double norm1() const     {return Vector<T>::max_val_abs();}
  
  // matrix infinity-norm: max row sum
  double norm_inf() const  {return Vector<T>::max_val_abs();}

  // matrix frobenius-norm
  double norm_frob() const {return sqrt(this->sumsquares());}



  // #####################################################
  //
  // Operations only appropriate for <double>
  //
  // #####################################################

public:

  Mat_DIAG<T>& hilbert(int N)
  {
    // load diagonal of (N,N) Hilbert matrix
    this->resize(N,N, false);
    for (int i=1; i<=N; ++i)
      this->vm1_[i] = (T) (1.0 / double(i+i-1));

    return (*this);
  }


  //-------------------------------------
  Mat_DIAG<T>& invert()
  //-------------------------------------
  {
    if (sizeof(T) != sizeof(double)) {
      umLOG(1, "invert only available for type = <double>.\n");
      umERROR("Mat_DIAG::invert()", "matrix type is not <double>.");
    }

    if (!this->is_square()) {
      umLOG(1, "TODO: pseudo-inverse for general (M,N) matrices.\n");
      umERROR("Mat_DIAG::invert()", "matrix is not square.");
    }

    for (int i=0; i<m_D; ++i) {
      this->v_[i] = 1.0 / this->v_[i];
    }

    return (*this);
  }


  //
  // Adjustable output format.
  //
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

  
  //---------------------------------------------
  // Region1D: 1D regions of Vector<T> objects
  //---------------------------------------------
public:

  // Construct with a Region1D along the diagonal
  Mat_DIAG(const Region1D< Vector<T> > &R, OBJ_mode md=OBJ_real, const char* sz="diag")
    : Vector<T>(R,md,sz),  // vector = region
      m_M(0), m_N(0), m_MN(0), m_D(0)
  {
    int D = this->size();
    set_counters(D,D);
  }

  // rebuild with a Region1D along the diagonal
  Mat_DIAG<T>& operator = (const Region1D< Vector<T> > &R)
  {
    Vector<T> vec(R);
    (*this) = vec;
    return (*this);
  }

};



///////////////////////////////////////////////////////////
//
// constructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
Mat_DIAG<T>::Mat_DIAG(const char* sz, OBJ_mode md)
//---------------------------------------------------------
  : Vector<T>(sz, md), 
    m_M(0), m_N(0), m_MN(0), m_D(0)
{}


// not "explicit"; allows construction from return objects
//---------------------------------------------------------
template <typename T> inline
Mat_DIAG<T>::Mat_DIAG(const Mat_DIAG<T> &B, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(sz, md), 
  m_M(0), m_N(0), m_MN(0), m_D(B.min_mn())
{
  int Nr = B.m_M, Nc = B.m_N;
  this->m_mode = md;

  // manage copy of real/temp objects
  // deletes B, if temporary.
  Vector<T>::operator= ((const Vector<T>&) B);

  // FIXME: Allow (logically) rectangular?
  if (Nr>0 && Nc>0) {
    set_counters(Nr,Nc);
  }
}


//---------------------------------------------------------
template <typename T> inline
Mat_DIAG<T>::Mat_DIAG(const Vector<T> &V, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(V,md,sz),  // deletes V, if temporary.
  m_M(0), m_N(0), m_MN(0), m_D(0)
{
  int D = this->size();
  set_counters(D,D);
}


//---------------------------------------------------------
template <typename T> inline
Mat_DIAG<T>::Mat_DIAG(int N, const char* sz)
//---------------------------------------------------------
: Vector<T>(N, sz),
  m_M(N), m_N(N), m_MN(0), m_D(N)
{
  set_counters(N,N);  // adjust logical size
}


//---------------------------------------------------------
template <typename T> inline
Mat_DIAG<T>::Mat_DIAG(int N, const T x, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(N, x, md, sz),
  m_M(0), m_N(0), m_MN(0), m_D(N)
{
  set_counters(N,N);  // adjust logical size
}


//---------------------------------------------------------
template <typename T> inline
Mat_DIAG<T>::Mat_DIAG(int N, const T *data, OBJ_mode md, const char* sz)
//---------------------------------------------------------
: Vector<T>(N, data, md, sz), 
  m_M(0), m_N(0), m_MN(0), m_D(N)
{
  set_counters(N,N);  // adjust logical size
}


///////////////////////////////////////////////////////////
//
// destructors
//
///////////////////////////////////////////////////////////

template <typename T> inline
Mat_DIAG<T>::~Mat_DIAG() 
{
  destroy(); 
}


template <typename T> inline
void Mat_DIAG<T>::destroy()
{
  // Base class Vector manages deallocation of data,
  Vector<T>::destroy();
  m_M = m_N = m_MN = m_D = 0;
}


// "Borrowing" data allows wrapping external arrays 
// to exploit matrix algorithms while avoiding any
// overhead of copying data.
//---------------------------------------------------------
template <typename T> inline
void Mat_DIAG<T>::borrow(int M, int N, T* p)
//---------------------------------------------------------
{
  assert(M*N >=0);    // must be non-negative
  assert(p);          // expecting valid data

  // base class calls virtual destroy() to reset
  // all members, and handles the storage.

  // only borrow elements for diagonal = min(M,N)
  m_D = std::min(M,N);
  Vector<T>::borrow(m_D, p);

  if (M>0 && N>0) {
    // prepare index arrays
    set_counters(M, N);
  }
}


//---------------------------------------------------------
template <typename T> inline
void Mat_DIAG<T>::borrow(const Vector<T>& CV)
//---------------------------------------------------------
{
  Vector<T>& V = const_cast<Vector<T>&>(CV);
  int N = V.length();
  T* p = V.data();
  this->borrow(N,N, p);
}



// The internal contiguous (0-offset) array v_[D] is
// allocated in base class Vector.  Here we simply 
// set the logical size, which may be rectangular.
//---------------------------------------------------------
template <typename T> inline
void Mat_DIAG<T>::set_counters(int M, int N)
//---------------------------------------------------------
{
  assert( this->v_ );     // data allocated in Vector::initialize()
  assert( M >= 1);
  assert( N >= 1);

  m_M  = M;        // num rows
  m_N  = N;        // num cols
  m_MN = M*N;      // total (logical) elements
  m_D  = std::min(M,N); // actual length of stored array    
}


//---------------------------------------------------------
template <typename T> inline
bool Mat_DIAG<T>::resize(int M, int N, bool bInit, T x)
//---------------------------------------------------------
{
  // The internal contiguous (0-offset) array 
  // v_[D] is allocated by base class Vector. 
  //
  // Call set_counters(M,N) to set up logical size

  // Resize existing object, optionally setting 
  // the entire array to some given inital value.
  // Return value indicates whether size has changed.

  assert(M >= 0);
  assert(N >= 0);

  int D = std::min(M,N);  // actual length of stored array

  bool bResized=false;

  if ((M != m_M) || (N != m_N) || (D != m_D)) 
  {
    bResized=true;
    this->destroy();      // clear allocation, zero members
    if (D > 0) 
    {
      initialize(D, bInit, x);  // allocate array with D elements
      set_counters(M, N);       // adjust logical size
    }
  }
  else if (bInit && this->m_Len>0)
  {
    fill(x);              // no resize, just fill existing array
  }

  m_M  = M;     // num rows
  m_N  = N;     // num cols
  m_MN = M*N;   // total (logical) elements
  m_D  = D;     // total (actual) elements

  return bResized;
}



///////////////////////////////////////////////////////////
//
// assignment
//
///////////////////////////////////////////////////////////

template <typename T> inline
Mat_DIAG<T>& Mat_DIAG<T>::operator=(const Mat_DIAG<T> &B)
{
  if (this->v_ == B.v_)
    return (*this);

  int M=B.m_M, N=B.m_N;
  if (this->m_name=="diag" || this->m_name.empty()) 
    this->m_name = B.name();

  // base class manages the actual allocation
  Vector<T>::operator= ((const Vector<T>&) B);

  // adjust logical size
  set_counters(M, N);

  return (*this);
}


template <typename T> inline
Mat_DIAG<T>& Mat_DIAG<T>::operator=(const Vector<T> &V)
{
  // Set vector arg to be matrix diagonal

  if (this->v_ == V.data())
    return (*this);

  if (this->m_name=="diag" || this->m_name.empty()) 
    this->m_name=V.name();

  int N = V.size();
  Vector<T>::operator= (V);
  set_counters(N,N);

  return (*this);
}



///////////////////////////////////////////////////////////
//
// I/O: write a format that can be read by Mat_DIAG<T>
//
///////////////////////////////////////////////////////////

template <typename T>
std::ostream& operator<<(std::ostream &s, const Mat_DIAG<T> &A)
{
  int M = A.M(), N = A.N(), D = A.D();
  s << M << "  " << N << "  " << D << "\n";

  // I/O iosflags
  // s << std::setiosflags(std::ios::scientific);
  // s << std::setiosflags(std::ios::fixed);
  // s << std::setprecision(3);

  for (int i=1; i<=D; ++i) {
    s << A(i) << "  ";
    // TLB: was (j%10).  Should be i%10?
    if (0==(i%10)) 
      s << "\n";
  }
  s << std::endl;
  return s;
}


template <typename T>
std::istream& operator>>(std::istream &s, Mat_DIAG<T> &A)
{
  int M=0, N=0, D=0;
  s >> M >> N >> D;
  A.resize(M,N);
  for (int i=1; i<=D; ++i) {
    s >> A(i);
  }
  return s;
}


//---------------------------------------------------------
template <typename T>
void Mat_DIAG<T>::print
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
  sprintf(buf, "%c%d.%d%s ", '%',wdth, prec,fmt);

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
void Mat_DIAG<T>::print_STREAM
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



//
// Basic matrix algorithms
//


// DMat_Diag& inv   (const DMat_Diag& A);
// DMat_Diag& trans (const DMat_Diag& A);
// DMat_Diag& abs   (const DMat_Diag& A);
// DMat_Diag& sqrt  (const DMat_Diag& A);
// DMat_Diag& sqr   (const DMat_Diag& A);
// DMat_Diag& exp   (const DMat_Diag& A);


inline DMat_Diag& inv (const DMat_Diag& A)
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "inv(%s)", A.name()); 

  DMat_Diag *tmp=new DMat_Diag(A, OBJ_temp, buf);
  tmp->invert();
  return (*tmp);
}


template <typename T>
inline Mat_DIAG<T>& trans(const Mat_DIAG<T>& A)
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "tr(%s)", A.name()); 

  DMat_Diag *tmp=new DMat_Diag(A, OBJ_temp, buf);
  tmp->transpose();
  return (*tmp);
}


template <typename T>
inline Mat_DIAG<T>& abs (const Mat_DIAG<T>& A)
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "abs(%s)", A.name()); 

  DMat_Diag *tmp=new DMat_Diag(A, OBJ_temp, buf);
  tmp->set_abs();
  return (*tmp);
}


inline DMat_Diag& sqrt(const DMat_Diag& A)
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "sqrt(%s)", A.name()); 

  DMat_Diag *tmp=new DMat_Diag(A, OBJ_temp, buf);
  tmp->SQRT();
  return (*tmp);
}


template <typename T>
inline Mat_DIAG<T>& sqr(const Mat_DIAG<T>& A)
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "(%s)^2", A.name()); 

  DMat_Diag *tmp=new DMat_Diag(A, OBJ_temp, buf);
  tmp->SQR();
  return (*tmp);
}


inline DMat_Diag& exp(const DMat_Diag& A)
{
  static char buf[100]={""};
  snprintf(buf, (size_t)90, "exp(%s)", A.name()); 

  DMat_Diag *tmp=new DMat_Diag(A, OBJ_temp, buf);
  tmp->exp_val();
  return (*tmp);
}



//
// Addition:
//   diag   + diag
//   diag   + vector
//   vector + diag
//   diag   + scalar
//   scalar + diag
//

// diag + diag
template <typename T> inline 
Mat_DIAG<T>& operator+(const Mat_DIAG<T> &A, const Mat_DIAG<T> &B)
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

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += B;
  return (*tmp);
}

// diag + vector
template <typename T> inline 
Mat_DIAG<T>& operator+(const Mat_DIAG<T> &A, const Vector<T> &V)
{
  std::string sz; tmp_op_name(A.name(),"+",V.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += V;
  return (*tmp);
}

// vector + diag
template <typename T> inline 
Mat_DIAG<T>& operator+(const Vector<T> &V, const Mat_DIAG<T> &A)
{
  std::string sz; tmp_op_name(V.name(),"+",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += V;
  return (*tmp);
}

// diag + scalar
template <typename T> inline 
Mat_DIAG<T>& operator+(const Mat_DIAG<T> &A, const T &x)
{
  std::string sz; tmp_op_name(A.name(),"+","x", sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


// scalar + diag
template <typename T> inline 
Mat_DIAG<T>& operator+(const T &x, const Mat_DIAG<T> &A)
{
  std::string sz; tmp_op_name("x","+",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) += x;
  return (*tmp);
}


//
// Subtraction:
//   diag   - diag
//   diag   - vector
//   vector - diag
//   diag   - scalar
//   scalar - diag
//

// diag - diag
template <typename T> inline 
Mat_DIAG<T>& operator-(const Mat_DIAG<T> &A, const Mat_DIAG<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"-",B.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= B;
  return (*tmp);
}

// diag = diag - vector
template <typename T> inline 
Mat_DIAG<T>& operator-(const Mat_DIAG<T> &A, const Vector<T> &V)
{
  std::string sz; tmp_op_name(A.name(),"-",V.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= V;
  return (*tmp);
}

// diag = vector - diag
template <typename T> inline 
Mat_DIAG<T>& operator-(const Vector<T> &V, const Mat_DIAG<T> &A)
{
  std::string sz; tmp_op_name(V.name(),"-",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(V, OBJ_temp, sz.c_str());
  (*tmp) -= A;
  return (*tmp);
}

// diag - scalar
template <typename T> inline 
Mat_DIAG<T>& operator-(const Mat_DIAG<T> &A, const T &x)
{
  std::string sz; tmp_op_name(A.name(),"-","x", sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) -= x;
  return (*tmp);
}

// scalar - diag
template <typename T> inline 
Mat_DIAG<T>& operator-(const T &x, const Mat_DIAG<T> &A) 
{ 
  //
  // create a matrix filled with scalar x; subtract A
  //
  std::string sz; tmp_op_name("x","-",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A.D(), x, OBJ_temp, sz.c_str());
  (*tmp) -= A;
  return (*tmp);
}



//
// Division :
//   diag   | diag  ...  matlab "right division"
//   diag   / diag  ...  "multiply by inverse"
//   diag   / scalar
//   scalar / diag
//

// NBN: using "||" due to overload abiguities
template <typename T> inline 
Mat_DIAG<T>& operator|| (const Mat_DIAG<T> &A, const Mat_DIAG<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"|",B.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) |= B;
  return (*tmp);
}


template <typename T> inline 
Mat_DIAG<T>& operator/ (const Mat_DIAG<T> &A, const Mat_DIAG<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"/",B.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= B;
  return (*tmp);
}


template <typename T> inline
Mat_DIAG<T>& operator/ (const Mat_DIAG<T> &A, const T &x) 
{
  assert(fabs(x)>0.0); 

  std::string sz; tmp_op_name(A.name(),"/","x", sz);
  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) /= x;
  return (*tmp);
}


template <typename T> inline 
Mat_DIAG<T>& operator/ (const T &x, const Mat_DIAG<T> &A) 
{ 
  //
  // create a matrix filled with scalar x; div_element(A)
  //
  std::string sz; tmp_op_name("x","/",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A.M(), A.N(), x, OBJ_temp, sz.c_str());
  tmp->div_element(A);
  return (*tmp);
}


//
// Multiplication :
//
// 1 :  diag * diag : D = D*D
//
// 2a:  diag * scal : D = D*x
// 2b:  scal * diag : D = x*D
//
// 3a:  diag * vec  : x = D*v
// 3b:  vec  * diag : x = v*D
//
// 4a:  dense * diag : M = A*D
// 4b:  diag * dense : M = D*A
//

// 1 :  diag * diag : D = D*D
template <typename T> inline 
Mat_DIAG<T>& operator*(const Mat_DIAG<T> &A, const Mat_DIAG<T> &B)
{
  std::string sz; tmp_op_name(A.name(),"*",B.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= B;
  return (*tmp);
}


// 2a:  diag * scal : D = D*x
template <typename T> inline 
Mat_DIAG<T>& operator*(const Mat_DIAG<T> &A, const T &x)
{
  std::string sz; tmp_op_name(A.name(),"*","x", sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}

// 2b:  scal * diag : D = x*D
template <typename T> inline 
Mat_DIAG<T>& operator*(const T &x, const Mat_DIAG<T> &A) 
{
  std::string sz; tmp_op_name("x","*",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= x;
  return (*tmp);
}


// 3a:  diag * vec : x = D*v
template <typename T> inline
Vector<T>& operator* (const Mat_DIAG<T> &A, const Vector<T> &V) 
{
  // allow diagonal matrix to be (logically) rectangular
  int rows=A.num_rows(), cols=A.num_cols(), N=V.size();
  assert(cols==N);
  
  Vector<T>* X = new Vector<T>(rows, T(0), OBJ_temp, "A*v");
  
  const T* Ai=A.data(); 
  const T* vi=V.data();
        T* xi=X->data();

  // Only dot non-zero part of A's diagonal
  for (int i=0; i<A.D(); ++i, ++Ai,++vi,++xi)
    (*xi) = (*Ai) * (*vi);

  // clean up temps
  if (OBJ_temp == A.get_mode()) {delete (&A);}
  if (OBJ_temp == V.get_mode()) {delete (&V);}

  return (*X);
}

// 3b:  vec  * diag : x = v*D
template <typename T> inline
Vector<T>& operator* (const Vector<T> &V, const Mat_DIAG<T> &A) 
{ 
  // allow diagonal matrix to be (logically) rectangular
  int rows=A.num_rows(), cols=A.num_cols(), N=V.size();
  assert(N==rows);
  
  Vector<T>* X = new Vector<T>(cols, T(0), OBJ_temp, "v*A");
  
  const T* vi=V.data();
  const T* Ai=A.data(); 
        T* xi=X->data();

  // Only dot non-zero part of A's diagonal
  for (int i=0; i<A.D(); ++i, ++Ai,++vi,++xi)
    (*xi) = (*vi) * (*Ai);

  // clean up temps
  if (OBJ_temp == A.get_mode()) {delete (&A);}
  if (OBJ_temp == V.get_mode()) {delete (&V);}

  return (*X);
}


// 4a:  dense * diag : M = A*D
template <typename T> inline
Mat_COL<T>& operator* (const Mat_COL<T> &A, const Mat_DIAG<T> &D) 
{
  std::string sz; tmp_op_name(A.name(),"*",D.name(), sz);

  // only using non-zero part of (square) diagonal matrix
  int cols=A.num_cols();
  int tD=D.D(); // actual number of diagonal elements

  if (cols != tD) { umERROR("dense * diag", "wrong dimensions"); }

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());

  // algorithm reduces to scaling the COLUMNS of A
  for (int j=1; j<=cols; ++j) {
    tmp->scale_col(j, D(j));
  }

  // if D is temporary, delete it.
  if (D.get_mode() == OBJ_temp) { delete (&D); }

  return (*tmp);
}

// 4b:  diag * dense : M = D*A
template <typename T> inline
Mat_COL<T>& operator* (const Mat_DIAG<T> &D, const Mat_COL<T> &A) 
{
  std::string sz; tmp_op_name(D.name(),"*",A.name(), sz);

  // only using non-zero part of (square) diagonal matrix
  int rows=A.num_rows(), cols=A.num_cols();
  int tD=D.D(); // actual number of diagonal elements

  if (rows != tD) { umERROR("diag * dense", "wrong dimensions"); }

  Mat_COL<T> *tmp=new Mat_COL<T>(A, OBJ_temp, sz.c_str());

  // algorithm reduces to scaling the ROWS of A
  for (int j=1; j<=cols; ++j) {
    tmp->scale_col(j, D);
  }

  // if D is temporary, delete it.
  if (D.get_mode() == OBJ_temp) { delete (&D); }

  return (*tmp);
}




// negation (unary operator)
template <typename T> inline 
Mat_DIAG<T>& operator- (const Mat_DIAG<T> &A) 
{
  std::string sz; tmp_op_name(" ","-",A.name(), sz);

  Mat_DIAG<T> *tmp=new Mat_DIAG<T>(A, OBJ_temp, sz.c_str());
  (*tmp) *= T(-1);
  return (*tmp);
}



//
// Matlab "element-wise" operations:  C = A .* B
//                                    C = A ./ B
//
template <typename T> inline 
void mult_element(const Mat_DIAG<T> &A, const Mat_DIAG<T> &B,  Mat_DIAG<T> &C) 
{ 
  C = A; 
  C.mult_element(B); 
}


template <typename T> inline 
void div_element(const Mat_DIAG<T> &A,  const Mat_DIAG<T> &B,  Mat_DIAG<T> &C) 
{
  C = A; 
  C.div_element(B); 
}


//---------------------------------------------------------
template <typename T> inline
void matmult
( 
  const Mat_DIAG<T> &A, 
  const Mat_DIAG<T> &B,
        Mat_DIAG<T> &C 
)
//---------------------------------------------------------
{
  assert(A.num_cols() == B.num_rows());
  int M = A.num_rows();
  int N = A.num_cols();
  int K = B.num_cols();

  T ZERO = T(0), sc = T(0);
  double* colk = NULL;

  // Allow diagonal result to be (logically) rectangular
  // Adjust shape of C, and set to zero
  C.resize(M,K, true, ZERO);

  const T* Ai = A.data();
  const T* Bi = B.data();
        T* Ci = C.data();

  // Only dot the non-zero parts of the diagonals
  int tD = std::min(A.D(), B.D());
  for (int i=0; i<tD; ++i, ++Ai,++Bi,++Ci)
  {
    Ci = Ai*Bi;
  }
}



//
// double precision specializations
//
void umPOLISH  (DMat_Diag& A, double eps = 1e-10);

#endif  // NDG__Mat_DIAG_H__INCLUDED
