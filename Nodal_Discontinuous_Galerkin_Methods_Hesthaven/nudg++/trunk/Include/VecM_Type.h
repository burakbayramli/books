// VecM_Type.h
// dense vector allocated in multiple blocks
// support for oversized sparse CS<T> objects
// 2007/01/01
//---------------------------------------------------------
#ifndef NDG__VecM_BLOCK_H__INCLUDED
#define NDG__VecM_BLOCK_H__INCLUDED


//#########################################################
// NBN: EXPERIMENTAL
//#########################################################
// This class implements a dense vector allocated 
// in multiple blocks.  Each block is a VectorM<T>. 
// Intended to support sparse CS<T> objects that 
// are too large for single contiguous allocation.
// Interface indexing is 1-based (i,j)
//#########################################################


#include "Vec_Type.h"


// typedef versions for common data types
template <typename T> class VectorM;

typedef VectorM<double>  DVecM;
typedef VectorM<dcmplx>  ZVecM;
typedef VectorM<int>     IVecM;
typedef VectorM<long>    LVecM;


//---------------------------------------------------------
template <typename T> 
class VectorM
//---------------------------------------------------------
{
  //
  // Member data
  //
protected:

  int   m_Len;        // total elements (i.e. sum of blocks)
  int   n1,n2;        // length of each block (TODO: allow d blocks)
  VectorM<T>**  V_;   // array of vector blocks

  std::string m_name;     // identifier string
  double      m_EqTol;    // allow "fuzzy" comparison
  bool        m_borrowed; // is data borrowed?
  int         m_id;       // identifier number
  OBJ_mode    m_mode;     // real or temporary

  static int  s_count;    // track number of objects

public:


  // constructors
  explicit VectorM(const char* sz="vecM", OBJ_mode md=OBJ_real);
  explicit VectorM(int N, const char* sz="vecM", OBJ_mode md=OBJ_real);
  explicit VectorM(std::istream &is, const char* sz="vecM", OBJ_mode md=OBJ_real);

  // destructor
  virtual ~VectorM();
  void destroy();
  void Free() { destroy(); }

  // manage allocation
  VectorM<T>& borrow(int N, T* p)   { return (*this); }
  VectorM<T>& own (VectorM<T>& B)   { return (*this); }

  void initialize(int N, bool bInit=true, T x=T(0));

  bool resize(int N, bool bInit=true, T x=T(0));
  bool extend(int N, bool bInit=true, T x=T(0));
  bool realloc(int N, bool bInit=true, T x=T(0));

  // copy/load
  VectorM<T>& copy(const T *vec)        { return (*this); }
  VectorM<T>& copy(int N, const T *vec) { return (*this); }
  void load(int N, const char *sdata)   { }

  // assignment
  VectorM<T>& operator=(const T &x);
  VectorM<T>& operator=(const VectorM<T> &B);

  VectorM<T>& append(const VectorM<T>& B);

  // utilities
  bool  ok() const                { return ((V_!=NULL) && (m_Len>0)); }
  int   size() const              { return  m_Len; }
  int   length() const            { return  m_Len; }
  void  set_name(const char* sz)  { m_name = sz; }
  const char* name() const        { return m_name.c_str(); }
  bool  is_borrowed() const       { return m_borrowed; }
  int   get_s_count() const       { return s_count; }

  // "mode"
  OBJ_mode get_mode() const       { return m_mode; }
  void set_mode(OBJ_mode mode)    { m_mode = mode; }


  //-------------------------------------
  // element access
  //-------------------------------------

  // set/get:  1-based access
  const T& operator()(int i) const;
        T& operator()(int i);

  // set/get:  0-based access
  const T& operator[](int i) const;
        T& operator[](int i);


protected:
  //-------------------------------------
  // Optional index checking
  //-------------------------------------
#if (CHECK_ARRAY_INDEX)

  void Check_index_0(int i) const {
    // check for legal 0-based index
    if (i< 0 )    throw 1;
    if (i>=m_Len) throw 2;
  }
  void Check_index_1(int i) const {
    // check for legal 1-based index
    if (i<1 )     throw 1;
    if (i>m_Len)  throw 2;
  }

#else

  // no checking of indices
  void Check_index_0(int i) const throw() {}
  void Check_index_1(int i) const throw() {}

#endif

};



///////////////////////////////////////////////////////////
//
// define static member data
//
///////////////////////////////////////////////////////////

template <typename T> int VectorM<T>::s_count=0;



///////////////////////////////////////////////////////////
//
// constructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
VectorM<T>::VectorM(const char* sz, OBJ_mode md);
//---------------------------------------------------------
: m_Len(0), n1(0), n2(0), V_(NULL), 
  m_name(xz), m_borrowed(false), m_mode(md)
{
  ++s_count;
}



//---------------------------------------------------------
template <typename T> inline
VectorM<T>::VectorM(int N, const char* sz, OBJ_mode md);
//---------------------------------------------------------
: m_Len(0), n1(0), n2(0), V_(NULL), 
  m_name(xz), m_borrowed(false), m_mode(md)
{
  ++s_count;
  resize(N);
}


//---------------------------------------------------------
template <typename T> inline
VectorM<T>::VectorM(std::istream &is, const char* sz, OBJ_mode md);
//---------------------------------------------------------
: m_Len(0), n1(0), n2(0), V_(NULL), 
  m_name(xz), m_borrowed(false), m_mode(md)
{
  ++s_count;
  umWARNING("VectorM(istream&)", "TODO: please implement");
}



///////////////////////////////////////////////////////////
//
// destructors
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T>
VectorM<T>::~VectorM()
//---------------------------------------------------------
{
  --s_count;

  if (V_) {
    destroy();
  }
}


//---------------------------------------------------------
template <typename T> inline
void VectorM<T>::destroy()
//---------------------------------------------------------
{
  if (V_ && (!m_borrowed)) {
    for (int i=0; i<2; ++i) {
      delete V_[i]; V_[i]=NULL;  // delete ith block
    }
    delete [] V_;   // delete the array of pointers
  }

  V_ = NULL;    // Pointers may be tested, 
  m_Len = 0;    // no data left
  n1=n2 = 0;
}



///////////////////////////////////////////////////////////
//
// manage allocation
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
void VectorM<T>::initialize(int N, bool bInit, T x)
//---------------------------------------------------------
{
  // check size of request in bytes
  double block = (double)(N * sizeof(int));
  if (block > 200.0 * 1024.0 * 1024.0) {
    umWARNING("VectorM<T>::initialize", 
              "requested allocation is %lf MB", 
              block/(1024.0 * 1024.0));
  }

  // set total length and chunck sizes
  m_Len=N; n1=N/2; n2=N-n1;

  try {
    V_ = new Vector<T>*[2];
    V_[0] = new Vector<T>(n1, bInit, x);
    V_[1] = new Vector<T>(n2, bInit, x);
  } 
  catch (...) 
  {
    umERROR("", "out of memory");
  }
}


//---------------------------------------------------------
template <typename T> inline
bool VectorM<T>::resize(int N, bool bInit, T x)
//---------------------------------------------------------
{
  return false;
}


//---------------------------------------------------------
template <typename T> inline
bool VectorM<T>::extend(int N, bool bInit, T x)
//---------------------------------------------------------
{
  return false;
}


//---------------------------------------------------------
template <typename T> inline
bool VectorM<T>::realloc(int N, bool bInit, T x)
//---------------------------------------------------------
{
  // alias for extend
  return extend(N, bInit, x);
}


///////////////////////////////////////////////////////////
//
// set/get:  1-based access
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
T& VectorM<T>::operator()(int i)
//---------------------------------------------------------
{
  assert(ok());
  Check_index_1(i);
  if (i<=n1) return V_[0]->operator()(i);
  else       return V_[1]->operator()(i);
}


//---------------------------------------------------------
template <typename T> inline
const T& VectorM<T>::operator()(int i) const
//---------------------------------------------------------
{
  assert(ok());
  Check_index_1(i); 
  if (i<=n1) return V_[0]->operator()(i);
  else       return V_[1]->operator()(i);
}


///////////////////////////////////////////////////////////
//
// set/get:  0-based access
//
///////////////////////////////////////////////////////////


//---------------------------------------------------------
template <typename T> inline
T& VectorM<T>::operator[](int i)
//---------------------------------------------------------
{
  Check_index_0(i); 
  if (i<n1) return V_[0]->operator[](i);
  else      return V_[1]->operator[](i);
}


//---------------------------------------------------------
template <typename T> inline
const T& VectorM<T>::operator[](int i) const
//---------------------------------------------------------
{
  Check_index_0(i);
  if (i<n1) return V_[0]->operator[](i);
  else      return V_[1]->operator[](i);
}



#endif  // NDG__VecM_BLOCK_H__INCLUDED
