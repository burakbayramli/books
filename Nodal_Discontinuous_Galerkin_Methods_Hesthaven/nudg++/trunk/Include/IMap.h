// IMap.h
// index vector for mapping regions of arrays
// 2006/12/17
//---------------------------------------------------------
#ifndef NDG__IMap_H__INCLUDED
#define NDG__IMap_H__INCLUDED


#include <vector>

// template <typename T> class Vector;


//---------------------------------------------------------
class IMap : public std::vector<int>
//---------------------------------------------------------
{
  // Note: inherits std::vector<> members

  typedef std::vector<int> iBASE;

public:
  IMap(int N=0)        : iBASE((size_type)N) {}
  IMap(const IMap& B)  : iBASE((iBASE&)B) {}
  IMap(const iBASE& B) : iBASE(B) {}
  IMap(int N, const int* p) { copy(N, p); }
  ~IMap() {}

  // set/get:  1-based access
  inline       int& operator()(int i)       {return iBASE::operator[]((size_type)(i)-1);}
  inline const int& operator()(int i) const {return iBASE::operator[]((size_type)(i)-1);}

  // set/get:  0-based access
  inline       int& operator[](int i)       {return iBASE::operator[]((size_type)i);}
  inline const int& operator[](int i) const {return iBASE::operator[]((size_type)i);}

  int   size() const        { return (int) (iBASE::size()); }
  void  fill(int i)         { assign(iBASE::size(), i); }
  IMap& operator=(int i)    { fill(i); return (*this); }

  IMap& copy(int len, const int *p)
  {
    resize(len);
    if (len>0) {
      assert(p);
      int Nmod4=len&3; int N4=len-Nmod4; 
      int i=0; IMap& v=(*this);
      for (i=0; i<N4; i+=4)  {v[i]=p[i]; v[i+1]=p[i+1]; v[i+2]=p[i+2]; v[i+3]=p[i+3];}
      for (i=N4; i<len; ++i) {v[i]=p[i];}
    }
    return (*this);
  }


  // Add a value to all elements.
  void add_val(const int &x) {
    if (0==x) { return; }
    int len=(int)size(); int Nmod4=len&3; int N4=len-Nmod4; 
    int i=0; IMap& v=(*this);
    for (i=0; i<N4; i+=4)  { v[i]+=x; v[i+1]+=x; v[i+2]+=x; v[i+3]+=x; }
    for (i=N4; i<len; ++i) { v[i]+=x; }
  }

  IMap& operator += (const int &x) { add_val( x); return (*this); }
  IMap& operator -= (const int &x) { add_val(-x); return (*this); }

  void operator += (const IMap &B) {
    assert(this->size() <= B.size());
    iBASE::iterator i; iBASE::const_iterator i2;
    for (i=begin(), i2=B.begin(); i!=end(); ++i, ++i2) {
      (*i) += (*i2);
    }
  }

  void operator -= (const IMap &B) {
    assert(this->size() <= B.size());
    iBASE::iterator i; iBASE::const_iterator i2;
    for (i=begin(), i2=B.begin(); i!=end(); ++i, ++i2) {
      (*i) -= (*i2);
    }
  }


  // Partition segment into equal intervals
  // If (stop<start), sequence decreases.
  // Resizes array if necessary
  void linspace(int start, int stop, int len=0)
  {
    assert(len>=0);

    if (0 == len) {
      // keep current size
    } else {
      resize((size_type)len); 
    }

    if (stop != start) {
      IMap& v=(*this); int N=(int)size();
      int delta=(stop-start) / (N-1);
      v(1) = start;                 // Set exact start value
      for (int i=2; i<N; ++i) {
        v(i) = v(i-1)+delta;        // delta may be negative
      }
      v(N) = stop;                  // Impose exact stop value
    } 
    else {
      fill(start);  // start and stop are the same
    }
  }


  // Set contents to be integer range from [start:stop].  
  // If (stop<start), range decreases.
  void range(int start, int stop) 
  {
    int len = 0;
    if (start<=stop)
         len = (stop-start)+1;
    else len = (start-stop)+1;

    linspace(start, stop, len);
  }

#if (0)
  // concatenate sets of IMap objects
  IMap& concat(const IMap &A, const IMap &B);
  IMap& concat(const IMap &A, const IMap &B, const IMap &C);
  IMap& concat(const IMap &A, const IMap &B, const IMap &C, const IMap &D);
#endif

  void print (
    FILE* os = stdout, 
    const char* msg=NULL,
    const char* fmt="lf", // [%d|%e|%lf|%g]
    int  prec =2,       // sig.figs|dec.places
    int  wdth =6,       // output spacing [12]
    bool vert =false,   // vertical/horizontal
    int  nline=5,       // entries per line
    int  nv   =0        // num vals to write (0 --> all)
  ) const;

};


//---------------------------------------------------------
// Global operators and routines
//---------------------------------------------------------

// C = A + B
inline IMap operator+(const IMap &A, const IMap &B) 
{
  IMap tmp(A); tmp += B; return tmp;
}


// C = A - B
inline IMap operator-(const IMap &A, const IMap &B) 
{
  IMap tmp(A); tmp -= B; return tmp;
}


//---------------------------------------------------------
inline void IMap::print
(
  FILE* os, 
  const char* msg,
  const char* fmt,  // [%d|%e|%lf|%g]
  int  prec,        // sig.figs|dec.places
  int  wdth,        // output spacing [12]
  bool vert,        // vertical/horizontal
  int  nline,       // entries per line
  int  nv           // num vals to write (0 --> all)
) const
//---------------------------------------------------------
{
  static char buf[20] = {""};

  // write min(nv,len) vals
  int len = this->size();
  if (nv > 0) len = (nv<=len ? nv : len);

  if (msg) { fprintf(os, "%s\n", msg); }
  fprintf(os, "(%d)\n", len);

  const IMap& v=(*this);

  if (1==nline || vert) {
    sprintf(buf, "%c%dd\n", '%',wdth);
    for (int i=0; i<len; ++i) {
      fprintf(os, buf, v[i]);
    }
  } else {
    sprintf(buf, "%c%dd ", '%',wdth);
    for (int i=0; i<len; ++i) {
      fprintf(os, buf, v[i]);
      if (i && (0 == ((i+1)%nline)))
        fprintf(os, "\n");
    }
  }
  fprintf(os, "\n");
  fflush(os);
}

#endif  // NDG__MappedRegion1d_H__INCLUDED
