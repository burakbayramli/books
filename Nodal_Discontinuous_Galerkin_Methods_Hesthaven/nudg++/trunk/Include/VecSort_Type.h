// VecSort_Type.h
// Vector<T> plus sort functions. T must be "sortable"
// 2007/07/31
//---------------------------------------------------------
#ifndef NDG__VecSort_Type_H__INCLUDED
#define NDG__VecSort_Type_H__INCLUDED

#include "Vec_Type.h"


// typedef versions for common data types
template <typename T> class VecSort;

typedef VecSort<int>    IVecSort;
typedef VecSort<double> DVecSort;


//---------------------------------------------------------
template <typename T> 
class VecSort : public virtual Vector<T>
//---------------------------------------------------------
{
public:
  VecSort() : Vector<T>() {}
  VecSort(int N) : Vector<T>(N) {}
  VecSort(const VecSort<T>& X) : Vector<T>(X) {}
  VecSort(const Vector<T>& X)  : Vector<T>(X) {}
 ~VecSort() {}

  VecSort<T>& operator=(const T& value)      { fill(value); return (*this); }
  VecSort<T>& operator=(const VecSort<T>& X) { Vector<T>::operator = (X); return *this; }

  // "operator==" - true if equality holds for all components.
  // "operator!=" - equals !"operator==".
  // "operator<"  - true if inequality holds for all components.
  // "operator>"  - true if inequality holds for all components.
  // "operator<=" - true if inequality holds for all components.
  // "operator>=" - true if inequality holds for all components.

  bool operator == (const VecSort<T>& X);
  bool operator != (const VecSort<T>& X)  { return !(*this == X); }

  bool operator <  (const VecSort<T>& X);
  bool operator >  (const VecSort<T>& X);
  bool operator <= (const VecSort<T>& X);
  bool operator >= (const VecSort<T>& X);


  // minValue(), maxValue() computes the min/max value of the 
  // entries in the vector. Overloaded version uses int arg 
  // to return index where the min/max value was found.
  T maxValue() const;
  T minValue() const;
  T maxValue(int& i) const;  // max occured in position i
  T minValue(int& i) const;  // min occured in position i


  // select() computes the kth smallest value in the vector. 
  // An overloaded version has an integer arg which sets the 
  // length (N) of the sub-vector on which the computation 
  // is performed.  The input array will be rearranged to 
  // have the computed value in location (k), with smaller 
  // elements moved to (1:k-1) (in arbitrary order, and all 
  // larger elements in (k+1:N) (also in arbitrary order).
  T select(int k);           // get the k smallest element
  T select(int k, int N);    // only use the N first positions


  // heapsort() sorts the vector using the heapsort algorithm. 
  // An overloaded version allows for only the entries in 
  // positions (1:N) to be sorted.
  void heapsort();
  void heapsort(int N);      // only sort the N first positions


  // mixedheapsort() sorts the vector using a mixture of three 
  // methods. Usually faster than "heapsort" for small vectors.
  void mixedheapsort();
  void mixedheapsort(int N); // only sort the N first positions


  // quicksort() sorts the vector using the quicksort.
  // Usually the fastest algorithm for large vectors.
  void quicksort();
  void quicksort(int N);     // only sort the N first positions


  // makeIndex() instead of changing the order of the array 
  // entries, an index vector is made which maps the original 
  // array entries onto a sorted array. When sorting several 
  // arrays simultaneously, this index vector can be used as 
  // input to sortFromIndexVec()
  void makeIndex(IVec& index) const;


  // sortFromIndexVec() changes the entry order in the object 
  // according to an index vector (e.g. computed by makeIndex()).
  void sortFromIndexVec(const IVec& index);
};



template <typename T>
bool VecSort<T>::operator == (const VecSort<T>& X)
{
  if (this->m_Len != X.m_Len)
    return false;
  else
  {
    int N = this->size();
    for (int i = 0; i < N; ++i)
      if (this->v_[i] != X.v_[i])
        return false;
  }
  return true;
}


template <typename T>
T VecSort<T>::maxValue() const
{
  T tmax = this->v_[0];
  const T* astop = this->end();
  for (const T* ap = this->begin(); ap < astop; ap++)
    if (*ap > tmax) tmax = *ap;

  return tmax;
}


template <typename T>
T VecSort<T>::minValue() const
{
  T tmin = this->v_[0];
  const T* astop = this->end();
  for (const T* ap = this->begin(); ap < astop; ap++)
    if (*ap < tmin) tmin = *ap;

  return tmin;
}


template <typename T>
T VecSort<T>::maxValue(int& i) const
{
  //
  // returns 1-based index
  //
  T tmax = this->v_[0];
  i = 1;
  int j = 1;
  
  const T* astop = this->end();
  for (const T* ap = this->begin(); ap < astop; ap++)
  {
    if (*ap > tmax) 
    {
      tmax = *ap;
      i = j;
    }
    j++;
  }
  return tmax;
}


template <typename T>
T VecSort<T>::minValue(int& i) const
{
  //
  // returns 1-based index
  //
  T tmin = this->v_[0];
  i = 1;
  int j = 1;
  
  const T* astop = this->end();
  for (const T* ap = this->begin(); ap < astop; ap++)
  {
    if (*ap < tmin)
    {
      tmin = *ap;
      i = j;
    }
    j++;
  }
  return tmin;
}


template <typename T>
T VecSort<T>::select(int k)
{
  // Select the kth smallest of this vector, 
  // rearrangement will occur unless the 
  // min or max value alternatives are used.
  if ( 1 == k ) 
    return minValue();
  else if ( this->size() == k ) 
    return maxValue();
  else 
    return select(k, this->size());
}


template <typename T>
T VecSort<T>::select(int k, int N)
{
  // Returns the kth smallest value in the range (1:N). 
  // The input array will be rearranged to have this value 
  // in location (k), with all smaller elements moved 
  // to (1:k-1) (in arbitrary order, and all larger 
  // elements in (k+1:N) (also in arbitrary order).
  int i,ir=N,j,l=1,midp;
  T v,temp;

  VecSort<T>& a = *this;
  
  for (;;) 
  {
    if (ir <= l+1) 
    {
      if (ir == l+1 && a(ir) < a(l)) {temp=a(l); a(l)=a(ir); a(ir)=temp;}
      return a(k);
    } 
    else 
    {
      midp=(l+ir)>>1;
      i=l+1;
      temp=a(midp); a(midp)=a(i); a(i)=temp;
      if (a(l) > a(ir)) {temp=a(l); a(l)=a(ir); a(ir)=temp;}
      if (a(i) > a(ir)) {temp=a(i); a(i)=a(ir); a(ir)=temp;}
      if (a(l) > a(i )) {temp=a(l); a(l)=a(i ); a(i )=temp;}
      j=ir;
      v=a(i);
      for (;;) {
        do i++; while (a(i) < v);
        do j--; while (a(j) > v);
        if (j < i) break;
        temp=a(i); a(i)=a(j); a(j)=temp;
      }
      a(l+1)=a(j);
      a(j)=v;
      if (j >= k) ir=j-1;
      if (j <= k) l=i;
    }
  }
}


template <typename T>
void VecSort<T>::heapsort()
{
  heapsort(this->size());
}


template <typename T>
void VecSort<T>::heapsort(int N)
{
  // modified sort.c (from Numerical Recipes)
  
  if (N<2) 
    return;
  
  int i,j;
  T v;
  
  VecSort<T>& a = *this;
  
  int l,ir;
  l=(N>>1);

  while (l>0) {
    v=a(l);
    i=l--;
    j=i<<1;
    while (j<N) {
      if (a(j)<a(j+1)) j++;
      if (v<a(j)) {
        a(i)=a(j);
        i=j; j <<= 1;
      }
      else  break;
    }
    if (j==N && v<a(j)) {
      a(i)=a(j);
      a(j)=v;
    } else
      a(i)=v;
  }
  v=a(N);
  a(N)=a(1);
  ir=N-1;
  while (ir>1) {
    i=1;j=2;
    while (j<ir) {
      if (a(j)<a(j+1)) j++;
      if (v<a(j)) {
        a(i)=a(j);
        i=j; j <<= 1;
      }
      else  break;
    }
    if (j==ir && v<a(j)) {
      a(i)=a(j);
      a(j)=v;
    } else
      a(i)=v;
    v=a(ir);
    a(ir--)=a(1);
  }
  a(1)=v;
}


template <typename T>
void VecSort<T>::mixedheapsort()
{ 
  mixedheapsort(this->m_Len);
}


template <typename T>
void VecSort<T>::mixedheapsort(int N)
{
  // Sort the first N items of the T vector.  Selects 
  // a sort method appropriate for length of vector.

  if (this->size() < N) {
    umERROR("VecSort<T>::mixedheapsort()", 
            "Requested sort of %d elements in array with length %d", N, this->m_Len);
  }

  int i,j;
  T v;
  VecSort<T>& a = *this;
  
  if (N < 14)
  {  
    //
    // straight insertion
    //
    for (j=2;j<=N;j++) {
      v=a(j); i=j;
      while ( a(i-1) > v ) { a(i)=a(i-1); i--; if (i==1) break; }
      a(i)=v;
    }
    return;
  }
  else if ( N <= 64 ) 
  {
    //
    // Shell's method
    //
    int inc;
    inc=13;
    do {
      inc *= 3;
      inc++;
    } while ( inc <= N );
    do {
      inc/=3;
      for (j=inc+1;j<=N;j++) {
        v=a(j); i=j;
        while (a(i-inc)>v) { a(i)=a(i-inc); i-=inc; if (i<=inc) break; }
        a(i)=v;
      }
    } while ( inc > 4 );
    for (j=2;j<=N;j++) {
      v=a(j); i=j;
      while ( a(i-1) > v ) { a(i)=a(i-1); i--; if (i==1) break; }
      a(i)=v;
    }
    return;
  }
  else 
  {  
    //
    // heap sort
    //
    int l,ir;
    l=(N>>1);
    while (l>0) {
      v=a(l);
      i=l--;
      j=i<<1;
      while (j<N) {
        if (a(j)<a(j+1)) j++;
        if (v<a(j)) {
          a(i)=a(j);
          i=j; j <<= 1;
        }
        else break;
      }
      if (j==N && v<a(j)) {
        a(i)=a(j);
        a(j)=v;
      }else
        a(i)=v;
    }
    v=a(N);
    a(N)=a(1);
    ir=N-1;
    while (ir>1) {
      i=1;j=2;
      while (j<ir) {
        if (a(j)<a(j+1)) j++;
        if (v<a(j)) {
          a(i)=a(j);
          i=j; j <<= 1;
        }
        else  break;
      }
      if (j==ir && v<a(j)) {
        a(i)=a(j);
        a(j)=v;
      }else
        a(i)=v;
      v=a(ir);
      a(ir--)=a(1);
    }
    a(1)=v;
  }
}


template <typename T>
void VecSort<T>::quicksort()
{
  quicksort(this->m_Len);
}


template <typename T>
void VecSort<T>::quicksort(int N)
{ 
  if ( N < 2 ) 
    return;

  if (this->size() < N) {
    umERROR("VecSort<T>::quicksort()", 
            "Requested sort(%d) in array length %d.", N, this->m_Len);
  }


  //
  // sort the (sub)vector a(1:N)
  //
  int i,ir=N,j,midp,l=1;
  int jstack=0;
  
  VecSort<T>& a = *this;  
  
  T v,temp;
  int istack[50]={0};
  
  for (;;) 
  {
    if (ir-l<8) 
    {
      for (j=l+1;j<=ir;j++) 
      {
        v=a(j); i=j;
        while ( a(i-1) > v ) { a(i)=a(i-1); i--; if (i==l) break; }
        a(i)=v;
      }
      if (jstack == 0) break;
      ir=istack[jstack--];
      l=istack[jstack--];
    } 
    else 
    {
      midp=(l+ir)>>1;
      i=l+1;
      temp=a(midp);a(midp)=a(i);a(i)=temp;
      if (a(l)>a(ir)) {
        temp=a(l);a(l)=a(ir);a(ir)=temp;
      }
      if (a(i)>a(ir)) {
        temp=a(i);a(i)=a(ir);a(ir)=temp;
      }
      if (a(l)>a(i)) {
        temp=a(l);a(l)=a(i);a(i)=temp;
      }
      j=ir;
      v=a(i);
      for (;;) {
        do i++; while (a(i) < v);
        do j--; while (a(j) > v);
        if (j<i) break;
        temp=a(i);a(i)=a(j);a(j)=temp;
      }
      a(l+1)=a(j);
      a(j)=v;
      if (jstack >= 48) 
        umERROR("VecSort:: quicksort","error NSTACK too small");
      if (ir-i+1 >= j-l) {
        istack[++jstack]=i;
        istack[++jstack]=ir;
        ir=j-1;
      } else {
        istack[++jstack]=l;
        istack[++jstack]=j-1;
        l=i;
      }
    }
  }
}


template <typename T>
void VecSort<T>::makeIndex(IVec& index) const
{
  int l=0,j=0,ir=0,indxt=0,i=0;
  T q=T(0);
  int N = this->m_Len;
  
  const VecSort<T>& V = *this;
  
  index.resize(this->size());
  
  for (j=1; j <= N; ++j)
    index(j)=j;
  l=(N >> 1) + 1;
  ir=N;
  for (;;)
  {
    if (l > 1)
      q=V((indxt=index(--l)));
    else
    {
      q=V((indxt=index(ir)));
      index(ir)=index(1);
      if (--ir == 1)
      {
        index(1)=indxt;
        return;
      }
    }
    i=l;
    j=l << 1;
    while (j <= ir)
    {
      if (j < ir && V(index(j)) < V(index(j+1)))
        j++;
      if (q < V(index(j)))
      {
        index(i)=index(j);
        j += (i=j);
      }
      else j=ir+1;
    }
    index(i)=indxt;
  }
}


template <typename T>
void VecSort<T>::sortFromIndexVec(const IVec& index)
{
  VecSort<T> tmp = (*this);
  int N = this->size();
  for (int i=1; i<=N; ++i)
    (*this)(i) = tmp(index(i));
}


template <typename T>
bool VecSort<T>::operator < (const VecSort<T>& X)
{
  if (X.size() == this->m_Len)
  {
    T* ap = this->begin();
    T* xp = X.begin();
    T* astop = this->end();

    for ( ; ap < astop; ap++, xp++)
      if (*ap >= *xp)
        return false;

    return true;
  }
  else
    return false;
}


template <typename T>
bool VecSort<T>::operator >  (const VecSort<T>& X)
{
  if (X.size() == this->m_Len)
  {
    T* ap = this->begin();
    T* xp = X.begin();
    T* astop = this->end();

    for ( ; ap < astop; ap++, xp++)
      if (*ap <= *xp)
        return false;

    return true;
  }
  else
    return false;
}


template <typename T>
bool VecSort<T>::operator <= (const VecSort<T>& X)
{
  if (X.size() == this->m_Len)
  {
    T* ap = this->begin();
    T* xp = X.begin();
    T* astop = this->end();

    for ( ; ap < astop; ap++, xp++)
      if (*ap > *xp)
        return false;

    return true;
  }
  else
    return false;
}


template <typename T>
bool VecSort<T>::operator >= (const VecSort<T>& X)
{
  if (X.size() == this->m_Len)
  {
    T* ap = this->begin();
    T* xp = X.begin();
    T* astop = this->end();

    for ( ; ap < astop; ap++, xp++)
      if (*ap < *xp)
        return false;

    return true;
  }
  else
    return false;
}



///////////////////////////////////////////////////////////
//
// Matlab "sort_unique" operation
//
///////////////////////////////////////////////////////////

#include <set>

template <typename T> inline 
Vector<T>& sort(const Vector<T> &A, bool unique, bool ascending=true)
{
  Vector<T> *tmp = new Vector<T>("vec", OBJ_temp);

  int N = A.size();
  if (N<2) {
    (*tmp) = A;       // vector of length {0/1}
  }
  else if (!unique) 
  {
    if (ascending) {
      VecSort<T> s(A);
      s.mixedheapsort();  // sort entire vector, retaining duplicates
      (*tmp) = s;
    } 
    else {
      // sort in descending order (specify binary predicate)
      (*tmp) = A;
      std::sort(tmp->begin(), tmp->end(), std::greater<T>());
    }
  } 
  else 
  {
    typedef typename std::set<T>::iterator SetIt;

    // std::set sorts and removes duplicates
    std::set<T> us;
    for (int i=1; i<=N; ++i) { us.insert(A(i)); }
    int len = (int) us.size(), sk=0;;
    tmp->resize(len);  SetIt it;
    for (it=us.begin(), sk=1; it!=us.end(); it++, sk++) {
      (*tmp)(sk) = (*it);
    }
    if (A.get_mode() == OBJ_temp) { delete (&A); }
  }

  return (*tmp);
}



///////////////////////////////////////////////////////////
//
// Matlab "[S,IX] sort(v, dir)" operation
//
///////////////////////////////////////////////////////////


template <typename T> inline 
void sort(const Vector<T> &A, Vector<T> &S, IVec& IX, eDir dir=eAscend)
{
  int N = A.size(); S.resize(N); IX.resize(N);
  if (N<2) {
    S = A;
    if (1==N) {
      IX(1)=1; 
    }
  }
  else 
  {
    VecSort<T> tmp(A);
    if (eAscend==dir) {
      tmp.makeIndex(IX);
      for (int i=1; i<=N; ++i) {
        S(i) = tmp(IX(i));
      }
    } 
    else {
      IVec ix; tmp.makeIndex(ix);
      for (int i=1,j=N; i<=N; ++i, --j) {
        S(i) = tmp(ix(j));  // sort the vector
        IX(i)=     ix(j);   // reverse indices
      }
    }
  } 
}


#endif  // NDG__VecSort_Type_H__INCLUDED
