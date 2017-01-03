#ifndef MyArray_H
#define MyArray_H
#include <iostream>

#define MAXDIM 3

template< typename T > class MyArray
{
 public:
  T* A;                   // the data
  int ndim;               // no of dimensions (axis)
  int size[MAXDIM];       // size/length of each dimension
  int length;             // total no of array entries
  T* allocate(int n1);
  T* allocate(int n1, int n2);
  T* allocate(int n1, int n2, int n3);
  void deallocate();
  bool indexOk(int i) const;      
  bool indexOk(int i, int j) const;
  bool indexOk(int i, int j, int k) const;
  
 public:
  MyArray() { A = NULL; length = 0; ndim = 0; }
  MyArray(int n1) { A = allocate(n1); }
  MyArray(int n1, int n2) { A = allocate(n1, n2); }
  MyArray(int n1, int n2, int n3) { A = allocate(n1, n2, n3); }
  MyArray(T* a, int ndim_, int size_[]);
  MyArray(const MyArray<T>& array);
  ~MyArray() { deallocate(); }

  bool redim(int n1);           
  bool redim(int n1, int n2);   
  bool redim(int n1, int n2, int n3);   
  
  // return the size of the arrays dimensions:
  int shape(int dim) const { return size[dim-1]; }
  
  // indexing:
  const T& operator()(int i) const;
  T& operator()(int i);
  const T& operator()(int i, int j) const;
  T& operator()(int i, int j);
  const T& operator()(int i, int j, int k) const;
  T& operator()(int i, int j, int k);
  
  MyArray<T>& operator= (const MyArray<T>& v);
  
  
  // return pointers to the data:
  const T* getPtr() const { return A;}   
  T* getPtr() { return A; }
  
  void print_(std::ostream& os);
  void dump(std::ostream& os);  // dump all
};


// bodies of inline functions:

template< typename T >
inline T& MyArray<T>:: operator()(int i)
{
#ifdef SAFETY_CHECKS
  indexOk(i);
#endif
  return A[i];
}


template< typename T >
inline const T& MyArray<T>:: operator()(int i) const
{
#ifdef SAFETY_CHECKS
  indexOk(i);
#endif
  return A[i];
}


template< typename T >
inline T& MyArray<T>:: operator()(int i, int j)
{
#ifdef SAFETY_CHECKS
  indexOk(i, j);
#endif
  return A[i*size[1] + j];
}


template< typename T >
inline const T& MyArray<T>:: operator()(int i, int j) const
{
#ifdef SAFETY_CHECKS
  indexOk(i, j);
#endif
  return A[i*size[1] + j];
}

// should precompute size[1]*size[2]...

template< typename T >
inline T& MyArray<T>:: operator()(int i, int j, int k)
{
#ifdef SAFETY_CHECKS
  indexOk(i, j, k);
#endif
  return A[i*size[1]*size[2] + j*size[2] + k];
}

template< typename T >
inline const T& MyArray<T>:: operator()(int i, int j, int k) const
{
#ifdef SAFETY_CHECKS
  indexOk(i, j, k);
#endif
  return A[i*size[1]*size[2] + j*size[2] + k];
}

// since this is a template class, we must make the body of the
// functions available in this header file:
#include <MyArray.cpp>

#endif
