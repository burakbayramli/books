#ifndef MYARRAY_CPP
#define MYARRAY_CPP

#include <memory>
#include "MyArray.h"



template< typename T >
MyArray<T>:: MyArray(const MyArray<T>& a)
{
  *this = a;
}
template< typename T >
MyArray<T>:: MyArray(T* a, int ndim_, int size_[])
{
  A = a; 
  ndim = ndim_; 
  length = 1;
  for (int i = 0; i < ndim; i++) {
    size[i] = size_[i];  // copy size
    length *= size[i];
  }
}

template< typename T >
bool MyArray<T>:: redim(int n1)
{
  if (size[0] == n1)  
    return false;
  deallocate();
  A = allocate(n1);
  return true;
}


template< typename T >
bool MyArray<T>:: redim(int n1, int n2)
{
  if (size[0] == n1 && size[1] == n2)  
    return false;
  if (n1*n2 == length) {  // same length
    size[0] = n1;
    size[1] = n2;
    return false;
  } else {
    deallocate();
    A = allocate(n1, n2);
    return true;
  }
}

template< typename T >
bool MyArray<T>:: redim(int n1, int n2, int n3)
{
  if (size[0] == n1 && size[1] == n2 && size[2] == n3)  
    return false;
  if (n1*n2*n3 == length) {  // same length
    size[0] = n1;
    size[1] = n2;
    size[2] = n3;
    return false;
  } else {
    deallocate();
    A = allocate(n1, n2, n3);
    return true;
  }
}

template< typename T >
T* MyArray<T>:: allocate(int n1)
{
  T* ptr = 0;
  size[0] = n1;
  ndim = 1;
  length = n1;
    
  if (n1 < 0) {
    std::cerr << "MyArray::allocate -- illegal length " << n1
	      << std::endl;
    exit(1);
  }
  else if (n1 == 0) {
    ptr = 0;
    return ptr;
  }
    
  try {
    ptr = new T[n1];
  } 
  catch (std::bad_alloc&) {
    std::cerr << "MyArray::allocate -- unable to allocate array of length "
	      << n1 << std::endl;
    exit(1);
  }
  for (int i = 0; i < n1; i++) { ptr[i] = 0; }
  return ptr;
}

template< typename T >
T* MyArray<T>:: allocate(int n1, int n2)
{
  T* ptr = allocate(n1*n2);
  size[0] = n1;
  size[1] = n2;
  length = n1*n2;
  ndim = 2;
  return ptr;
}

template< typename T >
T* MyArray<T>:: allocate(int n1, int n2, int n3)
{
  T* ptr = allocate(n1*n2*n3);
  size[0] = n1;
  size[1] = n2;
  size[2] = n3;
  length = n1*n2*n3;
  ndim = 3;
  return ptr;
}


template< typename T >
void MyArray<T>:: deallocate()
{
  delete [] A;
  for (int i = 0; i < MAXDIM; i++) { size[i] = 0; }
  length = 0;
}

template< typename T >
bool MyArray<T>:: indexOk(int i) const
{
  if (i < 0 || i >= size[0]) {
    std::cerr << "vector index check; index i=" << i 
	      << " out of bounds 0:" << size[0]-1
	      << std::endl;
    return false;
  }
  else
    return true;  // valid index!
}


template< typename T >
bool MyArray<T>:: indexOk(int i, int j) const
{
  if (!indexOk(i)) {
    return false;
  }
  else if (j < 0 || j >= size[1]) {
    std::cerr << "vector index check; index j=" << j
	      << " out of bounds 0:" << size[1]-1 << std::endl;
    return false;
  }
  else
    return true;
}


template< typename T >
bool MyArray<T>:: indexOk(int i, int j, int k) const
{
  if (!indexOk(i, j)) {
    return false;
  }
  else if (k < 0 || k >= size[2]) {
    std::cerr << "vector index check; index k=" << k
	      << " out of bounds 0:" << size[2]-1 << std::endl;
    return false;
  }
  else
    return true;
}

template< typename T >
MyArray<T>& MyArray<T>::operator=(const MyArray<T>& a)
{
  if (length != a.length) { 
    A = allocate(a.length);
    length = a.length;
  }
  ndim = a.ndim;
  int i;
  for (i = 0; i < ndim; i++) { size[i] = a.size[i]; }
  for (i = 0; i < length; i++) { A[i] = a.A[i]; }
  return *this;
} 

template< typename T >
void MyArray<T>:: print_(std::ostream& os)
{
  // very (too!) simple
  for (int i = 0; i < length; i++) {
    os << i << ":" << A[i] << "\n";
  }
}

template< typename T >
void MyArray<T>:: dump(std::ostream& os)
{
  os << "\nno of dimensions: " << ndim
     << "\nsizes: ";
  int i;
  for (i = 0; i < ndim; i++)
    os << size[i] << " ";
  os << "length: " << length << " ptr: " << A << std::endl;
  print_(os);
}



template<typename T> 
std::ostream& operator<<(std::ostream& os, MyArray<T>& arr)
{
  arr.print_(os);
  return os;
}

#endif
