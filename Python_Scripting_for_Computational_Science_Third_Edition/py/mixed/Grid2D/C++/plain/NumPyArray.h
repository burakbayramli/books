#ifndef NumPyArray_INCLUDED
#define NumPyArray_INCLUDED

#include <Python.h>              /* Python as seen from C */
#include <numpy/arrayobject.h>   /* NumPy  as seen from C */
#include <iostream>

class NumPyArray_Float
{
 private:
  PyArrayObject* a;

 public:
  NumPyArray_Float () { a=NULL; }
  NumPyArray_Float (int n)                  { create(n); }
  NumPyArray_Float (int n1, int n2)         { create(n1, n2); }
  NumPyArray_Float (int n1, int n2, int n3) { create(n1, n2, n3); }
  NumPyArray_Float (double* data, int n) 
    { wrap(data, n); }
  NumPyArray_Float (double* data, int n1, int n2) 
    { wrap(data, n1, n2); }
  NumPyArray_Float (double* data, int n1, int n2, int n3) 
    { wrap(data, n1, n2, n3); }
  NumPyArray_Float (PyArrayObject* array) { a = array; }
  // NOTE: if we here call wrap(a), a seg.fault appears!
  //NumPyArray_Float (PyArrayObject* array) { wrap(a); }

  // no destructor - created arrays must be destroyed manually...

  // the create functions allocates a new array of doubles
  // with prescribed size
  int create (int n);
  int create (int n1, int n2);
  int create (int n1, int n2, int n3);

  // the wrap functions takes an existing array, pointed to by
  // a single double* pointer, and wraps it in a NumPy array

  void wrap (double* data, int n) { 
    npy_intp dim1[1]; dim1[0] = n; 
    a = (PyArrayObject*) PyArray_SimpleNewFromData(
        1, dim1, NPY_DOUBLE, (void *) data);
  }

  void wrap (double* data, int n1, int n2) { 
    npy_intp dim2[2]; dim2[0] = n1; dim2[1] = n2;
    a = (PyArrayObject*) PyArray_SimpleNewFromData(
        2, dim2, NPY_DOUBLE, (void *) data);
  }

  void wrap (double* data, int n1, int n2, int n3) { 
    npy_intp dim3[3]; dim3[0] = n1; dim3[1] = n2; dim3[2] = n3;
    a = (PyArrayObject*) PyArray_SimpleNewFromData(
        3, dim3, NPY_DOUBLE, (void *) data);
  }

  // this wrap function takes a C representation of a NumPy
  // array and wraps in the present C++ class:
  void wrap (PyArrayObject* array) { a = array; }

  int checktype () const;  // are the entries of type double?
  int checkdim  (int expected_ndim) const;
  int checksize (int expected_size1, int expected_size2=0, 
		 int expected_size3=0) const;

  double  operator() (int i) const {
#ifdef INDEX_CHECK
    assert(PyArray_NDIM(a) == 1 && i >= 0 && i < PyArray_DIM(a,0));
#endif
    return *((double*) PyArray_GETPTR1(a,i));
  }
  double& operator() (int i) {
    return *((double*) PyArray_GETPTR1(a,i));
  }

  double  operator() (int i, int j) const {
    return *((double*) PyArray_GETPTR2(a,i,j));
  }
  double& operator() (int i, int j) {
    return *((double*) PyArray_GETPTR2(a,i,j));
  }

  double  operator() (int i, int j, int k) const {
    return *((double*) PyArray_GETPTR3(a,i,j,k));
  }
  double& operator() (int i, int j, int k) {
    return *((double*) PyArray_GETPTR3(a,i,j,k));
  }

  int dim() const { return a->nd; }  // no of dimensions
  int size1() const { return PyArray_DIM(a,0); }
  int size2() const { return PyArray_DIM(a,1); }
  int size3() const { return PyArray_DIM(a,2); }
  double* getData () { return (double*) a->data; }  // most useful in 1D
  PyArrayObject* getPtr () { return a; }
};

void dump (std::ostream& o, const NumPyArray_Float& a);

#endif
