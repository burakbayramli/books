#ifndef CONVERT_H
#define CONVERT_H
#include <Python.h>
#include <pyport.h>
#include <MyArray.h>

typedef double (*Fxy)(double x, double y);

class Convert_MyArray
{
 public:
  Convert_MyArray();
 ~Convert_MyArray();

  // borrow data:
  PyObject*        my2py (MyArray<double>& a);
  MyArray<double>* py2my (PyObject* a);

  // copy data:
  PyObject*        my2py_copy (MyArray<double>& a);
  MyArray<double>* py2my_copy (PyObject* a);

  // npy_intp to/from int array for array size:
  npy_intp         npy_size[MAXDIM];
  int              int_size[MAXDIM];
  void             set_npy_size(int*      dims, int nd);
  void             set_int_size(npy_intp* dims, int nd);

  // print array:
  void             dump(MyArray<double>& a);

  // convert Py function to C/C++ function calling Py:
  Fxy              set_pyfunc (PyObject* f);
 protected:
  static PyObject* _pyfunc_ptr;  // used in _pycall
  static double    _pycall (double x, double y);
};
#endif
