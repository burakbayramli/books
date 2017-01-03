#include <Python.h>
#include <numpy/arrayobject.h>
#include <convert.h>

Convert_MyArray:: Convert_MyArray()
{
  import_array();
}

Convert_MyArray:: ~Convert_MyArray()
{
  if (_pyfunc_ptr != NULL)
    Py_DECREF(_pyfunc_ptr);
}

void Convert_MyArray:: set_npy_size(int* dims, int nd)
{
  for (int k=1; k<nd; k++)
    npy_size[k] = (npy_intp) dims[k];
}

void Convert_MyArray:: set_int_size(npy_intp* dims, int nd)
{
  for (int k=1; k<nd; k++)
    int_size[k] = (int) dims[k];
}


PyObject* Convert_MyArray:: my2py(MyArray<double>& a)
{
  //a.dump(std::cout);
  set_npy_size(a.size, a.ndim);
  PyArrayObject* array =  (PyArrayObject*) \
          PyArray_SimpleNewFromData(a.ndim, npy_size, NPY_DOUBLE,
				    (void*) a.A); 
  if (array == NULL) {
    return NULL; /* PyArray_SimpleNewFromData raised an exception */ 
  }
  return PyArray_Return(array);
}

PyObject* Convert_MyArray:: my2py_copy(MyArray<double>& a)
{
  //a.dump(std::cout);
  set_npy_size(a.size, a.ndim);
  PyArrayObject* array =  (PyArrayObject*) \
          PyArray_SimpleNew(a.ndim, npy_size, NPY_DOUBLE); 
  if (array == NULL) {
    return NULL; /* PyArray_SimpleNew raised an exception */ 
  }
  double* ad = (double*) PyArray_DATA(array);
  for (int i = 0; i < a.length; i++) {
    ad[i] = a.A[i];
  }
  return PyArray_Return(array);
}

MyArray<double>* Convert_MyArray:: py2my(PyObject* a_)
{
  PyArrayObject* a = (PyArrayObject*) 
    PyArray_FROM_OTF(a_, NPY_DOUBLE, NPY_IN_ARRAY);
  if (a == NULL) { return NULL; }
  /*
  PyArrayObject* a = (PyArrayObject*) a_;
  if (PyArray_TYPE(a) != NPY_DOUBLE) {
    PyErr_SetString(PyExc_TypeError, "Not an array of double");
    return NULL;
  }
  */
  // borrow the data, but wrap it in MyArray:
  set_int_size(PyArray_DIMS(a), PyArray_NDIM(a));
  MyArray<double>* ma = new MyArray<double> \
      ((double*) PyArray_DATA(a), PyArray_NDIM(a), int_size);
  return ma;
}

MyArray<double>* Convert_MyArray:: py2my_copy(PyObject* a_)
{
  PyArrayObject* a = (PyArrayObject*) 
    PyArray_FROM_OTF(a_, NPY_DOUBLE, NPY_IN_ARRAY);
  if (a == NULL) { return NULL; }
  /*
  PyArrayObject* a = (PyArrayObject*) a_;
  if (PyArray_TYPE(a) != NPY_DOUBLE) {
    PyErr_SetString(PyExc_TypeError, "Not an array of double");
    return NULL;
  }
  */
  MyArray<double>* ma = new MyArray<double>();
  if (PyArray_NDIM(a) == 1) {
    ma->redim(PyArray_DIM(a,0));
  } else if (PyArray_NDIM(a) == 2) {
    ma->redim(PyArray_DIM(a,0), PyArray_DIM(a,1));
  } else if (PyArray_NDIM(a) == 3) {
    ma->redim(PyArray_DIM(a,0), PyArray_DIM(a,1), PyArray_DIM(a,2));
  }
  // copy data:
  double* ad = (double*) PyArray_DATA(a);
  double* mad = ma->A;
  for (int i = 0; i < ma->length; i++) {
    mad[i] = ad[i];
  }
  return ma;
}

double Convert_MyArray:: _pycall (double x, double y)
{
  PyObject* arglist = Py_BuildValue("(dd)", x, y);
  PyObject* result =  PyEval_CallObject(\
                      Convert_MyArray::_pyfunc_ptr, arglist);
  Py_DECREF(arglist);
  if (result == NULL) { /* cannot return NULL... */
    printf("Error in callback..."); exit(1);
  }
  double C_result = PyFloat_AS_DOUBLE(result);
  Py_DECREF(result); 
  return C_result;
}

PyObject* Convert_MyArray::_pyfunc_ptr = NULL;
  
Fxy Convert_MyArray:: set_pyfunc (PyObject* f)
{
  _pyfunc_ptr = f;
  Py_INCREF(_pyfunc_ptr);
  return _pycall;
}

void Convert_MyArray:: dump(MyArray<double>& a)
{
  a.print_(std::cout);
}

