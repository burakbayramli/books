#include <Python.h>              /* Python as seen from C */
#include <numpy/arrayobject.h>   /* NumPy  as seen from C */
#include <math.h>
#include <stdio.h>               /* for debug output */
#include <NumPy_macros.h>        /* useful macros */

static PyObject *modname_function1(PyObject *self, PyObject *args)
{
  PyArrayObject *array1, *array2;
  PyObject *callback, *arglist, *result;
  npy_intp array3_dims[2];
  <more local C variables...>

  /* assume arguments array, array2, callback */
  if (!PyArg_ParseTuple(args, "O!O!O:modname_function1",
			&PyArray_Type, &array1,
			&PyArray_Type, &array2,
			&callback)) {
    return NULL; /* PyArg_ParseTuple has raised an exception */
  } 

  <check array dimensions etc.>

  if (!PyCallable_Check(callback)) {
    PyErr_Format(PyExc_TypeError,
    "callback is not a callable function");
    return NULL;
  }
  /* Create output arrays: */
  array3_dims[0] = nx; array3_dims[1] = ny;
  array3 = (PyArrayObject *) \
           PyArray_SimpleNew(2, array3_dims, NPY_DOUBLE);
  if (array3 == NULL) { 
    printf("creating %dx%d array failed\n",
           (int) array3_dims[0], (int) array3_dims[1]);
    return NULL; /* PyArray_FromDims raises an exception */ 
  }

  /* Example on callback:

  arglist = Py_BuildValue(format, var1, var2, ...);
  result = PyEval_CallObject(callback, arglist);
  Py_DECREF(arglist);
  if (result == NULL) return NULL;
  <process result>
  Py_DECREF(result);
  */

  /* Example on array processing:
  for (i = 0; i <= imax; i++) {
    for (j = 0; j <= jmax; j++) {
      <work with DIND1(array2,i) if array2 is 1-dimensional>
      <or DIND2(array3,i,j) if array2 is 2-dimensional etc.>
      <or IIND1/2/3 for integer arrays>
    }
  }
  */
  return PyArray_Return(array3);
  /* or None: return Py_BuildValue(""); */
  /* or integer: return Py_BuildValue("i", some_int); */
}

static PyObject *modname_function2(PyObject *self, PyObject *args)
{ ... }

static PyObject *modname_function3(PyObject *self, PyObject *args)
{ ... }

/* Doc strings: */
static char modname_function1_doc[] = "...";
static char modname_function2_doc[] = "...";
static char modname_function3_doc[] = "...";
static char module_doc[] = "...";

/* Method table: */
static PyMethodDef modname_methods[] = {
  {"function1",        /* name of func when called from Python */
   modname_function1,  /* corresponding C function */
   METH_VARARGS,       /* positional (no keyword) arguments */
   modname_function1_doc}, /* doc string for function */
  {"function2",        /* name of func when called from Python */
   modname_function2,  /* corresponding C function */
   METH_VARARGS,       /* positional (no keyword) arguments */
   modname_function2_doc}, /* doc string for function */
  {"function3",        /* name of func when called from Python */
   modname_function3,  /* corresponding C function */
   METH_VARARGS,       /* positional (no keyword) arguments */
   modname_function3_doc}, /* doc string for function */
  {NULL, NULL}         /* required ending of the method table */
};

PyMODINIT_FUNC initmodname()
{
  Py_InitModule3("modname", modname_methods, module_doc);
  import_array();   /* required NumPy initialization */
}
