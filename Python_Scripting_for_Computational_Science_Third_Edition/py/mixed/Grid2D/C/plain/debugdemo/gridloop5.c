/* NOTE: this gridloop3.c file contains errors!
   The purpose is to illustrate debugging.
   Error: we call a function mydebug in gridloop2, but mydebug is
   not implemented
*/
#include <Python.h>              /* Python as seen from C */
#include <numpy/arrayobject.h>   /* NumPy  as seen from C */
#include <math.h>
#include <stdio.h>               /* for debug output */

double f1p(double x, double y) { return x + y; } /* test func. */

static PyObject *gridloop1(PyObject *self, PyObject *args)
{
  PyArrayObject *a, *xcoor, *ycoor;
  PyObject *func1, *arglist, *result;
  int nx, ny, i, j;
  double *a_ij, *x_i, *y_j;

  /* arguments: a, xcoor, ycoor, func1 */
  if (!PyArg_ParseTuple(args, "O!O!O!O:gridloop1",
			&PyArray_Type, &a,
			&PyArray_Type, &xcoor,
			&PyArray_Type, &ycoor,
			&func1)) {
    return NULL; /* PyArg_ParseTuple has raised an exception */
  } 

  /* alternative parsing without checking the pointer types:
  if (!PyArg_ParseTuple(args, "OOOO", &a, &xcoor, &ycoor, &func1)) {
    return NULL; }
  */
  if (PyArray_NDIM(a) != 2 || PyArray_TYPE(a) != NPY_DOUBLE) {
    PyErr_Format(PyExc_ValueError,
    "a array is %d-dimensional or not of type 'Python float'", PyArray_NDIM(a));
    return NULL;
  }
  nx = PyArray_DIM(a,0);
  ny = PyArray_DIM(a,1);
  if (PyArray_NDIM(xcoor) != 1 || PyArray_TYPE(xcoor) != NPY_DOUBLE ||
      PyArray_DIM(xcoor,0) != nx) {
    PyErr_Format(PyExc_ValueError,
    "xcoor array has wrong dimension (%d), type or length (%d)",
		 PyArray_NDIM(xcoor),PyArray_DIM(xcoor,0));
    return NULL;
  }
  if (PyArray_NDIM(ycoor) != 1 || PyArray_TYPE(ycoor) != NPY_DOUBLE ||
      PyArray_DIM(ycoor,0) != ny) {
    PyErr_Format(PyExc_ValueError,
    "ycoor array has wrong dimension (%d), type or length (%d)",
		 PyArray_NDIM(ycoor),PyArray_DIM(ycoor,0));
    return NULL;
  }
  if (!PyCallable_Check(func1)) {
    PyErr_Format(PyExc_TypeError,
    "func1 is not a callable function");
    return NULL;
  }
  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      a_ij = (double *) PyArray_GETPTR2(a, i, j);
      x_i = (double *) PyArray_GETPTR1(xcoor, i);
      y_j = (double *) PyArray_GETPTR1(xcoor, i);
      arglist = Py_BuildValue("(dd)", *x_i, *y_j);
      result = PyEval_CallObject(func1, arglist);
      Py_DECREF(arglist);
      if (result == NULL) return NULL; /* exception in func1 */
      *a_ij = PyFloat_AS_DOUBLE(result);
      Py_DECREF(result); 

#ifdef DEBUG
      printf("a[%d,%d]=func1(%g,%g)=%g\n",i,j,*x_i,*y_j,*a_ij);
#endif
    }
  }
  return Py_BuildValue("");  /* return None */
  /* alternative (return 0 for success): 
     return Py_BuildValue("i",0); */
}

#include <NumPy_macros.h>

static PyObject *gridloop2(PyObject *self, PyObject *args)
{
  PyArrayObject *a, *xcoor, *ycoor;
  npy_intp a_dims[2];
  PyObject *func1, *arglist, *result;
  int nx, ny, i, j;

  /* arguments: xcoor, ycoor, func1 */
  if (!PyArg_ParseTuple(args, "O!O!O:gridloop2",
			&PyArray_Type, &xcoor,
			&PyArray_Type, &ycoor,
			&func1)) {
    return NULL; /* PyArg_ParseTuple has raised an exception */
  } 
  nx = PyArray_DIM(xcoor,0);  ny = PyArray_DIM(ycoor,0);
  NDIM_CHECK(xcoor, 1); TYPE_CHECK(xcoor, NPY_DOUBLE);
  NDIM_CHECK(ycoor, 1); TYPE_CHECK(ycoor, NPY_DOUBLE);
  CALLABLE_CHECK(func1);
  /* create return array: */
  a_dims[0] = nx; a_dims[1] = ny;
  a = (PyArrayObject *) PyArray_SimpleNew(2, a_dims, NPY_DOUBLE);
  if (a == NULL) { 
    printf("creating a failed, dims=(%d,%d)\n",
	   (int) a_dims[0], (int) a_dims[1]);
    return NULL; /* PyArray_SimpleNew raised an exception */ 
  }

  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      arglist = Py_BuildValue("(dd)", DIND1(xcoor,i), DIND1(ycoor,j));
      result = PyEval_CallObject(func1, arglist);
      Py_DECREF(arglist);
      if (result == NULL) return NULL; /* exception in func1 */
      DIND2(a,i,j) = PyFloat_AS_DOUBLE(result);
      Py_DECREF(result); 
      
      mydebug(DIND1(xcoor,i),DIND1(ycoor,j),DIND2(a,i,j));
    }
  }
  return PyArray_Return(a);
}

/* doc strings: */
static char gridloop1_doc[] = \
  "gridloop1(a, xcoor, ycoor, pyfunc)";
static char gridloop2_doc[] = \
  "a = gridloop2(xcoor, ycoor, pyfunc)";
static char module_doc[] = \
  "module ext_gridloop:\n\
   gridloop1(a, xcoor, ycoor, pyfunc)\n\
   a = gridloop2(xcoor, ycoor, pyfunc)";

/* 
   The method table must always be present - it lists the 
   functions that should be callable from Python: 
*/
static PyMethodDef ext_gridloop_methods[] = {
  {"gridloop1",    /* name of func when called from Python */
   gridloop1,      /* corresponding C function */
   METH_VARARGS,   /* ordinary (not keyword) arguments */
   gridloop1_doc}, /* doc string for gridloop1 function */
  {"gridloop2",    /* name of func when called from Python */
   gridloop2,      /* corresponding C function */
   METH_VARARGS,   /* ordinary (not keyword) arguments */
   gridloop2_doc}, /* doc string for gridloop1 function */
  {NULL, NULL}
};

PyMODINIT_FUNC initext_gridloop()
{
  /* Assign the name of the module and the name of the
     method table and (optionally) a module doc string:
  */
  Py_InitModule3("ext_gridloop", ext_gridloop_methods, module_doc);
  /* without module doc string: 
  Py_InitModule ("ext_gridloop", ext_gridloop_methods); */

  import_array();   /* required NumPy initialization */
}
