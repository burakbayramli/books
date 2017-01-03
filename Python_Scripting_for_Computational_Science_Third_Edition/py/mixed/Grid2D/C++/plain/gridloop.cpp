#include <math.h>
#include <stdio.h>               /* for debug output */

double f1p(double x, double y) { return x + y; } /* test func. */

// Note: keep the parts of the extension module that needs
// the Python or NumPy C API in a single file!
// Therefore we include the class .h and .cpp files here:
#include <NumPyArray.h>
#include <NumPyArray.cpp>

extern "C" {  // extern "C" is important since this is compiled under C++

static PyObject* gridloop1(PyObject* self, PyObject* args)
{
  PyArrayObject *a_, *xcoor_, *ycoor_;
  PyObject *func1, *arglist, *result;

  /* arguments: a, xcoor, ycoor, func1 */
  if (!PyArg_ParseTuple(args, "O!O!O!O:gridloop1",
			&PyArray_Type, &a_,
			&PyArray_Type, &xcoor_,
			&PyArray_Type, &ycoor_,
			&func1)) {
    return NULL; /* PyArg_ParseTuple has raised an exception */
  } 
  NumPyArray_Float a (a_); 
  if (!a.checktype()) { return NULL; } 
  if (!a.checkdim(2)) { return NULL; }
  int nx = a.size1(); int ny = a.size2();
  NumPyArray_Float xcoor (xcoor_); 
  if (!xcoor.checktype()) { return NULL; }
  if (!xcoor.checkdim(1)) { return NULL; }
  if (!xcoor.checksize(nx)) { return NULL; }
  NumPyArray_Float ycoor (ycoor_); 
  if (!ycoor.checktype()) { return NULL; }
  if (!ycoor.checkdim(1)) { return NULL; }
  if (!ycoor.checksize(ny)) { return NULL; }
  if (!PyCallable_Check(func1)) {
    PyErr_Format(PyExc_TypeError,
    "func1 is not a callable function");
    return NULL;
  }
  int i,j;
  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      arglist = Py_BuildValue("(dd)", xcoor(i), ycoor(j));
      result = PyEval_CallObject(func1, arglist);
      Py_DECREF(arglist);
      if (result == NULL) return NULL; /* exception in func1 */
      a(i,j) = PyFloat_AS_DOUBLE(result);
      Py_DECREF(result); 

#ifdef DEBUG
      printf("a[%d,%d]=func1(%g,%g)=%g\n",i,j,xcoor(i),ycoor(j),a(i,j));
#endif
    }
  }
  return Py_BuildValue("");  /* return None */
  /* alternative (return 0 for success): 
     return Py_BuildValue("i",0); */
}

static PyObject* gridloop2(PyObject* self, PyObject* args)
{
  PyArrayObject *xcoor_, *ycoor_;
  PyObject *func1, *arglist, *result;

  /* arguments: xcoor, ycoor, func1 */
  if (!PyArg_ParseTuple(args, "O!O!O:gridloop2",
			&PyArray_Type, &xcoor_,
			&PyArray_Type, &ycoor_,
			&func1)) {
    return NULL; /* PyArg_ParseTuple has raised an exception */
  } 
  NumPyArray_Float xcoor (xcoor_); int nx = xcoor.size1();
  if (!xcoor.checktype()) { return NULL; }
  if (!xcoor.checkdim(1)) { return NULL; }
  NumPyArray_Float ycoor (ycoor_); int ny = ycoor.size1();
  if (!ycoor.checktype()) { return NULL; }
  if (!ycoor.checkdim(1)) { return NULL; }
  if (!PyCallable_Check(func1)) {
    PyErr_Format(PyExc_TypeError,
    "func1 is not a callable function");
    return NULL;
  }
  NumPyArray_Float a(nx, ny);  // return array

  int i,j;
  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      arglist = Py_BuildValue("(dd)", xcoor(i), ycoor(j));
      result = PyEval_CallObject(func1, arglist);
      Py_DECREF(arglist);
      if (result == NULL) return NULL; /* exception in func1 */
      a(i,j) = PyFloat_AS_DOUBLE(result);
      Py_DECREF(result); 

#ifdef DEBUG
      printf("a[%d,%d]=func1(%g,%g)=%g\n",i,j,xcoor(i),ycoor(j),a(i,j));
#endif
    }
  }
  return PyArray_Return(a.getPtr());
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
  {NULL, NULL}     /* required ending of the method table */
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

} // end extern "C"

