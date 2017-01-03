#include <math.h>
#include <stdio.h>               /* for debug output */
#include <PWOCallable.h>
#include <PWOSequence.h>
#include <PWONumber.h>

double f1p(double x, double y) { return x + y; } /* test func. */

// Note: keep the parts of the extension module that needs
// the Python or NumPy C API in a single file!
// Therefore we include the class .h and .cpp files here:
#include <NumPyArray.h>
#include <NumPyArray.cpp>

extern "C" {  // extern "C" is important since this is compiled under C++

static PyObject* gridloop1(PyObject* self, PyObject* args_)
{
  /* arguments: a, xcoor, ycoor */
  try {
    PWOSequence args (args_);
    NumPyArray_Float a ((PyArrayObject*) ((PyObject*) args[0])); 
    NumPyArray_Float xcoor ((PyArrayObject*) ((PyObject*) args[1])); 
    NumPyArray_Float ycoor ((PyArrayObject*) ((PyObject*) args[2])); 
    PWOCallable func1 (args[3]);

    if (!a.checktype()) { return NULL; } 
    if (!a.checkdim(2)) { return NULL; }
    int nx = a.size1(); int ny = a.size2();
    if (!xcoor.checktype()) { return NULL; }
    if (!xcoor.checkdim(1)) { return NULL; }
    if (!xcoor.checksize(nx)) { return NULL; }
    if (!ycoor.checktype()) { return NULL; }
    if (!ycoor.checkdim(1)) { return NULL; }
    if (!ycoor.checksize(ny)) { return NULL; }
    if (!func1.isCallable()) {
      PyErr_Format(PyExc_TypeError,
		   "func1 is not a callable function");
      return NULL;
    }
    int i,j;
    for (i = 0; i < nx; i++) {
      for (j = 0; j < ny; j++) {
	PWOTuple arglist(Py_BuildValue("(dd)", xcoor(i), ycoor(j)));
	PWONumber result(func1.call(arglist));
	a(i,j) = double(result);

	/* no efficiency improvement by explicit new/delete:
	PWOTuple* arglist = new PWOTuple(Py_BuildValue("(dd)", xcoor(i), ycoor(j)));
	PWONumber* result = new PWONumber(func1.call(*arglist));
	a(i,j) = double(*result);
	delete arglist; delete result;
	*/
	
#ifdef DEBUG
	printf("a[%d,%d] at (%g,%g)=%g\n",i,j,xcoor(i),ycoor(j),a(i,j));
#endif
      }
    }
    return PWONone();
  }
  catch (PWException e) { return e; }
}

static PyObject* gridloop2(PyObject* self, PyObject* args_)
{
  /* arguments: xcoor, ycoor, func1 */
  try {
    PWOSequence args (args_);
    NumPyArray_Float xcoor ((PyArrayObject*) ((PyObject*) args[0])); 
    NumPyArray_Float ycoor ((PyArrayObject*) ((PyObject*) args[1])); 
    PWOCallable func1 (args[2]);
    if (!xcoor.checktype()) { return NULL; }
    if (!xcoor.checkdim(1)) { return NULL; }
    if (!ycoor.checktype()) { return NULL; }
    if (!ycoor.checkdim(1)) { return NULL; }
    if (!func1.isCallable()) {
      PyErr_Format(PyExc_TypeError,
		   "func1 is not a callable function");
      return NULL;
    }
    int nx = xcoor.size1();  int ny = ycoor.size1();
    NumPyArray_Float a(nx, ny);  // return array

    int i,j;
    for (i = 0; i < nx; i++) {
      for (j = 0; j < ny; j++) {
	PWOTuple arglist(Py_BuildValue("(dd)", xcoor(i), ycoor(j)));
	PWONumber result(func1.call(arglist));
	a(i,j) = double(result);

#ifdef DEBUG
      printf("a[%d,%d]=f1p(%g,%g)=%g\n",i,j,xcoor(i),ycoor(j),a(i,j));
#endif
      }
    }
    return PyArray_Return(a.getPtr());
  }
  catch (PWException e) { return e; }
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

} // end extern "C"
