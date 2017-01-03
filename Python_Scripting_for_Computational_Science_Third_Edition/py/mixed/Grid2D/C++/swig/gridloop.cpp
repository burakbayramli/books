#include <NumPyArray.h>

void gridloop1(PyArrayObject* a_, PyArrayObject* xcoor_,
	       PyArrayObject* ycoor_, PyObject* func1)
{
  PyObject *arglist, *result;
  NumPyArray_Float a (a_); 
  int nx = a.size1(); int ny = a.size2();
  NumPyArray_Float xcoor (xcoor_); 
  NumPyArray_Float ycoor (ycoor_); 
  int i,j;
  for (i = 0; i < nx; i++) {
    for (j = 0; j < ny; j++) {
      arglist = Py_BuildValue("(dd)", xcoor(i), ycoor(j));
      result = PyEval_CallObject(func1, arglist);
      Py_DECREF(arglist);
      if (result == NULL) return; /* exception in func1 */
      a(i,j) = PyFloat_AS_DOUBLE(result);
      Py_DECREF(result); 

#ifdef DEBUG
      printf("a[%d,%d]=func1(%g,%g)=%g\n",i,j,xcoor(i),ycoor(j),a(i,j));
#endif
    }
  }
}

PyArrayObject* gridloop2(PyArrayObject* xcoor_,
			 PyArrayObject* ycoor_, PyObject* func1)
{
  PyObject *arglist, *result;
  NumPyArray_Float xcoor (xcoor_); int nx = xcoor.size1();
  NumPyArray_Float ycoor (ycoor_); int ny = ycoor.size1();
  NumPyArray_Float a(nx, ny);  // return array
  /* NOTE: got a segmentation fault in the above statement :-( */

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
  return (PyArrayObject*) PyArray_Return(a.getPtr());
}

