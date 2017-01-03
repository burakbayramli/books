#ifndef NUMPY_MACROS__H
#define NUMPY_MACROS__H

/* This file defines some macros for programming with
   NumPy arrays in C extension modules. */

/* define some macros for array dimension/type check
   and subscripting */
#define QUOTE(s) # s   /* turn s into string "s" */
#define NDIM_CHECK(a, expected_ndim) \
  if (PyArray_NDIM(a) != expected_ndim) { \
    PyErr_Format(PyExc_ValueError, \
    "%s array is %d-dimensional, but expected to be %d-dimensional",\
		 QUOTE(a), PyArray_NDIM(a), expected_ndim); \
    return NULL; \
  }
#define DIM_CHECK(a, dim, expected_length) \
  if (dim > PyArray_NDIM(a)) { \
    PyErr_Format(PyExc_ValueError, \
    "%s array has no %d dimension (max dim. is %d)", \
		 QUOTE(a), dim, PyArray_NDIM(a)); \
    return NULL; \
  } \
  if (PyArray_DIM(a, dim) != expected_length) { \
    PyErr_Format(PyExc_ValueError, \
    "%s array has wrong %d-dimension=%d (expected %d)", \
		 QUOTE(a), dim, PyArray_DIM(a, dim), expected_length); \
    return NULL; \
  }
#define TYPE_CHECK(a, tp) \
  if (PyArray_TYPE(a) != tp) { \
    PyErr_Format(PyExc_TypeError, \
    "%s array is not of correct type (%d)", QUOTE(a), tp); \
    return NULL; \
  }
#define CALLABLE_CHECK(func) \
  if (!PyCallable_Check(func)) { \
    PyErr_Format(PyExc_TypeError, \
    "%s is not a callable function", QUOTE(func)); \
    return NULL; \
  }

#define DIND1(a, i) *((double *) PyArray_GETPTR1(a, i))
#define DIND2(a, i, j) \
 *((double *) PyArray_GETPTR2(a, i, j))
#define DIND3(a, i, j, k) \
 *((double *) Py_Array_GETPTR3(a, i, j, k))

#define IIND1(a, i) *((int *) PyArray_GETPTR1(a, i))
#define IIND2(a, i, j) \
 *((int *) PyArray_GETPTR2(a, i, j))
#define IIND3(a, i, j, k) \
 *((int *) Py_Array_GETPTR3(a, i, j, k))

#endif
