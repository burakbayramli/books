#include <Python.h>
#include <math.h>
#ifdef HAVE_GSL
#include <gsl/gsl_cdf.h>
#include <gsl/gsl_errno.h>
#endif

/* module functions */
static PyObject *
lgh(PyObject *self, PyObject *args)
{

  double alpha;
  PyObject *counts;
  
  if (!PyArg_ParseTuple(args, "Od:lgh",
			&counts,
			&alpha))
    return NULL;

  if (!PyList_Check(counts)) {
    PyErr_SetString(PyExc_ValueError, "counts must be a list");
    return NULL;
  }

  int len = PyList_Size(counts);
  double lgh = -len * lgamma(alpha);

  int i;
  for (i=0; i < len; i++) {
    lgh += lgamma(PyInt_AsLong(PyList_GET_ITEM(counts,i)) + alpha); 
  }
  
   return(Py_BuildValue("d",lgh));

}


static PyObject *
lgamma_diff(PyObject *self, PyObject *args)
{
  float nijk, aijk;

  if (!PyArg_ParseTuple(args, "ff", &nijk, &aijk))
    return NULL;

  return Py_BuildValue("f",
		       ((nijk > 0) 
			? lgammaf(nijk+aijk) - lgammaf(aijk) 
			: 0.0));
}



/************************************************************
Compute marginal log-likelihood from a 'CPT' of counts
and a corresponding 'CPT' of Dirichlet parameters.

'interval': For any given joint instantiation of the parents,
counts for child values are 'interval' elements apart in 'data'
'ri': the number of child values
'data': Data counts as a flat list
'prior': List of Dirichlet parameters, prior[i] is the Dirichlet
parameter corresponding to data[i]

Example:
If X2 is the child in this factor:
X1,X2,X3,value
--------------
0,0,0,data[0]
0,0,1,data[1]
0,0,2,data[2]
0,1,0,data[3]
0,1,1,data[4]
0,1,2,data[5]
1,0,0,data[6]
1,0,1,data[7]
1,0,2,data[8]
1,1,0,data[9]
1,1,1,data[10]
1,1,2,data[11]

then 
'interval' is 3
'ri' is 2
************************************************************/
static PyObject *
family_llh(PyObject *self, PyObject *args)
{
  
  int interval, ri;
  PyObject *data, *prior;
  
  if (!PyArg_ParseTuple(args, "iiOO:family_llh", 
			&interval, 
			&ri,
			&data, 
			&prior))
    return NULL;

  if (!PyList_Check(data) || !PyList_Check(prior)) {
    PyErr_SetString(PyExc_ValueError, "data and prior must both be lists");
    return NULL;
  }

  int len = PyList_Size(data);

  if (PyList_Size(prior) != len) {
    PyErr_SetString(PyExc_ValueError, "prior list must be the same size as data list");
    return NULL;
  }
  
  int start, j, k, step;
  double nij, aij, nijk, aijk;
  double llh = 0.0;


  step = interval * ri;
  /* if 'j' is the index of a data element which
     is the first for some parent configuration,
     then 'j' + 'step' will also be an index for 
     for the first data point of some other parent 
     configuration (as long as 'j' + 'step' < 'len')
  */
     
  /* Continuing example above, 'step' is 6, so
     'start' will be 0,6
     'j' will take  0,1,2,6,7,8
  */
  for (start = 0; start < len; start += step) {
    for (j = start; j < start + interval; j++) {
      nij = aij = 0.0;
      for (k = j; k < j + step; k += interval) {       /* ri iterations */
	nijk = PyFloat_AsDouble(PyList_GET_ITEM(data,k)); 
	aijk = PyFloat_AsDouble(PyList_GET_ITEM(prior,k));
	aij += aijk;
	if (nijk != 0.0) {
	  llh += (lgamma(nijk+aijk) - lgamma(aijk));
	  nij += nijk;
	}
      }
      if (nij != 0.0) 
	llh += (lgamma(aij) - lgamma(nij+aij));
    }
  }
    return(Py_BuildValue("d",llh));
}


static PyObject *
family_bdeu(PyObject *self, PyObject *args)
{
  
  int interval, ri;
  PyObject *data;
  double precision;
  
  if (!PyArg_ParseTuple(args, "iiOd:family_bdeu", 
			&interval, 
			&ri,
			&data, 
			&precision))
    return NULL;

  if (!PyList_Check(data)) {
    PyErr_SetString(PyExc_ValueError, "data must be a list");
    return NULL;
  }

  int len = PyList_Size(data);

  int start, j, k, step;
  double nij, nijk;
  double aijk = precision / len;
  double lgamma_aijk = lgamma(aijk);
  double aij = aijk * ri;
  double lgamma_aij = lgamma(aij);
  double llh = 0.0;

  

  step = interval * ri;
  /* if 'j' is the index of a data element which
     is the first for some parent configuration,
     then 'j' + 'step' will also be an index for 
     for the first data point of some other parent 
     configuration (as long as 'j' + 'step' < 'len')
  */
     
  /* Continuing example above, 'step' is 6, so
     'start' will be 0,6
     'j' will take  0,1,2,6,7,8
  */
  for (start = 0; start < len; start += step) {
    for (j = start; j < start + interval; j++) {
      nij = 0.0;
      for (k = j; k < j + step; k += interval) {       /* ri iterations */
	nijk = PyFloat_AsDouble(PyList_GET_ITEM(data,k)); 
	if (nijk != 0.0) {
	  llh += (lgamma(nijk+aijk) - lgamma_aijk);
	  nij += nijk;
	}
      }
      if (nij != 0.0) 
	llh += (lgamma_aij - lgamma(nij+aij));
    }
  }
    return(Py_BuildValue("d",llh));
}

/* An exception that is raised whenever GSL raises an exception or if GSL is
 * missing
 */
static PyObject *py_gsl_exception;

#ifdef HAVE_GSL
static void
gsl_handler(const char *reason, const char *file, int line, int gsl_errno)
{ 
  PyErr_SetString(py_gsl_exception,reason);
}

static PyObject *
chisqprob(PyObject *self, PyObject *args)
{ 
  double chi2, k, p;

  if (!PyArg_ParseTuple(args,"dd", &chi2, &k))
    return NULL;

  p = gsl_cdf_chisq_P(chi2, k);
  if (PyErr_Occurred())
    return NULL;
       
  return Py_BuildValue("d", p);
}

#else
static PyObject *
chisqprob(PyObject *self, PyObject *args)
{ 
  PyErr_SetString(py_gsl_exception,"gPy was not compiled against the "
      "GNU Scientific Library hence this functionality is missing.");
  return NULL;
}
#endif

/* registration table */
static PyMethodDef gPyC_methods[] = {
  {"lgh",  lgh, METH_VARARGS,
   "Return log H.\n\nCalled as C{lgh(counts,alpha)}\n@param counts: list of counts\n@type counts: List\n@param alpha: Prior precision divided by size of table\n@type alpha: Float\n@return: lgh function\n@rtype: Float."},
  {"lgammadiff",  lgamma_diff, METH_VARARGS,
   "Return log gamma differences.\n\nCalled as C{lgammadiff(x,y)}\n@param x: Some number\n@type x: Float/int\n@param y: Some number\n@type y: Float/int\n@return: C{lgamma(x+y) - lgamma(y)}\n@rtype: Float."},
  {"family_llh",  family_llh, METH_VARARGS,
   "Return the family component of the marginal log-likelihood for a BN family from counts and prior parameters.\n\nCalled as C{family_llh(interval,ri,data,prior)}\n@param interval: The distance between successive child values in C{data}\n@type interval: int\n@param ri: The number of values for the child variable of the CPT\n@type ri: int\n@param data: The data for the CPT (in the standard ordering)\n@type data: List of ints/floats\n@param prior: The Dirichlet parameters. C{prior[i]} is the Dirichlet parameter for C{data[i]}\n@type prior: List of ints/floats\n@return: The family component of the marginal log-likelihood\n@rtype: Float"},
  {"family_bdeu",  family_bdeu, METH_VARARGS,
   "Return the family component of the BDeu score for a BN family from counts and prior precision.\n\nCalled as C{family_bdeu(interval,ri,data,precision)}\n@param interval: The distance between successive child values in C{data}\n@type interval: int\n@param ri: The number of values for the child variable of the CPT\n@type ri: int\n@param data: The data for the CPT (in the standard ordering)\n@type data: List of ints/floats\n@param precision: The prior precision.\n@type precision: Int/Float\n@return: The family component of the BDeu score\n@rtype: Float"},
  {"chisqprob", chisqprob, METH_VARARGS,
   "Return the cumulative probability of a statistic according to the Chi-squared distribution.\n\n"
#ifndef HAVE_GSL
   "Note: The GNU Scientific Library support was not enabled for this library and so"
   "This function will always fail.\n\n"
#endif
   "@param chi2: the test statistic\n"
   "@type chi2: double\n"
   "@param dof: the degrees of freedom of the Chi-squared distribution\n"
   "@type dof: double\n"
   "@return: The cumulative probability (a double between zero and one, inclusive).\n"
   "Requires gPy to be comgsl_cdf_chisq_P(chi2, k)" },
  {NULL, NULL, 0, NULL}        /* Sentinel */
};

/* module intialiser */
PyMODINIT_FUNC
initgPyC(void)
{
  PyObject *self = Py_InitModule3("gPyC", gPyC_methods,"Functions implemented in C");
  if (!self)
    return;

  py_gsl_exception = PyErr_NewException("gPyC.gsl_exception", NULL, NULL);
  Py_INCREF(py_gsl_exception);
  PyModule_AddObject(self, "gsl_exception", py_gsl_exception);
#ifdef HAVE_GSL
  gsl_set_error_handler(gsl_handler);
#endif
}

