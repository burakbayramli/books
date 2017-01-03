"""
Created on Wed Mar 21 14:20:59 2012

@author: kevin.sheppard
"""

import numpy as np
cimport numpy as np
cimport cython


@cython.boundscheck(False)
@cython.wraparound(False)
def arma(np.ndarray[np.float64_t, ndim=1] parameters,
         np.ndarray[np.float64_t, ndim=1] data,
         int p=0,
         int q=0):

    cdef size_t T, t, i
    T = data.size
    cdef np.ndarray[np.float64_t, ndim=1] errors = np.zeros(T)
    for t in xrange(T):
        errors[t] = data[t] - parameters[0]
        for i in xrange(p):
            if (t-i) >= 0:
                errors[t] -= parameters[i+1] * data[t-i]
        for i in xrange(q):
            if (t-i) >= 0:
                errors[t] -= parameters[i+p+1] * errors[t-i]

    return errors
