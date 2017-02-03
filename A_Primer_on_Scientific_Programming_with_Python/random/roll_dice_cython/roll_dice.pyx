# cython: profile=True
import  numpy as np
cimport numpy as np
import random

def roll_dice1(int N, int ndice, int nsix):
    cdef int M = 0            # no of successful events
    cdef int six, r
    cdef double p
    for i in range(N):
        six = 0               # how many dice with six eyes?
        for j in range(ndice):
            # Roll die no. j
            r = random.randint(1, 6)
            if r == 6:
               six += 1
        if six >= nsix:  # Successful event?
            M += 1
    p = float(M)/N
    return p

import cython
@cython.boundscheck(False)
def roll_dice2(int N, int ndice, int nsix):
    # Use numpy to generate all random numbers
    cdef int M = 0            # no of successful events
    cdef int six, r
    cdef double p
    cdef np.ndarray[np.int_t, ndim=2, negative_indices=False, 
                    mode='c'] eyes = np.random.random_integers(1, 6, (N, ndice))
    for i in range(N):
        six = 0               # how many dice with six eyes?
        for j in range(ndice):
            # Roll die no. j
            r = eyes[i,j]
            if r == 6:
               six += 1
        if six >= nsix:  # Successful event?
            M += 1
    p = float(M)/N
    return p


