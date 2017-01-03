'''lor_C.pyx: cython code for speed up.  Integrates the Lorenz system.

derived from hmm/C.pyx

See http://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods

'''
#To build: python3 setup.py build_ext --inplace

# Imitate http://docs.cython.org/src/tutorial/numpy.html and
# http://docs.cython.org/src/userguide/memoryviews.html
Copyright = '''
Copyright 2013 Andrew M. Fraser and Los Alamos National Laboroatory

This file is part of hmmds3.

Hmmds3 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Hmmds3 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

See the file gpl.txt in the root directory of the hmmds3 distribution
or see <http://www.gnu.org/licenses/>.
'''
import numpy as np
cimport cython
cimport numpy as np
DTYPE = np.float64
ctypedef np.float64_t DTYPE_t

@cython.boundscheck(False)
cdef f_lor_c(DTYPE_t *x, double t, double s, double b, double r,
             DTYPE_t *y_dot):
    '''This function acts as if it were pure C.  It calculates the vector
    field (y_dot) at x,t for the Lorenz system with parmaters s, b, r.

    '''
    y_dot[0] = s * (x[1] - x[0])
    y_dot[1] = r * x[0] - x[0] * x[2] - x[1]
    y_dot[2] = x[0] * x[1] - b * x[2]
    return

@cython.boundscheck(False)
cdef lor_step(
    DTYPE_t *x_i, # Initial position
    DTYPE_t *x_f, # Storage for final position
    double h,     # Time step
    double t,     # Time (not used)
    double s, double b, double r, # Parameters
):
    ''' Implements a fourth order Runge Kutta step using f_lor_c() for
        the vector field.
        '''
    cdef DTYPE_t k[5][3] # Storage for intermediate results.

    f_lor_c(x_i,t,s,b,r,k[0])
    for i in range(3):
        k[1][i] = h*k[0][i]
        x_f[i] = x_i[i] + k[1][i]/2

    f_lor_c(x_f,t,s,b,r,k[0])
    for i in range(3):
        k[2][i] = h*k[0][i]
        x_f[i] = x_i[i] + k[2][i]/2

    f_lor_c(x_f,t,s,b,r,k[0])
    for i in range(3):
        k[3][i] = h*k[0][i]
        x_f[i] = x_i[i] + k[3][i]

    f_lor_c(x_f,t,s,b,r,k[0])
    for i in range(3):
        k[4][i] = h*k[0][i]
        x_f[i] = x_i[i] + (k[1][i] + 2 * k[2][i] + 2 * k[3][i] + k[4][i])/6
    return

@cython.boundscheck(False)
def f_lor(np.ndarray[DTYPE_t, ndim=1] x, double t, double s, double b,
          double r):
    ''' Evaluates dy/dt at x for the Lorenz system.  Called from python.
    '''
    rv = np.empty(3) # Dangerous if global but cuts time by 0.54
    cdef np.ndarray[DTYPE_t, ndim=1] _tmp = rv
    cdef DTYPE_t *y_dot = <DTYPE_t *>_tmp.data # C-pointer to return data
    cdef DTYPE_t *X = <DTYPE_t *>x.data        # C-pointer to inital data
    f_lor_c(X, t, s, b, r, y_dot)
    return rv

@cython.boundscheck(False)
def Lsteps(IC,      # IC[0:3] is the initial condition
           s, b, r, # These are the Lorenz parameters
           T_step,  # The time between returned samples
           N_steps  # N_steps The number of returned samples
           ):
    '''Generate a Lorenz time series.  Called from python.
    '''
    rv = np.empty((N_steps, 3))       # Storage for result
    cdef DTYPE_t [:, :] rv_v = rv     # view of rv
    rv[0,:] = IC

    cdef DTYPE_t scratch[2][3]
    cdef DTYPE_t [:, :] s_v = scratch # view of scratch
    cdef double t = 0
    cdef int i, j, N
    N = max(1,(T_step/0.005))
    cdef double h = T_step/N
    for i in range(1,N_steps):
        s_v[0] = rv_v[i-1]
        for j in range(N):
            lor_step(&s_v[j%2,0], &s_v[(j+1)%2,0], h, t, s, b, r)
        rv_v[i] = s_v[N%2]
    return rv

#--------------------------------
# Local Variables:
# mode: python
# End:
