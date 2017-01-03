from __future__ import print_function, division
import numpy as np
from numba import autojit, jit, double, int32
import timeit, sys, os
sys.path.append(os.getcwd())
import cython_arma

def arma(parameters,data,p=0,q=0):
    T = data.size
    errors = np.zeros_like(data)
    for t in xrange(T):
        errors[t] = data[t] - parameters[0]
        for i in xrange(p):
            if (t-i) >= 0:
                errors[t] -= parameters[i+1] * data[t-i]
        for i in xrange(q):
            if (t-i) >= 0:
                errors[t] -= parameters[i+p+1] * errors[t-i]
    return errors


arma_autojit = autojit(arma)

arma_jit = jit(double[:](double[:],double[:],int32,int32))(arma)


parameters = np.zeros((3))
data = np.random.randn(10000)
p = 1
q = 1



if __name__=='__main__':
    arma(parameters,data,p,q)
    arma_autojit(parameters,data,p,q)
    arma_jit(parameters,data,p,q)
    cython_arma.arma(parameters,data,p,q)

    N = 50000


    setup='from performance_and_optimization_solutions import arma, arma_autojit, arma_jit;' + \
          'import cython_arma;' + \
          'import numpy as np;parameters = np.zeros((3));data = np.random.randn(10000);p = 1;q = 1'

    t1 = timeit.timeit('arma(parameters,data,p,q)', setup=setup, number=N//100)

    t2 = timeit.timeit('arma_autojit(parameters,data,p,q)', setup=setup, number=N)

    t3 = timeit.timeit('arma_jit(parameters,data,p,q)', setup=setup, number=N)

    t4 = timeit.timeit('cython_arma.arma(parameters,data,p,q)', setup=setup, number=N)

    times = np.array([t1/(N/100),t2/N,t3/N,t4/N])
    print('Performance relative to pure Python:')
    print('Autojit: {0:.2f}'.format(times[0]/times[1]-1))
    print('Jit:     {0:.2f}'.format(times[0]/times[2]-1))
    print('Cython:  {0:.2f}'.format(times[0]/times[3]-1))
