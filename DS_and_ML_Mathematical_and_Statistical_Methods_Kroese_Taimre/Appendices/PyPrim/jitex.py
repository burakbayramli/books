""" jitex .py """
import timeit
import numpy as np
from numba import jit
n = 10**8

#@jit
def myfun (s,n):
    for i in range (1,n):
        s = s+ 1/i
    return s

start = timeit .time. clock ()
print ("Euler's constant is approximately {:9.8f}".format(
                    myfun (0,n) - np.log(n)))
end = timeit.time.clock ()
print ("elapsed time: {:3.2f} seconds ".format(end - start ))
