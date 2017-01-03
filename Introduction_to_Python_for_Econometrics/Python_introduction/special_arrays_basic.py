from __future__ import division
from __future__ import print_function
import sys
from pylab import *
from numpy import *
# End Imports


M, N = 5, 5
x = ones((M,N)) # M by N array of 1s
x =  ones((M,M,N)) # 3D array
x =  ones((M,N), dtype='int32') # 32-bit integers

x = zeros((M,N)) # M by N array of 0s
x = zeros((M,M,N)) # 3D array of 0s
x = zeros((M,N),dtype='int64') # 64 bit integers

x = empty((M,N)) # M by N empty array
x = empty((N,N,N,N)) # 4D empty array
x = empty((M,N),dtype='float32') # 32-bit floats (single precision)

In = eye(N)

