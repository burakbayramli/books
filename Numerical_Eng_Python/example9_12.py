## example9_12
from numarray import ones,Float64
from eigenvals3 import *

N = 3
n = 100
d = ones((n))*2.0
c = ones((n-1))*(-1.0)
lambdas = eigenvals3(d,c,N)
print lambdas
raw_input("\nPress return to exit")
