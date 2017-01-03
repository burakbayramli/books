## example9_13
from numarray import ones
from lamRange import *
from inversePower3 import *

N = 10              
n = 100                
d = ones((n))*2.0
c = ones((n-1))*(-1.0)
r = lamRange(d,c,N)           # Bracket N smallest eigenvalues
s = (r[N-1] + r[N])/2.0       # Shift to midpoint of Nth bracket
lam,x = inversePower3(d,c,s)  # Inverse power method
print "Eigenvalue No.",N," =",lam
raw_input("\nPress return to exit")
