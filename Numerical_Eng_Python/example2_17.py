## example2_17
from numarray import zeros,Float64
from gaussSeidel import *

def iterEqs(x,omega):
    n = len(x)
    x[0] =omega*(x[1] - x[n-1])/2.0 + (1.0 - omega)*x[0]
    for i in range(1,n-1):
        x[i] = omega*(x[i-1] + x[i+1])/2.0 + (1.0 - omega)*x[i]
    x[n-1] = omega*(1.0 - x[0] + x[n-2])/2.0 \
           + (1.0 - omega)*x[n-1]
    return x
    
n = eval(raw_input("Number of equations ==> "))
x = zeros((n),type=Float64)
x,numIter,omega = gaussSeidel(iterEqs,x)
print "\nNumber of iterations =",numIter
print "\nRelaxation factor =",omega
print "\nThe solution is:\n",x
raw_input("\nPress return to exit")
    
