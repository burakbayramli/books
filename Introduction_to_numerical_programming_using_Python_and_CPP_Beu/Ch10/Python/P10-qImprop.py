# Evaluates improper integrals
from math import *
from integral import *

def Func1(x): return x * exp(-x*x)
def Func2(x): return sin(x)/x if x else 1e0
def Func3(x): return 1/sqrt(x)

# main

xinf = 1e10
eps = 1e-6
(s, xinf) = qImprop1(Func1,0e0,xinf,eps)
print("I1 = ",s," xinf = ",xinf)
(s, xinf) = qImprop1(Func2,0e0,xinf,eps)
print("I2 = ",s," xinf = ",xinf)
print("I3 = ",qImprop2(Func3,0e0,1e0,eps))
