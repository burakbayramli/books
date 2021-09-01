# Evaluates an integral using adaptive classical quadratures
from math import *
from integral import *

def Func(x): return (x*x*x) * exp(-x)

# main

a = 0e0; b = 1e0; eps = 1e-10

print("I TrapzCtrl   = ",qTrapzCtrl(Func,a,b,eps))
print("I SimpsonCtrl = ",qSimpsonCtrl(Func,a,b,eps))
print("I Romberg     = ",qRomberg(Func,a,b,eps))
