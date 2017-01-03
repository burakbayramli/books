#!/usr/bin/env python
from math import *

def Trapezoidal(a, b, f, n):
    h = (b-a)/float(n)
    s = 0
    x = a
    for i in range(1,n,1):
        x = x + h
        s = s + f(x)
    s = 0.5*(f(a) + f(b)) + s
    return h*s

def f1(x):
    f = exp(-x*x)*log(1+x*sin(x))
    return f

def f2(x):         # simple function for verification
    return 1 + x   # integral from 0 to 2 should be 4

a = 0; b = 2; n = 1000
for i in range(10000):
    result = Trapezoidal(a, b, f1, n)
import time  # measure time spent in the program
t1 = time.clock()  # CPU time so far in the program
print result, t1
