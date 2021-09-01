# Plot cylindrical Bessel functions
from math import *
from specfunc import *
from graphlib import *

def CBessJ(n, x):                # cylindrical Bessel function of order n+1/2
    return sqrt(2e0*x/pi) * SBessj(n,x)

def CBessN(n, x):               # cylindrical Neumann function of order n+1/2
    return sqrt(2e0*x/pi) * SBessy(n,x)

# main

GraphInit(1200,600)

xmin = 0e0; xmax = 50e0; h = 0.1e0
n = int((xmax-xmin)/h) + 1

x = [0]*(n+1)
y = [0]*(n+1)

for i in range(1,n+1):
  x[i] = xmin + i*h
  y[i] = CBessJ(5,x[i])

Plot(x,y,n,"blue",1,0.10,0.45,0.15,0.85,
     "x","J_11/2","Cylindrical Bessel function")

xmin = 5e0; xmax = 50e0; h = 0.1e0
n = int((xmax-xmin)/h) + 1

for i in range(1,n+1):
  x[i] = xmin + i*h
  y[i] = CBessN(5,x[i])

Plot(x,y,n,"red",1,0.60,0.95,0.15,0.85,
     "x","N_11/2","Cylindrical Neumann function")

MainLoop()
