# Plot polynomial and derivative
from elemfunc import *
from graphlib import *

# main

np = 5                                                 # degree of polynomial
a = [63e0/8e0, 0e0, -70e0/8e0, 0e0, 15e0/8e0, 0e0]             # coefficients
b = [0]*(np+1)                                         # coeffs of derivative

xmin = -1e0; xmax = 1e0                                     # plotting domain
h = 0.01e0                                                 # argument spacing
n = int((xmax-xmin)/h) + 1                                 # number of points

x = [0]*(n+1); y = [0]*(n+1); z = [0]*(n+1)                # arrays for plots

PolyDerive(a,b,np)                          # coefficients of derivative in b

for i in range(1,n+1):
  x[i] = xmin + (i-1)*h                                            # argument
  y[i] = Poly(x[i],a,np)                                         # polynomial
  z[i] = Poly(x[i],b,np-1)                                       # derivative

GraphInit(1200,600)

Plot(x,y,n,"blue",1,0.10,0.45,0.15,0.85,"x","P(x)","Polynomial")
Plot(x,z,n,"red" ,1,0.60,0.95,0.15,0.85,"x","P'(x)","Derivative")

MainLoop()
