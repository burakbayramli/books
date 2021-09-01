# Fresnel integrals and Cornu spiral
from math import *
from integral import *
from graphlib import *

def CosF(u): return cos(0.5*pi*u*u)         # integrands of Fresnel integrals
def SinF(u): return sin(0.5*pi*u*u)

# main

eps = 1e-6                                   # relative integration precision
xmin = -3.5; xmax = 3.5                            # interval of upper limits
h = 0.05;                                             # plotting mesh spacing
n = int((xmax-xmin)/h) + 1                           # number of upper limits

x = [0]*(2*n+1); c = [0]*(2*n+1); s = [0]*(n+1)

for i in range(1,n+1):
   x[i] = xmin + (i-1)*h; x[i+n] = x[i]                         # upper limit
   c[i] = qRomberg(CosF,0e0,x[i],eps)                     # Fresnel integrals
   s[i] = qRomberg(SinF,0e0,x[i],eps); c[i+n] = s[i]

GraphInit(1200,600)

nn = [0, n, 2*n]; col = ["", "red", "blue"]; sty = [0, 1,-1]
MultiPlot(x,c,c,nn,col,sty,2,10,
          0e0,0e0,0,0e0,0e0,0,0.10,0.45,0.15,0.85,
          "x","C, S","Fresnel integrals")

Plot(c,s,n,"green",1,0.60,0.95,0.15,0.85,"C(x)","S(x)","Cornu Spiral")

MainLoop()
